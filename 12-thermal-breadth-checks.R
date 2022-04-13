#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Assess thermal breadth of occurrence and background points
#######################################################

source("E:/Documents/GitHub/mosquito-sdm/0-config.R")


#------------------------------------------------------
## DATA LOAD-IN ##
#------------------------------------------------------
#------------------------------------------------------
# Load in occurrence data
#------------------------------------------------------
Mosquitoes_SpeciesOfInterest <- read.csv("GBIF_Datasets_Cleaned/Mosquitoes_SpeciesOfInterest.csv", header = TRUE,
                                         encoding = "UTF-8", stringsAsFactors = FALSE)


#------------------------------------------------------
# Load in environmental predictors and stack by activity season
#------------------------------------------------------
predictors_preStack <- alply(list.files("Environmental Predictors Merged",
                                        pattern = ".tif",
                                        full.names = TRUE), 1, function(file){
                                          print(file)
                                          rast <- raster(file)
                                          return(rast)
                                        })
rasterNames <- c("CD","EVIM","EVISD","FC","HPD","PDQ","PhotoASTM","PhotoASTSD","PrecipASTM","PrecipASTSD","PWQ","SW","TAM","TASD","WS")
predictors_preStack %<>% setNames(rasterNames)

predictors_yearRound <- predictors_preStack[c(1:6,11:12,15,13:14)] %>% stack()
predictors_photoSeason <- predictors_preStack[c(1:6,11:12,15,7:8)] %>% stack()
predictors_precipSeason <- predictors_preStack[c(1:6,11:12,15,9:10)] %>% stack()


#------------------------------------------------------
# Load in landmass mask
#------------------------------------------------------
landmass_mask <- raster("Filter Masks/Landmass/Landmass.tif") %>%
  setNames(c("Landmass"))


#------------------------------------------------------
# Load in activity season length masks
#------------------------------------------------------
activity_lengths <- alply(list.files("Filter Masks/Activity Season Lengths",
                                     pattern = ".tif",
                                     full.names = TRUE), 1, function(file){
                                       print(file)
                                       rast <- raster(file)
                                       return(rast)
                                     }) %>%
  setNames(c("Photoperiod","Precipitation"))
activity_lengths_index <- c(NA,1,2,NA,NA,1,NA,1)


#------------------------------------------------------
# Load in predictor sum non-NA masks
#------------------------------------------------------
predictor_sum_mask <- alply(list.files("Filter Masks/Predictor Sum Non-NA",
                                       pattern = ".tif",
                                       full.names = TRUE), 1, function(file){
                                         print(file)
                                         rast <- raster(file)
                                         return(rast)
                                       }) %>%
  setNames(c("Year-Round","Photoseason","Precipitation"))
predictor_sum_index <- c(1,2,3,1,1,2,1,2)


#------------------------------------------------------
# Load in background bias masks
#------------------------------------------------------
bias_masks <- alply(list.files("Background Bias Masks",
                               pattern = ".RDS",
                               full.names = TRUE), 1, function(file){
                                 print(file)
                                 df <- readRDS(file)
                                 return(df)
                               }) %>%
  setNames(c("An_Gambiae","An_Stephensi","Cx_Annuli","Main"))
bias_masks_index <- c(4,4,1,2,3,4,4,4)


#------------------------------------------------------
# Load in ecoregion shapefiles
#------------------------------------------------------
ecoregions <- alply(list.files("Ecoregion_Outputs/Shapefiles",
                         pattern = ".RDS",
                         full.names = TRUE), 1, function(file){
                           print(file)
                           shapefile <- readRDS(file)
                           return(shapefile)
                           }) %>%
  setNames(c("Ae_Aegypti","Ae_Albopictus","An_Gambiae","An_Stephensi",
             "Cx_Annuli","Cx_Pipiens","Cx_Quinque","Cx_Tarsalis"))
  


#------------------------------------------------------
## STEP 1: PREPARE OCC SF OBJECTS AND BG MASK BY SPECIES ##
#------------------------------------------------------
occ_sf_list <- c()
bg_mask_list <- c()

for(i in 1:length(SpeciesOfInterest_Names)) {
  print(paste0("-- Step 1: Preparing occ sf objects and bg mask for ", SpeciesOfInterest_Names[[i]]," --"))
  
  #------------------------------------------------------
  ## OCCURRENCES ##
  #------------------------------------------------------
  #------------------------------------------------------
  # Acquire occurrences for species of interest
  #------------------------------------------------------
  occ_points <- Mosquitoes_SpeciesOfInterest %>%
    filter(species == SpeciesOfInterest_Names[[i]]) %>%
    dplyr::select(c(decimalLongitude, decimalLatitude))
  
  
  #------------------------------------------------------
  # Sieve 1: landmass
  #------------------------------------------------------
  landmass_values <- raster::extract(landmass_mask, occ_points)
  occ_landmass <- occ_points[which(!is.na(landmass_values)),]
  
  
  #------------------------------------------------------
  # Sieve 2: activity season
  #------------------------------------------------------
  if(SpeciesOfInterest_Names[[i]] %in% c("Aedes albopictus",
                                         "Anopheles gambiae",
                                         "Culex pipiens",
                                         "Culex tarsalis")) {
    occ_activity_length <- raster::extract(activity_lengths[[(activity_lengths_index[[i]])]], occ_landmass)
    occ_activity <- occ_landmass[which(occ_activity_length > 0),]
  } else {
    occ_activity <- occ_landmass
  }
  
  
  #------------------------------------------------------
  # Sieve 3: predictor sum non-NA
  #------------------------------------------------------
  occ_predictor_sum_vals <- raster::extract(predictor_sum_mask[[(predictor_sum_index[[i]])]], occ_activity)
  occ_predictor_sum <- occ_activity[which(!is.na(occ_predictor_sum_vals)),]
  
  
  #------------------------------------------------------
  # Sieve 4: unique cell centroids
  #------------------------------------------------------
  rast <- predictors_preStack[[1]] # Choose any generic raster to acquire cells and centroids from
  
  occ_cell_centroids <- cellFromXY(rast, occ_predictor_sum) %>% as.data.frame() %>%
    setNames("cell") %>% unique() %>%
    mutate(longitude = xFromCell(rast, cell),  # Acquire longitude (x) and latitude (y) from cell centroids
           latitude = yFromCell(rast, cell)) %>%
    dplyr::select(-cell) %>% # Cell number is now obsolete if working from (x,y) as an sf object
    filter(!is.na(longitude) & !is.na(latitude)) # Remove the NA locations
  
  occ_sf <- st_as_sf(occ_cell_centroids, coords = c("longitude","latitude"),
                     crs = 4326, agr = "constant")
  
  
  #------------------------------------------------------
  # Append filtered occ to list
  #------------------------------------------------------
  occ_sf_list %<>% c(list(occ_sf))
  
  
  
  #------------------------------------------------------
  ## BACKGROUND MASK ##
  #------------------------------------------------------
  #------------------------------------------------------
  # Sieve 1: landmass
  #------------------------------------------------------
  bg_mask <- bias_masks[[(bias_masks_index[[i]])]]
  sf::sf_use_s2(FALSE)
  bg_sf <- st_as_sf(bg_mask, coords = c("longitude","latitude"),
                    crs = 4326, agr = "constant")
  
  landmass_values <- raster::extract(landmass_mask, bg_sf)
  bg_landmass <- bg_sf[which(!is.na(landmass_values)),]
  
  
  #------------------------------------------------------
  # Sieve 2: activity season
  #------------------------------------------------------
  if(SpeciesOfInterest_Names[[i]] %in% c("Aedes albopictus",
                                         "Anopheles gambiae",
                                         "Culex pipiens",
                                         "Culex tarsalis")) {
    bg_activity_length <- raster::extract(activity_lengths[[(activity_lengths_index[[i]])]], bg_landmass)
    bg_activity <- bg_landmass[which(bg_activity_length > 0),]
  } else {
    bg_activity <- bg_landmass
  }
  
  
  #------------------------------------------------------
  # Sieve 3: predictor sum non-NA
  #------------------------------------------------------
  bg_predictor_sum_vals <- raster::extract(predictor_sum_mask[[(predictor_sum_index[[i]])]], bg_activity)
  bg_predictor_sum <- bg_activity[which(!is.na(bg_predictor_sum_vals)),]
  
  
  #------------------------------------------------------
  # Sieve 4: ecoregion
  #------------------------------------------------------
  bg_eco_intersect <- st_intersects(bg_predictor_sum, ecoregions[[i]])
  bg_eco_inds <- purrr::map_dbl(bg_eco_intersect, function(x) length(x)) %>% 
    magrittr::is_greater_than(0) %>% which()
  bg_eco_sf <- bg_predictor_sum[bg_eco_inds,]
  
  
  #------------------------------------------------------
  # Append filtered bg mask to list
  #------------------------------------------------------
  bg_mask_list %<>% c(list(bg_eco_sf))
  
}



#------------------------------------------------------
## STEP 2: EXTRACT TEMPERATURE VALUES BY SPECIES ##
#------------------------------------------------------
occ_temp_mean_list <- c()
bg_temp_mean_list <- c()
occ_temp_sd_list <- c()
bg_temp_sd_list <- c()

for(i in 1:length(SpeciesOfInterest_Names)) {
  set.seed(seedNum)
  print(paste0("-- Step 2: Extracting temperature values for ", SpeciesOfInterest_Names[[i]]," --"))
  
  #------------------------------------------------------
  # Set predictor stack according to specific activity season setting
  #------------------------------------------------------
  if(SpeciesOfInterest_Names[[i]] == "Aedes aegypti" |
     SpeciesOfInterest_Names[[i]] == "Anopheles stephensi" |
     SpeciesOfInterest_Names[[i]] == "Culex annulirostris" |
     SpeciesOfInterest_Names[[i]] == "Culex quinquefasciatus") {
    predictors <- predictors_yearRound }
  
  if(SpeciesOfInterest_Names[[i]] == "Aedes albopictus" |
     SpeciesOfInterest_Names[[i]] == "Culex pipiens" |
     SpeciesOfInterest_Names[[i]] == "Culex tarsalis") {
    predictors <- predictors_photoSeason }
  
  if(SpeciesOfInterest_Names[[i]] == "Anopheles gambiae") {
    predictors <- predictors_precipSeason }
  
  
  #------------------------------------------------------
  # Select occ and random sample bg without replacement from weighted bias mask at (2x occ) multiplier
  #------------------------------------------------------
  set.seed(seedNum)
  occ <- occ_sf_list[[i]]
  multiplier <- 2
  
  bg_df <- bg_mask_list[[i]] %>%
    mutate(weight = count/sum(count))
  bg <- bg_df[sample(nrow(bg_df),
                     size = multiplier * nrow(occ),
                     replace = FALSE,
                     prob = bg_df$weight),]
  
  
  #------------------------------------------------------
  # Extract temperature mean and SD values for occ and bg
  #------------------------------------------------------
  occ_temp_mean <- data.frame(raster::extract(predictors[[10]], occ)) %>%
    cbind(st_coordinates(occ) %>% as.data.frame()) %>%
    setNames(c("temp_mean","longitude","latitude"))
  bg_temp_mean <- data.frame(raster::extract(predictors[[10]], bg)) %>%
    cbind(st_coordinates(bg) %>% as.data.frame()) %>%
    setNames(c("temp_mean","longitude","latitude"))
  
  occ_temp_sd <- data.frame(raster::extract(predictors[[11]], occ)) %>%
    cbind(st_coordinates(occ) %>% as.data.frame()) %>%
    setNames(c("temp_sd","longitude","latitude"))
  bg_temp_sd <- data.frame(raster::extract(predictors[[11]], bg)) %>%
    cbind(st_coordinates(bg) %>% as.data.frame()) %>%
    setNames(c("temp_sd","longitude","latitude"))
  
  
  #------------------------------------------------------
  # Save extracted temperature mean and SD dataframes
  #------------------------------------------------------
  occ_temp_mean_list %<>% c(list(occ_temp_mean))
  bg_temp_mean_list %<>% c(list(bg_temp_mean))
  occ_temp_sd_list %<>% c(list(occ_temp_sd))
  bg_temp_sd_list %<>% c(list(bg_temp_sd))
  
}



#------------------------------------------------------
## THERMAL BREADTH FIGURES ##
#------------------------------------------------------
for(i in 1:length(SpeciesOfInterest_Names)) {
  occ_temp_mean <- occ_temp_mean_list[[i]] %>% mutate(set = "Occurrence")
  bg_temp_mean <- bg_temp_mean_list[[i]] %>% mutate(set = "Background")
  temp_mean_df <- rbind(occ_temp_mean, bg_temp_mean)
  
  occ_temp_sd <- occ_temp_sd_list[[i]] %>% mutate(set = "Occurrence")
  bg_temp_sd <- bg_temp_sd_list[[i]] %>% mutate(set = "Background")
  temp_sd_df <- rbind(occ_temp_sd, bg_temp_sd)
  
  
  #------------------------------------------------------
  # Histograms
  #------------------------------------------------------
  gg_hist_mean <- ggplot(temp_mean_df, aes(x = temp_mean, fill = set)) +
    geom_histogram(alpha = 0.6, position = "identity") +
    xlab("Temperature Mean (°C)") +
    ylab("Count") +
    theme_bw() +
    ggtitle(paste0(SpeciesOfInterest_Names[[i]],": Thermal Breadth (Mean)")) +
    labs(fill = "Set")
  
  gg_hist_sd <- ggplot(temp_sd_df, aes(x = temp_sd, fill = set)) +
    geom_histogram(alpha = 0.6, position = "identity") +
    xlab("Temperature Standard Deviation (°C)") +
    ylab("Count") +
    theme_bw() +
    ggtitle(paste0(SpeciesOfInterest_Names[[i]],": Thermal Breadth (SD)")) +
    labs(fill = "Set")
  
  
  #------------------------------------------------------
  # Boxplots
  #------------------------------------------------------
  gg_boxplot_mean <- ggplot(temp_mean_df, aes(x = set, y = temp_mean, fill = set)) +
    geom_boxplot(alpha = 0.7) +
    xlab("Set") +
    ylab("Temperature Mean (°C)") +
    ggtitle(paste0(SpeciesOfInterest_Names[[i]],": Thermal Breadth (Mean)")) +
    labs(fill = "Set") +
    theme_bw() +
    coord_flip()
  
  gg_boxplot_sd <- ggplot(temp_sd_df, aes(x = set, y = temp_sd, fill = set)) +
    geom_boxplot(alpha = 0.7) +
    xlab("Set") +
    ylab("Temperature Standard Deviation (°C)") +
    ggtitle(paste0(SpeciesOfInterest_Names[[i]],": Thermal Breadth (SD)")) +
    labs(fill = "Set") +
    theme_bw() +
    coord_flip()
  
  
  #------------------------------------------------------
  # Combined plots
  #------------------------------------------------------
  save_name <- paste0("Thermal Breadth Check Figures/",SpeciesOfInterest_Names[[i]]," - Temp Mean.pdf")
  pdf(save_name)
  grid.arrange(gg_hist_mean, gg_boxplot_mean + theme(plot.title = element_blank()), nrow=2)
  dev.off()
  
  save_name <- paste0("Thermal Breadth Check Figures/",SpeciesOfInterest_Names[[i]]," - Temp SD.pdf")
  pdf(save_name)
  grid.arrange(gg_hist_sd, gg_boxplot_sd + theme(plot.title = element_blank()), nrow=2)
  dev.off()
  
}


