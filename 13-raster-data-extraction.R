#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Extract and collate raster data for each of the mosquito species' analysis dataset
#######################################################

source("C:/Users/tejas/Documents/GitHub/mosquito-sdm/0-config.R")


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
  setNames(c("An_Gambiae","An_Stephensi","Main"))
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
             "Cx_Pipiens","Cx_Quinque","Cx_Tarsalis"))



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
## STEP 2: EXTRACT RASTER VALUES BY SPECIES ##
#------------------------------------------------------
#------------------------------------------------------
# Create lists and containers to house train-eval metadata and extracted raster covariate data
#------------------------------------------------------
traineval_metadata <- data.frame(matrix(ncol = 8, nrow = 8))
colnames(traineval_metadata) <- c("Species","Activity_Season","Training_Occ","Training_Bg","Evaluation_Occ","Evaluation_Bg","Total_Occ","Total_Bg")
sdm_data <- list()


#------------------------------------------------------
# Loop through by species
#------------------------------------------------------
for(i in 1:length(SpeciesOfInterest_Names)) {
  print(paste0("-- Step 2: Extracting raster values for ", SpeciesOfInterest_Names[[i]]," --"))
  tic <- Sys.time()
  
  #------------------------------------------------------
  # Set predictor stack according to specific activity season setting
  #------------------------------------------------------
  if(SpeciesOfInterest_Names[[i]] == "Aedes aegypti" |
     SpeciesOfInterest_Names[[i]] == "Anopheles stephensi" |
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
  # Extract raster covariate values for occ and bg
  #------------------------------------------------------
  predictors_occ <- data.frame(raster::extract(predictors, occ)) %>%
    cbind(c(rep(SpeciesOfInterest_Names[[i]], nrow(.))),
          st_coordinates(occ),
          c(rep(ActivitySeason_Type[i], nrow(.))),
          c(rep("1", nrow(.))))
  
  predictors_bg <- data.frame(raster::extract(predictors, bg)) %>%
    cbind(c(rep(SpeciesOfInterest_Names[[i]], nrow(.))),
          st_coordinates(bg),
          c(rep(ActivitySeason_Type[i], nrow(.))),
          c(rep("0", nrow(.))))
  
  
  #------------------------------------------------------
  # Split data 80% for model training, and 20% for evaluation
  #------------------------------------------------------
  set.seed(seedNum)
  train_percent <- 0.8
  
  predictors_occ$Data_Split <- NA
  predictors_occ$Data_Split[sample(nrow(predictors_occ),
                                   size = train_percent * nrow(predictors_occ),
                                   replace = FALSE)] <- "Training"
  predictors_occ$Data_Split[is.na(predictors_occ$Data_Split)] <- "Evaluation"
  
  predictors_bg$Data_Split <- NA
  predictors_bg$Data_Split[sample(nrow(predictors_bg),
                                  size = train_percent * nrow(predictors_bg),
                                  replace = FALSE)] <- "Training"
  predictors_bg$Data_Split[is.na(predictors_bg$Data_Split)] <- "Evaluation"
  
  
  #------------------------------------------------------
  # Rename columns and reorder
  #------------------------------------------------------
  colnames(predictors_occ)[12] <- "Species"
  colnames(predictors_occ)[13] <- "Centroid_Longitude"
  colnames(predictors_occ)[14] <- "Centroid_Latitude"
  colnames(predictors_occ)[15] <- "Activity_Season"
  colnames(predictors_occ)[16] <- "Occ1_or_Bg0"
  colnames(predictors_bg)[12] <- "Species"
  colnames(predictors_bg)[13] <- "Centroid_Longitude"
  colnames(predictors_bg)[14] <- "Centroid_Latitude"
  colnames(predictors_bg)[15] <- "Activity_Season"
  colnames(predictors_bg)[16] <- "Occ1_or_Bg0"
  
  predictors_occ %<>% .[,c(12,15:17,13:14,1:11)]
  predictors_bg %<>% .[,c(12,15:17,13:14,1:11)]
  
  
  #------------------------------------------------------
  # Merge dataframes from occ and bg
  #------------------------------------------------------
  predictors_all <- rbind(predictors_occ, predictors_bg)
  sdm_data[[i]] <- predictors_all
 
  
  #------------------------------------------------------
  # Record the train-eval metadata for occ and bg
  #------------------------------------------------------
  traineval_metadata[[1]][[i]] <- SpeciesOfInterest_Names[[i]]
  traineval_metadata[[2]][[i]] <- ActivitySeason_Type[[i]]
  traineval_metadata[[3]][[i]] <- nrow(predictors_all %>% filter(Data_Split == "Training",
                                                                 Occ1_or_Bg0 == "1"))
  traineval_metadata[[4]][[i]] <- nrow(predictors_all %>% filter(Data_Split == "Training",
                                                                 Occ1_or_Bg0 == "0"))
  traineval_metadata[[5]][[i]] <- nrow(predictors_all %>% filter(Data_Split == "Evaluation",
                                                                 Occ1_or_Bg0 == "1"))
  traineval_metadata[[6]][[i]] <- nrow(predictors_all %>% filter(Data_Split == "Evaluation",
                                                                 Occ1_or_Bg0 == "0"))
  traineval_metadata[[7]][[i]] <- traineval_metadata[[3]][[i]] + traineval_metadata[[5]][[i]]
  traineval_metadata[[8]][[i]] <- traineval_metadata[[4]][[i]] + traineval_metadata[[6]][[i]]
  
  toc <- Sys.time()
  toc - tic
}



#------------------------------------------------------
## STEP 3: FORMAT AND EXPORT THE ANALYSIS DATASET ##
#------------------------------------------------------
sdm_data_all_species <- data.frame()

for(i in 1:length(sdm_data)) {
  print(paste0("-- Step 3: Formatting and exporting raster covariate data and analysis dataset for ", SpeciesOfInterest_Names[[i]]," --"))
  
  #------------------------------------------------------
  # Rename the columns of the dataframe and merge before exporting the .csv
  # {PhotoASTM, PrecipASTM, and TAM} = TAM in the .csv
  # {PhotoASTSD, PrecipASTSD, and TASD} = TASD in the .csv
  #------------------------------------------------------
  colnames(sdm_data[[i]]) <- c("Species","Activity_Season","Occ1_or_Bg0","Data_Split","Centroid_Longitude","Centroid_Latitude",
                              "CD","EVIM","EVISD","FC","HPD","PDQ","PWQ","SW","WS","TAM","TASD")
  
  
  #------------------------------------------------------
  # Merge all species' raster covariate data
  #------------------------------------------------------
  sdm_data_all_species %<>% rbind(sdm_data[[i]])
}


#------------------------------------------------------
# Export .csv files
#------------------------------------------------------
write.csv(traineval_metadata, file = "Metadata/Train Eval Metadata by Species.csv", row.names=FALSE)
write.csv(sdm_data_all_species, file = "Analysis_Dataset/SDM_Data.csv", row.names=FALSE)
print(paste0("-- Completed extraction of the analysis dataset for all species --"))


