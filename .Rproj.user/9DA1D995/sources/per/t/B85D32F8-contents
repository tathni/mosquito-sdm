#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Extract and collate raster data for each of the mosquito species of interest
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
# Load in activity season length rasters
#------------------------------------------------------
activity_lengths <- alply(list.files("Activity Season Lengths",
                                     pattern = ".tif",
                                     full.names = TRUE), 1, function(file){
                                       print(file)
                                       rast <- raster(file)
                                       return(rast)
                                     }) %>%
  setNames(c("Photoperiod","Precipitation"))
activity_lengths_index <- c(NA,1,2,NA,NA,1,NA,1)


#------------------------------------------------------
# Load in summed environmental predictors
#------------------------------------------------------
predictor_sums <- alply(list.files("Environmental Predictors Summed",
                                   pattern = ".tif",
                                   full.names = TRUE), 1, function(file){
                                     print(file)
                                     rast <- raster(file)
                                     return(rast)
                                   }) %>%
  setNames(c("Photoperiod","Precipitation","YearRound"))
predictor_sums_index <- c(3,1,2,3,3,1,3,1)


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
rasterNames <- c("CD","EVIM","EVISD","FC","HPD","PDQ","PhotoASTM","PhotoASTSD","PrecipASTM","PrecipASTSD","PWQ","TAM","TASD","WS")
predictors_preStack <- setNames(predictors_preStack, rasterNames)

predictors_yearRound <- predictors_preStack[c(1:6,11,14,12:13)] %>% stack()
predictors_photoSeason <- predictors_preStack[c(1:6,11,14,7:8)] %>% stack()
predictors_precipSeason <- predictors_preStack[c(1:6,11,14,9:10)] %>% stack()


#------------------------------------------------------
# Create lists and containers to house filter metadata
#------------------------------------------------------
filter_metadata_pre <- readRDS("filter_metadata_pre.RDS")
filter_metadata_shell <- data.frame(matrix(ncol = 4, nrow=8))
colnames(filter_metadata_shell) <- c("Activity_Season","Unique_NonNA_Cells","Activity_Season_Cells","Raster_Sum_NonNA_Cells")
filter_metadata <- cbind(filter_metadata_pre, filter_metadata_shell) %>%
  .[,c(1,8,2:7,9:11)]



#------------------------------------------------------
## STEP 1: PREPARE OCC SF OBJECTS AND BG MASK BY SPECIES ##
#------------------------------------------------------
occ_sf_list <- c()
bg_mask_list <- c()

#------------------------------------------------------
# Loop through by species
#------------------------------------------------------
for(i in 1:length(SpeciesOfInterest_Names)) {
  print(paste0("-- Step 1: Preparing occ sf objects and bg mask for ", SpeciesOfInterest_Names[i]," --"))
  tic <- Sys.time()
  
  #------------------------------------------------------
  ## OCCURRENCES ##
  #------------------------------------------------------
  #------------------------------------------------------
  # Acquire unique raster centroids and create sf object of occurrence points
  #------------------------------------------------------
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Acquiring unique raster centroids and creating sf object for occ"))
  
  occ_points <- Mosquitoes_SpeciesOfInterest %>%
    filter(species == SpeciesOfInterest_Names[[i]]) %>%
    dplyr::select(c(decimalLongitude, decimalLatitude))
  
  rast <- predictors_preStack[[1]] # Choose any generic raster to acquire cells and centroids from
  
  occ_longlat <- cellFromXY(rast, occ_points) %>% as.data.frame() %>%
    setNames("cell") %>% unique() %>%
    mutate(longitude = xFromCell(rast, cell),  # Acquire longitude (x) and latitude (y) from cell centroids
           latitude = yFromCell(rast, cell)) %>%
    dplyr::select(-cell) %>% # Cell number is now obsolete if working from (x,y) as an sf object
    filter(!is.na(longitude) & !is.na(latitude)) # Remove the NA locations
  
  occ_sf <- st_as_sf(occ_longlat, coords = c("longitude","latitude"),
                     crs = 4326, agr = "constant")
  
  unique_cells <- nrow(occ_sf) # Acquire count for filter metadata
  
  
  #------------------------------------------------------
  # Filter occ to locations with activity season length > 0
  #------------------------------------------------------
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Activity season length filtering for occ"))
  
  if(SpeciesOfInterest_Names[[i]] %in% c("Aedes albopictus",
                                         "Anopheles gambiae",
                                         "Culex pipiens",
                                         "Culex tarsalis")) {
    occ_activity_length <- raster::extract(activity_lengths[[(activity_lengths_index[[i]])]], occ_sf)
    occ_activity_sf <- occ_sf[which(occ_activity_length > 0),]
  } else {
    occ_activity_sf <- occ_sf
  }
  
  occ_sf_list <- c(occ_sf_list,
                   list(occ_activity_sf))
  
  activity_season_cells <- nrow(occ_activity_sf) # Acquire count for filter metadata
  
  
  #------------------------------------------------------
  # Filter occ to locations with summed rasters > 0
  #------------------------------------------------------
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Summed rasters filtering for occ"))
  
  occ_summed <- raster::extract(predictor_sums[[(predictor_sums_index[[i]])]], occ_activity_sf)
  occ_summed_sf <- occ_activity_sf[which(occ_summed > 0),]
  
  occ_sf_list <- c(occ_sf_list,
                   list(occ_summed_sf))
  
  raster_sum_cells <- nrow(occ_summed_sf) # Acquire count for filter metadata
  
  
  #------------------------------------------------------
  # Record the filter metadata for occurrences
  #------------------------------------------------------
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Recording the filter metadata for occ"))
  
  filter_metadata[[2]][[i]] <- ActivitySeason_Type[[i]]
  filter_metadata[[9]][[i]] <- unique_cells
  filter_metadata[[10]][[i]] <- activity_season_cells
  filter_metadata[[11]][[i]] <- raster_sum_cells
  
  
  
  #------------------------------------------------------
  ## BACKGROUND MASK ##
  #------------------------------------------------------
  #------------------------------------------------------
  # Filter bg mask to locations within ecoregion
  #------------------------------------------------------
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Ecoregion filtering for bg mask"))
  
  bg_mask <- bias_masks[[(bias_masks_index[[i]])]]
  
  sf::sf_use_s2(FALSE)
  bg_sf <- st_as_sf(bg_mask, coords = c("longitude","latitude"),
                    crs = 4326, agr = "constant")
  
  bg_eco_intersect <- st_intersects(bg_sf, ecoregions[[i]])
  bg_eco_inds <- purrr::map_dbl(bg_eco_intersect, function(x) length(x)) %>% 
    magrittr::is_greater_than(0) %>% which()
  bg_eco_sf <- bg_sf[bg_eco_inds,]
  
  
  #------------------------------------------------------
  # Filter bg mask to locations with activity season length > 0
  #------------------------------------------------------
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Activity season length filtering for bg mask"))
  
  if(SpeciesOfInterest_Names[[i]] %in% c("Aedes albopictus",
                                         "Anopheles gambiae",
                                         "Culex pipiens",
                                         "Culex tarsalis")) {
    bg_activity_length <- raster::extract(activity_lengths[[(activity_lengths_index[i])]], bg_eco_sf)
    bg_activity_sf <- bg_eco_sf[which(bg_activity_length > 0),]
  } else {
    bg_activity_sf <- bg_eco_sf
  }
  
  
  #------------------------------------------------------
  # Filter bg mask to locations with summed rasters > 0
  #------------------------------------------------------
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Summed rasters filtering for bg mask"))
  
  bg_summed <- raster::extract(predictor_sums[[(predictor_sums_index[[i]])]], bg_activity_sf)
  bg_summed_sf <- bg_activity_sf[which(bg_summed > 0),]
  bg_mask_list <- c(bg_mask_list,
                    list(bg_summed_sf))
  
  toc <- Sys.time()
  toc - tic
  
}



#------------------------------------------------------
## STEP 2: EXTRACT RASTER VALUES BY SPECIES ##
#------------------------------------------------------
#------------------------------------------------------
# Create lists and containers to house train-eval metadata, extracted raster covariate data, and species maps
#------------------------------------------------------
traineval_metadata <- data.frame(matrix(ncol = 5, nrow=8))
colnames(traineval_metadata) <- c("Species","Training_Occ","Training_Bg","Evaluation_Occ","Evaluation_Bg")

sdmData <- list()
species_maps <- c()


#------------------------------------------------------
# Loop through by species
#------------------------------------------------------
for(i in 1:length(SpeciesOfInterest_Names)) {
  print(paste0("-- Step 2: Extracting raster values for ", SpeciesOfInterest_Names[i]," --"))
  tic <- Sys.time()
  
  #------------------------------------------------------
  # Set predictor stack according to specific activity season setting
  #------------------------------------------------------
  if(SpeciesOfInterest_Names[i] == "Aedes aegypti" |
     SpeciesOfInterest_Names[i] == "Anopheles stephensi" |
     SpeciesOfInterest_Names[i] == "Culex annulirostris" |
     SpeciesOfInterest_Names[i] == "Culex quinquefasciatus") {
    predictors <- predictors_yearRound }
  
  if(SpeciesOfInterest_Names[i] == "Aedes albopictus" |
     SpeciesOfInterest_Names[i] == "Culex pipiens" |
     SpeciesOfInterest_Names[i] == "Culex tarsalis") {
    predictors <- predictors_photoSeason }
  
  if(SpeciesOfInterest_Names[i] == "Anopheles gambiae") {
    predictors <- predictors_precipSeason }
  
  
  #------------------------------------------------------
  # Select occ and random sample bg without replacement from weighted bias mask at (2x occ) multiplier
  #------------------------------------------------------
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Selecting occ and random sampling bg from weighted bias mask"))
  set.seed(seedNum)
  
  occ <- occ_sf_list[[i]]
  
  bg_df <- bg_mask_list[[i]] %>%
    mutate(weight = count/sum(count))
  bg <- bg_df[sample(nrow(bg_df),
                     size = 2*nrow(occ),
                     replace = FALSE,
                     prob = bg_df$weight),]
  
  
  #------------------------------------------------------
  # Plot occ and selected bg on ecoregion maps
  #------------------------------------------------------
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Plotting occ and selected bg on ecoregion maps"))
  
  world <- st_as_sf(wrld_simpl[wrld_simpl@data$UN!="10",]) %>% st_set_crs(my_crs)
  
  species_plot <- ggplot() + 
    geom_sf(data = ecoregions[[i]], color = "black", fill = "lightgrey", alpha = 0.5) +
    geom_sf(data = occ, aes(color = "Occurrence"), size = 2, alpha = 0.7, show.legend = "point") + 
    geom_sf(data = bg, aes(color = "Background"), size = 2, alpha = 0.4, show.legend = "point") + 
    scale_color_manual(name = "Centroid",
                       values = c("Occurrence" = "#b80700", "Background" = "#003f91")) +
    theme_bw() +
    theme(legend.position="bottom") +
    ggtitle(SpeciesOfInterest_Names[i])
  
  species_maps <- c(species_maps, species_plot)
  
  
  #------------------------------------------------------
  # Extract raster covariate values for occ and bg
  #------------------------------------------------------
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Extracting raster covariate values for occ and bg"))
  
  predictors_occ <- data.frame(raster::extract(predictors, occ)) %>%
    cbind(c(rep(SpeciesOfInterest_Names[i], nrow(.))),
          st_coordinates(occ),
          c(rep(ActivitySeason_Type[i], nrow(.))),
          c(rep("Occurrence", nrow(.))))
  
  predictors_bg <- data.frame(raster::extract(predictors, bg)) %>%
    cbind(c(rep(SpeciesOfInterest_Names[i], nrow(.))),
          st_coordinates(bg),
          c(rep(ActivitySeason_Type[i], nrow(.))),
          c(rep("Background", nrow(.))))
  
  
  #------------------------------------------------------
  # Split data 80% for model training, and 20% for evaluation
  #------------------------------------------------------
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Splitting data into train-evaluation sets at 80%/20%"))
  set.seed(seedNum)
  
  predictors_occ$Data_Split <- NA
  predictors_occ$Data_Split[sample(nrow(predictors_occ),
                                   size = 0.8*nrow(predictors_occ),
                                   replace = FALSE)] <- "Training"
  predictors_occ$Data_Split[is.na(predictors_occ$Data_Split)] <- "Evaluation"
  
  predictors_bg$Data_Split <- NA
  predictors_bg$Data_Split[sample(nrow(predictors_bg),
                                  size = 0.8*nrow(predictors_bg),
                                  replace = FALSE)] <- "Training"
  predictors_bg$Data_Split[is.na(predictors_bg$Data_Split)] <- "Evaluation"
  
  
  #------------------------------------------------------
  # Merge raster covariate dataframes from occ and bg
  #------------------------------------------------------
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Merging raster covariate dataframes from occ and bg"))
  
  colnames(predictors_occ)[11] <- "Species"
  colnames(predictors_occ)[14] <- "Activity_Season"
  colnames(predictors_occ)[15] <- "Occ_or_Bg"
  colnames(predictors_bg)[11] <- "Species"
  colnames(predictors_bg)[14] <- "Activity_Season"
  colnames(predictors_bg)[15] <- "Occ_or_Bg"
  
  predictors_all <- rbind(predictors_occ, predictors_bg)
  
  sdmData <- c(sdmData, predictors_all)
 
  
  #------------------------------------------------------
  # Record the train-eval metadata for occ and bg
  #------------------------------------------------------
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Recording the train-eval metadata for occ and bg"))
  
  traineval_metadata[[1]][[i]] <- SpeciesOfInterest_Names[i]
  traineval_metadata[[2]][[i]] <- ActivitySeason_Type[[i]]
  traineval_metadata[[3]][[i]] <- nrow(predictors_all %>% filter(Data_Split == "Training",
                                                                 Occ_or_Bg == "Occurrence"))
  traineval_metadata[[4]][[i]] <- nrow(predictors_all %>% filter(Data_Split == "Training",
                                                                 Occ_or_Bg == "Background"))
  traineval_metadata[[5]][[i]] <- nrow(predictors_all %>% filter(Data_Split == "Evaluation",
                                                                 Occ_or_Bg == "Occurrence"))
  traineval_metadata[[6]][[i]] <- nrow(predictors_all %>% filter(Data_Split == "Evaluation",
                                                                 Occ_or_Bg == "Occurrence"))
  
  toc <- Sys.time()
  toc - tic
}



#------------------------------------------------------
## STEP 3: FORMAT AND EXPORT DATA/FIGURES ##
#------------------------------------------------------
#------------------------------------------------------
# Format raster covariate dataframes
#------------------------------------------------------
for(i in 1:length(sdmData)) {
  print(paste0("-- Step 3: Formatting raster covariate data for ", SpeciesOfInterest_Names[i]," --"))
  predictors_all <- sdmData[[i]]
  
  if(SpeciesOfInterest_Names[i] == "Aedes aegypti" |
     SpeciesOfInterest_Names[i] == "Anopheles stephensi" |
     SpeciesOfInterest_Names[i] == "Culex annulirostris" |
     SpeciesOfInterest_Names[i] == "Culex quinquefasciatus") {
    ## ??
    predictors_all %<>% .[,c(??)]
  }
  
  if(SpeciesOfInterest_Names[i] == "Aedes albopictus" |
     SpeciesOfInterest_Names[i] == "Culex pipiens" |
     SpeciesOfInterest_Names[i] == "Culex tarsalis") {
    predictors_all %<>% .[,c(??)]
  }
  
  if(SpeciesOfInterest_Names[i] == "Anopheles gambiae") {
    predictors_all %<>% .[,c(??)]
  }
  
  ## need to re-name photo/precip/yr column names by activity ssn, re-order column names, and merge all species together
  ## need to put colnames(predictors_all) <- c("Centroid_Longitude","Centroid_Latitude")
  
  
  #------------------------------------------------------
  # Rename the columns of the dataframe and merge before exporting the .csv
  # {PhotoASTM, PrecipASTM, and TAM} = TAM in the .csv
  # {PhotoASTSD, PrecipASTSD, and TASD} = TASD in the .csv
  #------------------------------------------------------
  colnames(df_yearRound) <- c("Species","Longitude","Latitude","DataSplit","Occ1_or_Bg0","ELEV","EVIM",
                              "EVISD","FC","HPD","PDQ","PWQ","TAM","TASD")
  
  colnames(df_photoSeason) <- c("Species","Longitude","Latitude","DataSplit","Occ1_or_Bg0","ELEV","EVIM",
                                "EVISD","FC","HPD","PDQ","TAM","TASD","PWQ")
  df_photoSeason <- df_photoSeason[,c(1:11,14,12:13)]
  
  colnames(df_precipSeason) <- c("Species","Longitude","Latitude","DataSplit","Occ1_or_Bg0","ELEV","EVIM",
                                 "EVISD","FC","HPD","PDQ","TAM","TASD","PWQ")
  df_precipSeason <- df_precipSeason[,c(1:11,14,12:13)]
  
  
  df_final <- rbind(df_yearRound, df_photoSeason, df_precipSeason)
}


#------------------------------------------------------
# Merge all species' raster covariate data
#------------------------------------------------------
# ??


#------------------------------------------------------
# Save species occurrence and bg on ecoregion maps
#------------------------------------------------------
for(i in 1:length(species_maps)) {
  species_plot <- species_maps[[i]]
  save_name <- paste0("! Figures/Species Occurrence Maps/",SpeciesOfInterest_Names[[i]]," - Occ and Bg on Ecoregions.pdf")
  pdf(save_name)
  species_plot
  dev.off()
}


#------------------------------------------------------
# Export .csv files
#------------------------------------------------------
# ??
write.csv(traineval_metadata, file = "Train Eval Metadata by Species.csv", row.names=FALSE)
write.csv(filter_metadata, file = "Filter Metadata by Species.csv", row.names=FALSE)
write.csv(df_final, file = "SDM Data.csv", row.names=FALSE)











# -- 


#------------------------------------------------------
## DATA LOAD-IN ##
#------------------------------------------------------
#------------------------------------------------------
# Load in mosquito occurrences
#------------------------------------------------------
print(paste0("Loading in mosquito occurrence data"))
Mosquitoes_SpeciesOfInterest <- read.csv("GBIF_Datasets_Cleaned/Mosquitoes_SpeciesOfInterest.csv", header = TRUE,
                                         encoding = "UTF-8", stringsAsFactors = FALSE)
Background_Culicidae <- read.csv("GBIF_Datasets_Cleaned/Background_Culicidae.csv", header = TRUE,
                                 encoding = "UTF-8", stringsAsFactors = FALSE)
Background_Culicidae_Australia_Supplement <- read.csv("GBIF_Datasets_Cleaned/Background_Culicidae_Australia_Supplement.csv",
                                                      header = TRUE, encoding = "UTF-8", stringsAsFactors = FALSE)

#------------------------------------------------------
# Load in environmental predictors
#------------------------------------------------------
print(paste0("Loading in raster data"))
predictors_preStack <- alply(list.files("Environmental Predictors Merged",
                                        pattern = ".tif",
                                        full.names = TRUE), 1, function(file){
                                          print(file)
                                          rast <- raster(file)
                                          return(rast)
                                        })
rasterNames <- c("ELEV","EVIM","EVISD","FC","HPD","PDQ","PhotoASTM","PhotoASTSD","PrecipASTM","PrecipASTSD","PWQ","TAM","TASD")
predictors_preStack <- setNames(predictors_preStack, rasterNames)


#------------------------------------------------------
# Compress predictors into a stacked raster for each activity season combination
#------------------------------------------------------
predictors_yearRound <- predictors_preStack[c(1:6,11:13)] %>% stack()
predictors_photoSeason <- predictors_preStack[c(1:8,11)] %>% stack()
predictors_precipSeason <- predictors_preStack[c(1:6,9:11)] %>% stack()
predictor_sum_yearRound <- raster("Predictor_Sum_YearRound.tif")
predictor_sum_photoSeason <- raster("Predictor_Sum_PhotoSeason.tif")
predictor_sum_precipSeason <- raster("Predictor_Sum_PrecipSeason.tif")


#------------------------------------------------------
# Read in the cropped raster sampling ranges and bias masks
#------------------------------------------------------
sampling_ranges <- alply(list.files("Sampling Range Maps",
                                   pattern = ".tif",
                                   full.names = TRUE), 1, function(file){
                                     print(file)
                                     rast <- raster(file)
                                     return(rast)
                                   })

bias_masks <- alply(list.files("Bias Masks",
                                  pattern = ".tif",
                                  full.names = TRUE), 1, function(file){
                                    print(file)
                                    rast <- raster(file)
                                    return(rast)
                                  })



#------------------------------------------------------
## PREPARE LISTS AND CONTAINERS ##
#------------------------------------------------------
#------------------------------------------------------
# Compile necessary lists and values needed in the SDM for loop
#------------------------------------------------------
speciesList <- c("AedesAegypti",
                  "AedesAlbopictus",
                  "AnophelesGambiae",
                  "AnophelesStephensi",
                  "CulexAnnulirostris",
                  "CulexPipiens",
                  "CulexQuinquefasciatus",
                  "CulexTarsalis")

SpeciesOfInterest_Names <- c("Aedes aegypti",
                             "Aedes albopictus",
                             "Anopheles gambiae",
                             "Anopheles stephensi",
                             "Culex annulirostris",
                             "Culex pipiens",
                             "Culex quinquefasciatus",
                             "Culex tarsalis")

trainingList <- c("AedesAegypti_TrainOcc",
                  "AedesAlbopictus_TrainOcc",
                  "AnophelesGambiae_TrainOcc",
                  "AnophelesStephensi_TrainOcc",
                  "CulexAnnulirostris_TrainOcc",
                  "CulexPipiens_TrainOcc",
                  "CulexQuinquefasciatus_TrainOcc",
                  "CulexTarsalis_TrainOcc")

evaluationList <- c("AedesAegypti_EvalOcc",
                     "AedesAlbopictus_EvalOcc",
                     "AnophelesGambiae_EvalOcc",
                     "AnophelesStephensi_EvalOcc",
                     "CulexAnnulirostris_EvalOcc",
                     "CulexPipiens_EvalOcc",
                     "CulexQuinquefasciatus_EvalOcc",
                     "CulexTarsalis_EvalOcc")

traineval_metadata <- data.frame(matrix(ncol = 6, nrow=8))
colnames(traineval_metadata) <- c("Species","Activity_Season_Restriction","Training_Occ","Training_Bg","Evaluation_Occ","Evaluation_Bg")

filter_metadata <- data.frame(matrix(ncol = 7, nrow=8))
colnames(filter_metadata) <- c("Species","Activity_Season_Restriction","Cleaned_Points","Landmass_Points","Activity_Season_Points",
                           "Sampling_Range_Points","Unique_Points_Final")

mosq_bias_list <- list()

sdmData_yearRound <- list()
sdmData_photoSeason <- list()
sdmData_precipSeason <- list()

counter_YR <- 1
counter_PhS <- 1
counter_PrS <- 1



#------------------------------------------------------
## LOOP THROUGH EACH SPECIES AND RASTER DATA EXTRACT ##
#------------------------------------------------------
for (i in 1:length(SpeciesOfInterest_Names)) { 
  tic <- Sys.time()
  print(paste0("Species of interest is ", SpeciesOfInterest_Names[i]))
  
  #------------------------------------------------------
  # Set predictor stack according to specific activity season setting
  #------------------------------------------------------
  if(SpeciesOfInterest_Names[i] == "Aedes aegypti" |
     SpeciesOfInterest_Names[i] == "Anopheles stephensi" |
     SpeciesOfInterest_Names[i] == "Culex annulirostris" |
     SpeciesOfInterest_Names[i] == "Culex quinquefasciatus") {
    predictorSum <- predictor_sum_yearRound
    predictors <- predictors_yearRound }
  
  if(SpeciesOfInterest_Names[i] == "Aedes albopictus" |
     SpeciesOfInterest_Names[i] == "Culex pipiens" |
     SpeciesOfInterest_Names[i] == "Culex tarsalis") {
    predictorSum <- predictor_sum_photoSeason
    predictors <- predictors_photoSeason }
  
  if(SpeciesOfInterest_Names[i] == "Anopheles gambiae") {
    predictorSum <- predictor_sum_precipSeason
    predictors <- predictors_precipSeason }
  
  predictorSum_world <- predictor_sum_yearRound
 
  
  #------------------------------------------------------
  # Isolate long/lat coordinates of occurrence points
  #------------------------------------------------------
  species_df <- assign(speciesList[i], filter(Mosquitoes_SpeciesOfInterest, species == SpeciesOfInterest_Names[i]))
  occGPS <- dplyr::select(species_df, c(decimalLongitude, decimalLatitude))
  cleaned_points <- nrow(occGPS)
  
  
  
  #------------------------------------------------------
  ## RESTRICT OCCURRENCES THROUGH PROGRESSIVE FILTERS ##
  #------------------------------------------------------
  #------------------------------------------------------
  # First sieve: landmass only
  #------------------------------------------------------
  # Restrict occurrence points to acquire landmass-only points and exclude points in the ocean
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Isolating training and evaluation cells, landmass restricted"))
  remove_df <- c(which(is.na(raster::extract(predictorSum_world, occGPS))))
  if(length(remove_df) > 0) {
    occGPS_noNA <- occGPS[-remove_df,]
  } else {
    occGPS_noNA <- occGPS
  }
  
  landmass_points <- nrow(occGPS_noNA)
  
  
  #------------------------------------------------------
  # Second sieve: activity season
  #------------------------------------------------------
  # Restrict landmass points to acquire points within the given species' activity season configuration
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Isolating training and evaluation cells, activity season restricted"))
  remove_df <- c(which(is.na(raster::extract(predictorSum, occGPS_noNA))))
  if(length(remove_df) > 0) {
    occGPS_activity_noNA <- occGPS_noNA[-remove_df,]
  } else {
    occGPS_activity_noNA <- occGPS_noNA
  }
  
  activity_season_points <- nrow(occGPS_activity_noNA)
  
  
  #------------------------------------------------------
  # Third sieve: sampling range
  #------------------------------------------------------
  # Restrict activity season points to acquire points within the given species' background sampling range
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Isolating training and evaluation cells, sampling range restricted"))
  remove_df <- c(which(is.na(raster::extract(sampling_ranges[[i]], occGPS_activity_noNA))))
  if(length(remove_df) > 0) {
    occGPS_samprange_noNA <- occGPS_activity_noNA[-remove_df,]
  } else {
    occGPS_samprange_noNA <- occGPS_activity_noNA
  }
  
  sampling_range_points <- nrow(occGPS_samprange_noNA)
  
  
  #------------------------------------------------------
  # Fourth sieve: unique, non-NA points
  #------------------------------------------------------
  # Restrict occurrences to unique, non-NA cells of 1km x 1km, and then back-acquire the centroid (x,y) of the cells
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Isolating training and evaluation cells, unique and non-NA"))
  cell_centroids_unique <- xyFromCell(predictors, cellFromXY(predictors, occGPS_samprange_noNA) %>% 
                                unique) %>% as.data.frame()
  
  unique_points <- nrow(cell_centroids_unique)
  
  
  
  #------------------------------------------------------
  # Training-evaluation split
  #------------------------------------------------------
  # Assign 80% of all cells without replacement as training data
  # Set aside 20% for evaluation
  train_ratio <- round(nrow(cell_centroids_unique) * 0.8)
  set.seed(seedNum)
  cells_train <- assign(trainingList[i], cell_centroids_unique[sample(nrow(cell_centroids_unique), train_ratio), ])
  cells_eval <- assign(evaluationList[i], setdiff(cell_centroids_unique, cells_train))
  

  
  #------------------------------------------------------
  ## COVARIATE EXTRACTION ##
  #------------------------------------------------------
  #------------------------------------------------------
  # Extract covariates from occurrences (train and eval)
  #------------------------------------------------------
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Extracting covariate info from training occurrences"))
  train_occ <- cbind(c(rep(1, nrow(cells_train))),
                     data.frame(raster::extract(predictors, cells_train)),
                     cells_train,
                     c(rep("Training",nrow(cells_train))))
  colnames(train_occ)[1] <- "Occ1_or_Bg0"
  colnames(train_occ)[11] <- "decimalLongitude"
  colnames(train_occ)[12] <- "decimalLatitude"
  colnames(train_occ)[13] <- "dataSplit"
  train_occ <- train_occ[,c(11:13,1:10)]
  
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Extracting covariate info from evaluation occurrences"))
  eval_occ <- cbind(c(rep(1, nrow(cells_eval))),
                    data.frame(raster::extract(predictors, cells_eval)),
                    cells_eval,
                    c(rep("Evaluation",nrow(cells_eval))))
  colnames(eval_occ)[1] <- "Occ1_or_Bg0"
  colnames(eval_occ)[11] <- "decimalLongitude"
  colnames(eval_occ)[12] <- "decimalLatitude"
  colnames(eval_occ)[13] <- "dataSplit"
  eval_occ <- eval_occ[,c(11:13,1:10)]
  
  
  
  #------------------------------------------------------
  # Bias mask setup
  #------------------------------------------------------
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Setting up bias mask and sampling weights"))
  # Subset the background pool of mosquitoes to exclude the current species of interest
  possible_bg <- Background_Culicidae %>%
    filter(!species == SpeciesOfInterest_Names[[i]]) %>%
    .[,c("decimalLongitude", "decimalLatitude")]
  if(SpeciesOfInterest_Names[i] == "Culex annulirostris") {
    possible_bg %<>% rbind(Background_Culicidae_Australia_Supplement %>% .[,c("decimalLongitude", "decimalLatitude")])
  }
  
  # Restrict the pool of points to the a priori defined geographic sampling range
  remove_df <- c(which(is.na(raster::extract(sampling_ranges[[i]], possible_bg))))
  if(length(remove_df) > 0) {
    possible_bg_noNA <- possible_bg[-remove_df,]
  } else {
    possible_bg_noNA <- possible_bg
  } 
  
  
  #------------------------------------------------------
  # Populate the currently-empty bias mask for background sampling
  #------------------------------------------------------
  mosq_bias <- bias_masks[[i]]  
  pixels <- cellFromXY(mosq_bias, 
                       cbind(possible_bg$decimalLongitude,   
                             possible_bg$decimalLatitude)) %>% table
  mosq_bias[as.numeric(names(pixels))] <- mosq_bias[as.numeric(names(pixels))] + 
    as.vector(pixels)
  
  
  
  #------------------------------------------------------
  # Background sampling
  #------------------------------------------------------
  # Random sample and select background points from the biased, weighted mask
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Sampling background cells"))
  select_trainBg <- nrow(cells_train) # Same number of background points as occurrence points
  select_evalBg <- nrow(cells_eval)
  
  memory.limit(size=56000)
  cells_train_bg <- enmSdm::sampleRast(mosq_bias, n = select_trainBg, replace = F, prob = T) %>%
    as.data.frame()
  cells_eval_bg <- enmSdm::sampleRast(mosq_bias, n = select_evalBg, replace = F, prob = T) %>%
    as.data.frame()
  
  
  
  
  #------------------------------------------------------
  # Extract covariates from background (train and eval)
  #------------------------------------------------------
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Extracting covariate info from training background"))
  set.seed(seedNum)
  cells_train_bg_predictors <- cbind(data.frame(raster::extract(predictors, cells_train_bg)),
                                     cells_train_bg)
  train_bg <- cbind(c(rep(0, nrow(cells_train_bg_predictors))),
                          cells_train_bg_predictors,
                          c(rep("Training", nrow(cells_train_bg_predictors))))

  colnames(train_bg)[1] <- "Occ1_or_Bg0"
  colnames(train_bg)[11] <- "decimalLongitude"
  colnames(train_bg)[12] <- "decimalLatitude"
  colnames(train_bg)[13] <- "dataSplit"
  train_bg <- train_bg[,c(11:13,1:10)]

  
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Extracting covariate info from evaluation background"))
  set.seed(seedNum)
  cells_eval_bg_predictors <- cbind(data.frame(raster::extract(predictors, cells_eval_bg)),
                                    cells_eval_bg)
  eval_bg <- cbind(c(rep(0, nrow(cells_eval_bg_predictors))),
                         cells_eval_bg_predictors,
                         c(rep("Evaluation", nrow(cells_eval_bg_predictors))))
                 
  colnames(eval_bg)[1] <- "Occ1_or_Bg0"
  colnames(eval_bg)[11] <- "decimalLongitude"
  colnames(eval_bg)[12] <- "decimalLatitude"
  colnames(eval_bg)[13] <- "dataSplit"
  eval_bg <- eval_bg[,c(11:13,1:10)]
  

  print(paste0("Training_Occ: ", nrow(train_occ), "; Training_Bg: ", nrow(train_bg),
               "; Evaluation_Occ: ",nrow(eval_occ), "; Evaluation_Bg: ", nrow(eval_bg)))
  
  
  
  #------------------------------------------------------
  ## DATA MERGING AND BINDING ##
  #------------------------------------------------------
  #------------------------------------------------------
  # Merge training and occurrences for both the training set and evaluation set
  #------------------------------------------------------
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Merging covariate info from occ/bg points for training/eval"))
  predictors_train_df <- rbind(train_occ, train_bg)
  predictors_eval_df <- rbind(eval_occ, eval_bg)
  
  
  #------------------------------------------------------
  # Bind the training and background datasets into one
  # Complete cases should remove nothing; this is a safety check to ensure that sdmData_raw = sdmData
  #------------------------------------------------------
  sdmData_raw <- rbind(predictors_train_df, predictors_eval_df)
  sdmData <- sdmData_raw[complete.cases(sdmData_raw), ] %>%  
    mutate(species = SpeciesOfInterest_Names[i])
  sdmData <- sdmData[,c(14,1:13)]
  print(paste0("SDM Data Raw: ", nrow(sdmData_raw), "; SDM Data: ", nrow(sdmData)))
  
  
  #------------------------------------------------------
  # Save the data for each species into a list, to compile into a single csv below
  #------------------------------------------------------
  if(SpeciesOfInterest_Names[i] == "Aedes aegypti" |
     SpeciesOfInterest_Names[i] == "Anopheles stephensi" |
     SpeciesOfInterest_Names[i] == "Culex annulirostris" |
     SpeciesOfInterest_Names[i] == "Culex quinquefasciatus") {
    sdmData_yearRound[[counter_YR]] <- sdmData
    counter_YR <- counter_YR + 1
  }
  
  if(SpeciesOfInterest_Names[i] == "Aedes albopictus" |
     SpeciesOfInterest_Names[i] == "Culex pipiens" |
     SpeciesOfInterest_Names[i] == "Culex tarsalis") {
    sdmData_photoSeason[[counter_PhS]] <- sdmData
    counter_PhS <- counter_PhS + 1
  }
  
  if(SpeciesOfInterest_Names[i] == "Anopheles gambiae") {
    sdmData_precipSeason[[counter_PrS]] <- sdmData
    counter_PrS <- counter_PrS + 1
  }
  
  
  
  #------------------------------------------------------
  ## SUMMARY STATISTICS ##
  #------------------------------------------------------
  #------------------------------------------------------
  # Save summary statistics in the pre-created dataframe
  #------------------------------------------------------
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Saving summary statistics"))
  traineval_metadata[[1]][[i]] <- SpeciesOfInterest_Names[i]
  traineval_metadata[[2]][[i]] <- ActivitySeason_Type[[i]]
  traineval_metadata[[3]][[i]] <- nrow(train_occ)
  traineval_metadata[[4]][[i]] <- nrow(train_bg)
  traineval_metadata[[5]][[i]] <- nrow(eval_occ)
  traineval_metadata[[6]][[i]] <- nrow(eval_bg)
  
  
  filter_metadata[[1]][[i]] <- SpeciesOfInterest_Names[i]
  filter_metadata[[2]][[i]] <- ActivitySeason_Type[[i]]
  filter_metadata[[3]][[i]] <- cleaned_points
  filter_metadata[[4]][[i]] <- landmass_points
  filter_metadata[[5]][[i]] <- activity_season_points
  filter_metadata[[6]][[i]] <- sampling_range_points
  filter_metadata[[7]][[i]] <- unique_points

  
  #------------------------------------------------------
  # Print elapsed time for given species' raster data extraction
  #------------------------------------------------------
  toc <- Sys.time()
  toc - tic
  
}


#------------------------------------------------------
## CONSOLIDATE DATAFRAMES FOR SAVING ##
#------------------------------------------------------
#------------------------------------------------------
# Create merged dataframes to output as .csv's
#------------------------------------------------------
df_yearRound <- sdmData_yearRound[[1]][FALSE, ]
df_photoSeason <- sdmData_photoSeason[[1]][FALSE, ]
df_precipSeason <- sdmData_precipSeason[[1]][FALSE, ]

counter_YR <- 1
counter_PhS <- 1
counter_PrS <- 1


for (i in 1:length(SpeciesOfInterest_Names)) {
  if(SpeciesOfInterest_Names[i] == "Aedes aegypti" |
     SpeciesOfInterest_Names[i] == "Anopheles stephensi" |
     SpeciesOfInterest_Names[i] == "Culex annulirostris" |
     SpeciesOfInterest_Names[i] == "Culex quinquefasciatus") {
    df_yearRound <- rbind(df_yearRound, sdmData_yearRound[[counter_YR]])
    counter_YR <- counter_YR + 1
  }
  
  if(SpeciesOfInterest_Names[i] == "Aedes albopictus" |
     SpeciesOfInterest_Names[i] == "Culex pipiens" |
     SpeciesOfInterest_Names[i] == "Culex tarsalis") {
    df_photoSeason <- rbind(df_photoSeason, sdmData_photoSeason[[counter_PhS]])
    counter_PhS <- counter_PhS + 1
  }
  
  if(SpeciesOfInterest_Names[i] == "Anopheles gambiae") {
    df_precipSeason <- rbind(df_precipSeason, sdmData_precipSeason[[counter_PrS]])
    counter_PrS <- counter_PrS + 1
  }
}


#------------------------------------------------------
# Rename the columns of the dataframe and merge before exporting the .csv
# {PhotoASTM, PrecipASTM, and TAM} = TAM in the .csv
# {PhotoASTSD, PrecipASTSD, and TASD} = TASD in the .csv
#------------------------------------------------------
colnames(df_yearRound) <- c("Species","Longitude","Latitude","DataSplit","Occ1_or_Bg0","ELEV","EVIM",
                                  "EVISD","FC","HPD","PDQ","PWQ","TAM","TASD")

colnames(df_photoSeason) <- c("Species","Longitude","Latitude","DataSplit","Occ1_or_Bg0","ELEV","EVIM",
                                  "EVISD","FC","HPD","PDQ","TAM","TASD","PWQ")
df_photoSeason <- df_photoSeason[,c(1:11,14,12:13)]

colnames(df_precipSeason) <- c("Species","Longitude","Latitude","DataSplit","Occ1_or_Bg0","ELEV","EVIM",
                                  "EVISD","FC","HPD","PDQ","TAM","TASD","PWQ")
df_precipSeason <- df_precipSeason[,c(1:11,14,12:13)]


df_final <- rbind(df_yearRound, df_photoSeason, df_precipSeason)


#------------------------------------------------------
# Merge filter_metadata with filter_metadata_pre from the data cleaning script
#------------------------------------------------------
filter_metadata_pre <- readRDS("filter_metadata_pre.RDS")
filter_metadata %<>% cbind(filter_metadata_pre[2:7]) %>%
  .[,c(1:2,8:13,3:7)]


#------------------------------------------------------
# Output .csv files
#------------------------------------------------------
write.csv(traineval_metadata, file = "Train Eval Metadata by Species.csv", row.names=FALSE)
write.csv(filter_metadata, file = "Filter Metadata by Species.csv", row.names=FALSE)
write.csv(df_final, file = "SDM Data.csv", row.names=FALSE)



