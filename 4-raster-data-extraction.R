# This script extracts and collates raster data for each of the 8 species of interest

### SETUP ###
library(raster)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(dismo)
library(enmSdm)
library(rgdal)
library(sf)
library(maptools)
library(beepr)
data(wrld_simpl)
setwd("E:/SynologyDrive/Tejas_Server/! Research/! Mordecai Lab/! Mosquito SDM MaxEnt Mechanistic/")
memory.limit(size=56000)
seedNum <- 250



### LOAD IN MOSQUITO OCCURRENCES ###
print(paste0("Loading in mosquito occurrence data"))
# Read in cleaned mosquito species occurrence data and country codes list
Mosquitoes_SpeciesOfInterest <- read.csv("GBIF Datasets Cleaned/Mosquitoes_SpeciesOfInterest.csv", header = TRUE,
                                         encoding = "UTF-8", stringsAsFactors = FALSE)
Background_Culicidae <- read.csv("GBIF Datasets Cleaned/Background_Culicidae.csv", header = TRUE,
                                 encoding = "UTF-8", stringsAsFactors = FALSE)
Background_Culicidae_Australia_Supplement <- read.csv("GBIF Datasets Cleaned/Background_Culicidae_Australia_Supplement.csv",
                                                      header = TRUE, encoding = "UTF-8", stringsAsFactors = FALSE)


# List of species of interest
SpeciesOfInterest_Names <- c("Aedes aegypti",
                             "Aedes albopictus",
                             "Anopheles gambiae",
                             "Anopheles stephensi",
                             "Culex annulirostris",
                             "Culex pipiens",
                             "Culex quinquefasciatus",
                             "Culex tarsalis")
ActivitySeason_Type <- c("None- Year Round",
                         "Photoperiod",
                         "Precipitation",
                         "None- Year Round",
                         "None- Year Round",
                         "Photoperiod",
                         "None- Year Round",
                         "Photoperiod")




### CREATE STACKED RASTER SUMS ###
# predictor_sum_yearRound <- sum(predictors_yearRound) %>%
#   reclassify(cbind(-Inf, 0, NA), right=T)
# 
# predictor_sum_photoSeason <- sum(predictors_photoSeason) %>%
#   reclassify(cbind(-Inf, 0, NA), right=T)
# 
# predictor_sum_precipSeason <- sum(predictors_precipSeason) %>%
#   reclassify(cbind(-Inf, 0, NA), right=T)
# 
# writeRaster(predictor_sum_yearRound, filename = "Predictor_Sum_YearRound.tif", format = "GTiff", overwrite=T)
# writeRaster(predictor_sum_photoSeason, filename = "Predictor_Sum_PhotoSeason.tif", format = "GTiff", overwrite=T)
# writeRaster(predictor_sum_precipSeason, filename = "Predictor_Sum_PrecipSeason.tif", format = "GTiff", overwrite=T)




### CREATE BACKGROUND SAMPLING MAPS ###
# predictor_sum_yearRound <- raster("Predictor_Sum_YearRound.tif")
# predictor_sum_photoSeason <- raster("Predictor_Sum_PhotoSeason.tif")
# predictor_sum_precipSeason <- raster("Predictor_Sum_PrecipSeason.tif")
# 
# SouthAmerica_list <- c("Colombia", "Venezuela", "Suriname", "Guyana", "French Guiana",
#                        "Ecuador", "Peru", "Bolivia", "Chile", "Argentina", "Uruguay",
#                        "Paraguay", "Brazil", "Falkland Islands (Malvinas)")
# SouthAmerica <- wrld_simpl[wrld_simpl$NAME %in% SouthAmerica_list, ]
# NorthAmerica <- wrld_simpl[wrld_simpl$REGION==19,]
# NorthAmerica <- NorthAmerica[!NorthAmerica$NAME %in% SouthAmerica_list, ]
# Africa <- wrld_simpl[wrld_simpl$REGION==2,]
# Oceania <- wrld_simpl[wrld_simpl$REGION==9,]
# Europe <- wrld_simpl[wrld_simpl$REGION==150,]
# Europe_noRussia <- wrld_simpl[wrld_simpl$REGION==150 & !wrld_simpl$NAME == "Russia", ]
# Asia <- wrld_simpl[wrld_simpl$REGION==142,]
# SouthAsia_list <- c("India","Pakistan","Nepal","Bangladesh","Sri Lanka", "Bhutan")
# SouthAsia <- wrld_simpl[wrld_simpl$NAME %in% SouthAsia_list, ]
# PipiensOnly <- rbind(NorthAmerica, Europe_noRussia,
#                      wrld_simpl[wrld_simpl$NAME %in% "Japan", ],
#                      wrld_simpl[wrld_simpl$NAME == "Australia", ])
# 
# AedesAegypti_map <- rbind(NorthAmerica, SouthAmerica, Africa, Asia, Oceania)
# AedesAlbopictus_map <- rbind(NorthAmerica, SouthAmerica, Europe_noRussia, Africa, Asia)
# AnophelesGambiae_map <- Africa
# AnophelesStephensi_map <- SouthAsia
# CulexAnnulirostris_map <- Oceania
# CulexPipiens_map <- PipiensOnly
# CulexQuinquefasciatus_map <- rbind(NorthAmerica, Oceania, Asia)
# CulexTarsalis_map <- NorthAmerica
# 
# # A cropped, summed predictor raster for each species of interest only within regions/continents of occurrence
# map_1 <- crop(predictor_sum_yearRound, extent(AedesAegypti_map)) %>%
#   mask(AedesAegypti_map)
# map_2 <- crop(predictor_sum_photoSeason, extent(AedesAlbopictus_map)) %>%
#   mask(AedesAlbopictus_map)
# map_3 <- crop(predictor_sum_precipSeason, extent(AnophelesGambiae_map)) %>%
#   mask(AnophelesGambiae_map)
# map_4 <- crop(predictor_sum_yearRound, extent(AnophelesStephensi_map)) %>%
#   mask(AnophelesStephensi_map)
# map_5 <- crop(predictor_sum_yearRound, extent(CulexAnnulirostris_map)) %>%
#   mask(CulexAnnulirostris_map)
# map_6 <- crop(predictor_sum_photoSeason, extent(CulexPipiens_map)) %>%
#   mask(CulexPipiens_map)
# map_7 <- crop(predictor_sum_yearRound, extent(CulexQuinquefasciatus_map)) %>%
#   mask(CulexQuinquefasciatus_map)
# map_8 <- crop(predictor_sum_photoSeason, extent(CulexTarsalis_map)) %>%
#   mask(CulexTarsalis_map)
# 
# # Save cropped rasters for future use
# writeRaster(map_1, filename = "Sampling Range Maps/AeAegypti_SamplingRange.tif", format = "GTiff", overwrite=T)
# writeRaster(map_2, filename = "Sampling Range Maps/AeAlbopictus_SamplingRange.tif", format = "GTiff", overwrite=T)
# writeRaster(map_3, filename = "Sampling Range Maps/AnGambiae_SamplingRange.tif", format = "GTiff", overwrite=T)
# writeRaster(map_4, filename = "Sampling Range Maps/AnStephensi_SamplingRange.tif", format = "GTiff", overwrite=T)
# writeRaster(map_5, filename = "Sampling Range Maps/CxAnnulirostris_SamplingRange.tif", format = "GTiff", overwrite=T)
# writeRaster(map_6, filename = "Sampling Range Maps/CxPipiens_SamplingRange.tif", format = "GTiff", overwrite=T)
# writeRaster(map_7, filename = "Sampling Range Maps/CxQuinquefasciatus_SamplingRange.tif", format = "GTiff", overwrite=T)
# writeRaster(map_8, filename = "Sampling Range Maps/CxTarsalis_SamplingRange.tif", format = "GTiff", overwrite=T)


# # Diagnostic continent maps for troubleshooting
# map_NA <- crop(predictor_sum_yearRound, extent(NorthAmerica)) %>%
#   mask(NorthAmerica)
# map_SA <- crop(predictor_sum_yearRound, extent(SouthAmerica)) %>%
#   mask(SouthAmerica)
# map_Africa <- crop(predictor_sum_yearRound, extent(Africa)) %>%
#   mask(Africa)
# map_Oceania <- crop(predictor_sum_yearRound, extent(Oceania)) %>%
#   mask(Oceania)
# map_Europe <- crop(predictor_sum_yearRound, extent(Europe)) %>%
#   mask(Europe)
# map_Europe_noRussia <- crop(predictor_sum_yearRound, extent(Europe_noRussia)) %>%
#   mask(Europe_noRussia)
# map_Asia <- crop(predictor_sum_yearRound, extent(Asia)) %>%
#   mask(Asia)
# map_SouthAsia <- crop(predictor_sum_yearRound, extent(SouthAsia)) %>%
#   mask(SouthAsia)
# 
# writeRaster(map_NA, filename = "Diagnostic Continent Maps/NorthAmerica_DiagnosticMap.tif", format = "GTiff", overwrite=T)
# writeRaster(map_SA, filename = "Diagnostic Continent Maps/SouthAmerica_DiagnosticMap.tif", format = "GTiff", overwrite=T)
# writeRaster(map_Africa, filename = "Diagnostic Continent Maps/Africa_DiagnosticMap.tif", format = "GTiff", overwrite=T)
# writeRaster(map_Oceania, filename = "Diagnostic Continent Maps/Oceania_DiagnosticMap.tif", format = "GTiff", overwrite=T)
# writeRaster(map_Europe, filename = "Diagnostic Continent Maps/Europe_DiagnosticMap.tif", format = "GTiff", overwrite=T)
# writeRaster(map_Europe_noRussia, filename = "Diagnostic Continent Maps/Europe_noRussia_DiagnosticMap.tif", format = "GTiff", overwrite=T)
# writeRaster(map_Asia, filename = "Diagnostic Continent Maps/Asia_DiagnosticMap.tif", format = "GTiff", overwrite=T)
# writeRaster(map_SouthAsia, filename = "Diagnostic Continent Maps/SouthAsia_DiagnosticMap.tif", format = "GTiff", overwrite=T)





### CREATE BIAS MASK FOR SAMPLING ###
# biasMask_1 <- sampling_ranges[[1]]
# biasMask_1[!is.na(biasMask_1)] <- 0
# biasMask_2 <- sampling_ranges[[2]]
# biasMask_2[!is.na(biasMask_2)] <- 0
# biasMask_3 <- sampling_ranges[[3]]
# biasMask_3[!is.na(biasMask_3)] <- 0
# biasMask_4 <- sampling_ranges[[4]]
# biasMask_4[!is.na(biasMask_4)] <- 0
# biasMask_5 <- sampling_ranges[[5]]
# biasMask_5[!is.na(biasMask_5)] <- 0
# biasMask_6 <- sampling_ranges[[6]]
# biasMask_6[!is.na(biasMask_6)] <- 0
# biasMask_7 <- sampling_ranges[[7]]
# biasMask_7[!is.na(biasMask_7)] <- 0
# biasMask_8 <- sampling_ranges[[8]]
# biasMask_8[!is.na(biasMask_8)] <- 0
# 
# # Save bias mask rasters for future use
# writeRaster(biasMask_1, filename = "Bias Masks/AeAegypti_BiasMask.tif", format = "GTiff", overwrite=T)
# writeRaster(biasMask_2, filename = "Bias Masks/AeAlbopictus_BiasMask.tif", format = "GTiff", overwrite=T)
# writeRaster(biasMask_3, filename = "Bias Masks/AnGambiae_BiasMask.tif", format = "GTiff", overwrite=T)
# writeRaster(biasMask_4, filename = "Bias Masks/AnStephensi_BiasMask.tif", format = "GTiff", overwrite=T)
# writeRaster(biasMask_5, filename = "Bias Masks/CxAnnulirostris_BiasMask.tif", format = "GTiff", overwrite=T)
# writeRaster(biasMask_6, filename = "Bias Masks/CxPipiens_BiasMask.tif", format = "GTiff", overwrite=T)
# writeRaster(biasMask_7, filename = "Bias Masks/CxQuinquefasciatus_BiasMask.tif", format = "GTiff", overwrite=T)
# writeRaster(biasMask_8, filename = "Bias Masks/CxTarsalis_BiasMask.tif", format = "GTiff", overwrite=T)




### LOAD IN RASTERS ###
print(paste0("Loading in raster data"))
# Read in and name the environmental predictors
predictors_preStack <- alply(list.files("Environmental Predictors Merged",
                                        pattern = ".tif",
                                        full.names = TRUE), 1, function(file){
                                          print(file)
                                          rast <- raster(file)
                                          return(rast)
                                        })
rasterNames <- c("ELEV","EVIM","EVISD","FC","HPD","PDQ","PhotoASTM","PhotoASTSD","PrecipASTM","PrecipASTSD","PWQ","TAM","TASD")
predictors_preStack <- setNames(predictors_preStack, rasterNames)

# Compress predictors into a stacked raster for each activity season combination
predictors_yearRound <- predictors_preStack[c(1:6,11:13)] %>% stack()
predictors_photoSeason <- predictors_preStack[c(1:8,11)] %>% stack()
predictors_precipSeason <- predictors_preStack[c(1:6,9:11)] %>% stack()
predictor_sum_yearRound <- raster("Predictor_Sum_YearRound.tif")
predictor_sum_photoSeason <- raster("Predictor_Sum_PhotoSeason.tif")
predictor_sum_precipSeason <- raster("Predictor_Sum_PrecipSeason.tif")

# Read in the cropped raster sampling ranges
sampling_ranges <- alply(list.files("Sampling Range Maps",
                                   pattern = ".tif",
                                   full.names = TRUE), 1, function(file){
                                     print(file)
                                     rast <- raster(file)
                                     return(rast)
                                   })

# Read in the bias masks
bias_masks <- alply(list.files("Bias Masks",
                                  pattern = ".tif",
                                  full.names = TRUE), 1, function(file){
                                    print(file)
                                    rast <- raster(file)
                                    return(rast)
                                  })



### PREPARE LISTS AND CONTAINERS ###
# Compile necessary lists needed in the SDM for loop
speciesList <- c("AedesAegypti",
                  "AedesAlbopictus",
                  "AnophelesGambiae",
                  "AnophelesStephensi",
                  "CulexAnnulirostris",
                  "CulexPipiens",
                  "CulexQuinquefasciatus",
                  "CulexTarsalis")

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

trainevalStats <- data.frame(matrix(ncol = 6, nrow=8))
colnames(trainevalStats) <- c("Species","Activity_Season_Restriction","Training_Occ","Training_Bg","Evaluation_Occ","Evaluation_Bg")

filterStats <- data.frame(matrix(ncol = 7, nrow=8))
colnames(filterStats) <- c("Species","Activity_Season_Restriction","Cleaned_Points","Landmass_Points","Activity_Season_Points",
                           "Sampling_Range_Points","Unique_Points_Final")

mosq_bias_list <- list()

sdmData_yearRound <- list()
sdmData_photoSeason <- list()
sdmData_precipSeason <- list()

counter_YR <- 1
counter_PhS <- 1
counter_PrS <- 1


### INITIATE RASTER DATA EXTRACTION ###
# Loop through each species
for (i in 1:length(SpeciesOfInterest_Names)) { 
  tic <- Sys.time()
  print(paste0("Species of interest is ", SpeciesOfInterest_Names[i]))
  
  # Set predictor stack according to specific activity season setting
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
 
  
  
  ### OCCURRENCE POINTS ###
  species_df <- assign(speciesList[i], filter(Mosquitoes_SpeciesOfInterest, species == SpeciesOfInterest_Names[i]))
  
  # Isolate long/lat coordinates of occurrence points
  occGPS <- dplyr::select(species_df, c(decimalLongitude, decimalLatitude))
  cleaned_points <- nrow(occGPS)
  
  
  ### FIRST LEVEL OF RESTRICTION: LANDMASS-ONLY ###
  # Restrict occurrence points to acquire landmass-only points and exclude points in the ocean
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Isolating training and evaluation cells, landmass restricted"))
  remove_df <- c(which(is.na(raster::extract(predictorSum_world, occGPS))))
  if(length(remove_df) > 0) {
    occGPS_noNA <- occGPS[-remove_df,]
  } else {
    occGPS_noNA <- occGPS
  }
  
  landmass_points <- nrow(occGPS_noNA)
  
  
  
  ### SECOND LEVEL OF RESTRICTION: ACTIVITY SEASON ###
  # Restrict landmass points to acquire points within the given species' activity season configuration
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Isolating training and evaluation cells, activity season restricted"))
  remove_df <- c(which(is.na(raster::extract(predictorSum, occGPS_noNA))))
  if(length(remove_df) > 0) {
    occGPS_activity_noNA <- occGPS_noNA[-remove_df,]
  } else {
    occGPS_activity_noNA <- occGPS_noNA
  }
  
  activity_season_points <- nrow(occGPS_activity_noNA)
  
  
  
  ### THIRD LEVEL OF RESTRICTION: SAMPLING RANGE ###
  # Restrict activity season points to acquire points within the given species' background sampling range
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Isolating training and evaluation cells, sampling range restricted"))
  remove_df <- c(which(is.na(raster::extract(sampling_ranges[[i]], occGPS_activity_noNA))))
  if(length(remove_df) > 0) {
    occGPS_samprange_noNA <- occGPS_activity_noNA[-remove_df,]
  } else {
    occGPS_samprange_noNA <- occGPS_activity_noNA
  }
  
  sampling_range_points <- nrow(occGPS_samprange_noNA)
  
  
  
  ### FOURTH LEVEL OF RESTRICTION: UNIQUE, NON-NA ###
  # Restrict occurrences to unique, non-NA cells of 1km x 1km, and then back-acquire the centroid (x,y) of the cells
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Isolating training and evaluation cells, unique and non-NA"))
  cell_centroids_unique <- xyFromCell(predictors, cellFromXY(predictors, occGPS_samprange_noNA) %>% 
                                unique) %>% as.data.frame()
  
  unique_points <- nrow(cell_centroids_unique)
  
  
  
  ### TRAINING-EVALUATION SPLIT ###
  # Assign 80% of all cells without replacement as training data
  # Set aside 20% for evaluation
  train_ratio <- round(nrow(cell_centroids_unique) * 0.8)
  set.seed(seedNum)
  cells_train <- assign(trainingList[i], cell_centroids_unique[sample(nrow(cell_centroids_unique), train_ratio), ])
  cells_eval <- assign(evaluationList[i], setdiff(cell_centroids_unique, cells_train))
  

  
  ### OCCURRENCE COVARIATES EXTRACTION ###
  # Extract covariates for training points occurrences
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
  
  # Extract covariates for evaluation points occurrences
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
  
  
  
  ### BIAS MASK SETUP ###
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
  
  # Populate the currently-empty bias mask for background sampling
  mosq_bias <- bias_masks[[i]]  
  pixels <- cellFromXY(mosq_bias, 
                       cbind(possible_bg$decimalLongitude,   
                             possible_bg$decimalLatitude)) %>% table
  mosq_bias[as.numeric(names(pixels))] <- mosq_bias[as.numeric(names(pixels))] + 
    as.vector(pixels)
  
  
  
  
  ### BACKGROUND SAMPLING ###
  # Random sample and select background points from the biased, weighted mask
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Sampling background cells"))
  select_trainBg <- nrow(cells_train) # Same number of background points as occurrence points
  select_evalBg <- nrow(cells_eval)
  
  memory.limit(size=56000)
  cells_train_bg <- enmSdm::sampleRast(mosq_bias, n = select_trainBg, replace = F, prob = T) %>%
    as.data.frame()
  cells_eval_bg <- enmSdm::sampleRast(mosq_bias, n = select_evalBg, replace = F, prob = T) %>%
    as.data.frame()
  
  
  
  
  
  ### BACKGROUND COVARIATE EXTRACTION ###
  # Extract covariates for training points background
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

  
  # Extract covariates for evaluation points background
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
  
  
  
  ### DATA MERGING AND BINDING ###
  # Merge training and occurrences for both the training set and evaluation set
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Merging covariate info from occ/bg points for training/eval"))
  predictors_train_df <- rbind(train_occ, train_bg)
  predictors_eval_df <- rbind(eval_occ, eval_bg)
  
  
  # Bind the training and background datasets into one
  # Complete cases should remove nothing; this is a safety check to ensure that sdmData_raw = sdmData
  sdmData_raw <- rbind(predictors_train_df, predictors_eval_df)
  sdmData <- sdmData_raw[complete.cases(sdmData_raw), ] %>%  
    mutate(species = SpeciesOfInterest_Names[i])
  sdmData <- sdmData[,c(14,1:13)]
  print(paste0("SDM Data Raw: ", nrow(sdmData_raw), "; SDM Data: ", nrow(sdmData)))
  
  
  # Save the data for each species into a list, to compile into a single csv below
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
  
  
  
  ### SUMMARY STATISTICS ###
  # Save summary statistics by species
  print(paste0("[",SpeciesOfInterest_Names[i],"]: Saving summary statistics"))
  trainevalStats[[1]][[i]] <- SpeciesOfInterest_Names[i]
  trainevalStats[[2]][[i]] <- ActivitySeason_Type[[i]]
  trainevalStats[[3]][[i]] <- nrow(train_occ)
  trainevalStats[[4]][[i]] <- nrow(train_bg)
  trainevalStats[[5]][[i]] <- nrow(eval_occ)
  trainevalStats[[6]][[i]] <- nrow(eval_bg)
  
  
  # Save filter flowchart statistics by species
  filterStats[[1]][[i]] <- SpeciesOfInterest_Names[i]
  filterStats[[2]][[i]] <- ActivitySeason_Type[[i]]
  filterStats[[3]][[i]] <- cleaned_points
  filterStats[[4]][[i]] <- landmass_points
  filterStats[[5]][[i]] <- activity_season_points
  filterStats[[6]][[i]] <- sampling_range_points
  filterStats[[7]][[i]] <- unique_points

  
  # Print elapsed time for given species' raster data extraction
  toc <- Sys.time()
  toc - tic
  
}


### CONSOLIDATE DATAFRAMES FOR SAVING ###
# Create merged dataframes to output as .csv's
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


# Rename the columns of the dataframe and merge before exporting the .csv
# {PhotoASTM, PrecipASTM, and TAM} = TAM in the .csv
# {PhotoASTSD, PrecipASTSD, and TASD} = TASD in the .csv
colnames(df_yearRound) <- c("Species","Longitude","Latitude","DataSplit","Occ1_or_Bg0","ELEV","EVIM",
                                  "EVISD","FC","HPD","PDQ","PWQ","TAM","TASD")

colnames(df_photoSeason) <- c("Species","Longitude","Latitude","DataSplit","Occ1_or_Bg0","ELEV","EVIM",
                                  "EVISD","FC","HPD","PDQ","TAM","TASD","PWQ")
df_photoSeason <- df_photoSeason[,c(1:11,14,12:13)]

colnames(df_precipSeason) <- c("Species","Longitude","Latitude","DataSplit","Occ1_or_Bg0","ELEV","EVIM",
                                  "EVISD","FC","HPD","PDQ","TAM","TASD","PWQ")
df_precipSeason <- df_precipSeason[,c(1:11,14,12:13)]


df_final <- rbind(df_yearRound, df_photoSeason, df_precipSeason)



# Merge filterStats with filterStats_pre from the data cleaning script
filterStats_pre <- readRDS("filterStats_pre.RDS")
filterStats %<>% cbind(filterStats_pre[2:7]) %>%
  .[,c(1:2,8:13,3:7)]



### SAVE SDM DATA ###
# Output .csv files
write.csv(trainevalStats, file = "Train Eval Statistics by Species.csv", row.names=FALSE)
write.csv(filterStats, file = "Filter Statistics by Species.csv", row.names=FALSE)
write.csv(df_final, file = "SDM Data.csv", row.names=FALSE)
write.csv(df_yearRound, file = "SDM Data - Year Round.csv", row.names = FALSE)
write.csv(df_photoSeason, file = "SDM Data - Photoperiod Activity Season.csv", row.names = FALSE)
write.csv(df_precipSeason, file = "SDM Data - Precipitation Activity Season.csv", row.names = FALSE)



