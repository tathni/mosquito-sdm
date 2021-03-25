# This script extracts and collates raster data for each of the 7 species of interest

# Setup
library(raster)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(dismo)
library(rgdal)
library(sf)
library(maptools)
library(beepr)
data(wrld_simpl)
setwd("E:/SynologyDrive/Tejas_Server/! Research/! Mordecai Lab/! Mosquito SDM MaxEnt Mechanistic/")
tic <- Sys.time()
seedNum <- 250


# Read in cleaned mosquito species occurrence data and country codes list
Mosquitoes_SpeciesOfInterest <- read.csv("GBIF Datasets Cleaned/Mosquitoes_SpeciesOfInterest_Cleaned.csv", header = TRUE,
                                         encoding = "UTF-8", stringsAsFactors = FALSE)

countryCodes <- read.csv("Country Codes/country-and-continent-codes.csv", sep = ",", header = TRUE,
                         encoding = "UTF-8", stringsAsFactors = FALSE)


# Assign continent names to both cleaned datasets
Mosquitoes_SpeciesOfInterest <- Mosquitoes_SpeciesOfInterest %>% 
  mutate(continent = countryCodes$Continent_Name[match(Mosquitoes_SpeciesOfInterest$countryCode, countryCodes$Two_Letter_Country_Code)])


# List of species of interest
SpeciesOfInterest_Names <- c("Aedes aegypti",
                             "Aedes albopictus",
                             "Anopheles gambiae",
                             "Anopheles stephensi",
                             "Culex pipiens",
                             "Culex quinquefasciatus",
                             "Culex tarsalis")
ActivitySeason_Type <- c("None- Year Round",
                         "Photoperiod",
                         "Precipitation",
                         "None- Year Round",
                         "Photoperiod",
                         "None- Year Round",
                         "Photoperiod")


# Read in and name the environmental predictors
predictors_preStack <- alply(list.files("Environmental Predictors Merged",
                               pattern = ".tif",
                               full.names = TRUE), 1, function(file){
  print(file)
  rast <- raster(file)
  return(rast)
})

rasterNames <- c("ELEV","EVIM","EVISD","FC","HP","PDQ","PhotoASTM","PhotoASTSD","PrecipASTM","PrecipASTSD","PWQ","TAM","TASD")
predictors_preStack <- setNames(predictors_preStack, rasterNames)


# Compress predictors into a stacked raster for each activity season combination
predictors_yearRound <- predictors_preStack[c(1:6,11:13)] %>% stack()
predictors_photoSeason <- predictors_preStack[c(1:8,11)] %>% stack()
predictors_precipSeason <- predictors_preStack[c(1:6,9:11)] %>% stack()


predictor_sum_yearRound <- raster("Predictor_Sum_YearRound.tif")
predictor_sum_photoSeason <- raster("Predictor_Sum_PhotoSeason.tif")
predictor_sum_precipSeason <- raster("Predictor_Sum_PrecipSeason.tif")


# predictor_sum_yearRound <- sum(predictors_yearRound) %>%
#   reclassify(cbind(-Inf, 0, NA), right=T)
# 
# predictor_sum_photoSeason <- sum(predictors_photoSeason) %>%
#   reclassify(cbind(-Inf, 0, NA), right=T)
# 
# predictor_sum_precipSeason <- sum(predictors_precipSeason) %>%
#   reclassify(cbind(-Inf, 0, NA), right=T)

# writeRaster(predictor_sum_yearRound, filename = "Predictor_Sum_YearRound.tif", format = "GTiff", overwrite=T)
# writeRaster(predictor_sum_photoSeason, filename = "Predictor_Sum_PhotoSeason.tif", format = "GTiff", overwrite=T)
# writeRaster(predictor_sum_precipSeason, filename = "Predictor_Sum_PrecipSeason.tif", format = "GTiff", overwrite=T)




# # Create background sampling maps based on species of interest's continent
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
# PipiensOnly <- rbind(NorthAmerica, Europe_noRussia, Africa,
#                      wrld_simpl[wrld_simpl$NAME %in% "Japan", ],
#                      wrld_simpl[wrld_simpl$NAME == "Australia", ],
#                      wrld_simpl[wrld_simpl$NAME == "South Korea", ])
# 
# AedesAegypti_map <- rbind(NorthAmerica, SouthAmerica, Africa, Asia, Oceania)
# AedesAlbopictus_map <- rbind(NorthAmerica, SouthAmerica, Europe_noRussia, Africa, Asia)
# AnophelesGambiae_map <- Africa
# AnophelesStephensi_map <- SouthAsia
# CulexPipiens_map <- PipiensOnly
# CulexQuinquefasciatus_map <- rbind(NorthAmerica, SouthAmerica, Oceania, Asia)
# CulexTarsalis_map <- NorthAmerica
# 
# 
# # Create a cropped, summed predictor raster for each species of interest only within continent(s)/region(s) of occurrence
# map_1 <- crop(predictor_sum_yearRound, extent(AedesAegypti_map)) %>%
#   mask(AedesAegypti_map)
# 
# map_2 <- crop(predictor_sum_photoSeason, extent(AedesAlbopictus_map)) %>%
#   mask(AedesAlbopictus_map)
# 
# map_3 <- crop(predictor_sum_precipSeason, extent(AnophelesGambiae_map)) %>%
#   mask(AnophelesGambiae_map)
# 
# map_4 <- crop(predictor_sum_yearRound, extent(AnophelesStephensi_map)) %>%
#   mask(AnophelesStephensi_map)
# 
# map_5 <- crop(predictor_sum_photoSeason, extent(CulexPipiens_map)) %>%
#   mask(CulexPipiens_map)
# 
# map_6 <- crop(predictor_sum_yearRound, extent(CulexQuinquefasciatus_map)) %>%
#   mask(CulexQuinquefasciatus_map)
# 
# map_7 <- crop(predictor_sum_photoSeason, extent(CulexTarsalis_map)) %>%
#   mask(CulexTarsalis_map)
# 
# 
# # Save cropped rasters for future use
# writeRaster(map_1, filename = "AeAegypti_SamplingMap.tif", format = "GTiff", overwrite=T)
# writeRaster(map_2, filename = "AeAlbopictus_SamplingMap.tif", format = "GTiff", overwrite=T)
# writeRaster(map_3, filename = "AnGambiae_SamplingMap.tif", format = "GTiff", overwrite=T)
# writeRaster(map_4, filename = "AnStephensi_SamplingMap.tif", format = "GTiff", overwrite=T)
# writeRaster(map_5, filename = "CxPipiens_SamplingMap.tif", format = "GTiff", overwrite=T)
# writeRaster(map_6, filename = "CxQuinquefasciatus_SamplingMap.tif", format = "GTiff", overwrite=T)
# writeRaster(map_7, filename = "CxTarsalis_SamplingMap.tif", format = "GTiff", overwrite=T)


# Read in the cropped raster sampling maps
sampling_maps <- alply(list.files("Sampling Maps",
                                   pattern = ".tif",
                                   full.names = TRUE), 1, function(file){
                                     print(file)
                                     rast <- raster(file)
                                     return(rast)
                                   })


# Compile necessary lists needed in the SDM for loop
speciesList <- c("AedesAegypti",
                  "AedesAlbopictus",
                  "AnophelesGambiae",
                  "AnophelesStephensi",
                  "CulexPipiens",
                  "CulexQuinquefasciatus",
                  "CulexTarsalis")

trainingList <- c("AedesAegypti_Training",
                  "AedesAlbopictus_Training",
                  "AnophelesGambiae_Training",
                  "AnophelesStephensi_Training",
                  "CulexPipiens_Training",
                  "CulexQuinquefasciatus_Training",
                  "CulexTarsalis_Training")

evaluationList <- c("AedesAegypti_Evaluation",
                     "AedesAlbopictus_Evaluation",
                     "AnophelesGambiae_Evaluation",
                     "AnophelesStephensi_Evaluation",
                     "CulexPipiens_Evaluation",
                     "CulexQuinquefasciatus_Evaluation",
                     "CulexTarsalis_Evaluation")


summaryStats <- data.frame(matrix(ncol = 6, nrow=7))
colnames(summaryStats) <- c("Species","Activity_Season_Restriction","Training_Occ","Training_Bg","Evaluation_Occ","Evaluation_Bg")

filterStats <- data.frame(matrix(ncol = 7, nrow=7))
colnames(filterStats) <- c("Species","Activity_Season_Restriction","Raw_GBIF_Occurrences","Landmass_Points","Activity_Season_Points",
                           "Sampling_Frame_Points","Unique_Points_Final")

sdmData_yearRound <- list()
sdmData_photoSeason <- list()
sdmData_precipSeason <- list()

counter_YR <- 1
counter_PhS <- 1
counter_PrS <- 1


# Loop through each species
for (i in 1:length(SpeciesOfInterest_Names)) { 
  print(paste0("Species of interest is ", SpeciesOfInterest_Names[i]))
  
  # Set predictor stack according to specific activity season setting
  if(SpeciesOfInterest_Names[i] == "Aedes aegypti" |
     SpeciesOfInterest_Names[i] == "Anopheles stephensi" |
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
 
  
  # Assign 80% from each species of interest without replacement as training data
  # Set aside 20% for evaluation
  species_df <- assign(speciesList[i], filter(Mosquitoes_SpeciesOfInterest, species == SpeciesOfInterest_Names[i]))
  occGPS_raw <- dplyr::select(species_df, c(species, decimalLongitude, decimalLatitude)) %>% 
    unique
  raw_gbif_points <- nrow(occGPS_raw)
  
  q <- round(nrow(occGPS_raw) * 0.8)
  set.seed(seedNum)
  occGPS_train <- assign(trainingList[i], occGPS_raw[sample(nrow(occGPS_raw), q), ])
  occGPS_eval <- assign(evaluationList[i], setdiff(occGPS_raw, occGPS_train))
  
  
  # Isolate long/lat coordinates of occurrence points
  occGPS_train %<>% dplyr::select(decimalLongitude, decimalLatitude)
  occGPS_eval %<>% dplyr::select(decimalLongitude, decimalLatitude)
  
  
  
  
  # Restrict occurrence points to acquire landmass-only points and exclude points in the ocean
  print(paste0("Isolating training and evaluation cells, landmass restricted, for ", SpeciesOfInterest_Names[i]))
  remove_df <- c(which(is.na(raster::extract(predictorSum_world, occGPS_train))))
  if(length(remove_df) > 0) {
    occGPS_train_noNA <- occGPS_train[-remove_df,]
  } else {
    occGPS_train_noNA <- occGPS_train
  }
  
  remove_df <- c(which(is.na(raster::extract(predictorSum_world, occGPS_eval))))
  if(length(remove_df) > 0) {
    occGPS_eval_noNA <- occGPS_eval[-remove_df,] 
  } else {
    occGPS_eval_noNA <- occGPS_eval
  }
  
  landmass_points <- sum(nrow(occGPS_train_noNA), nrow(occGPS_eval_noNA))
  
  
  # Restrict landmass points to acquire points within the given species' activity season configuration
  print(paste0("Isolating training and evaluation cells, activity season restricted, for ", SpeciesOfInterest_Names[i]))
  remove_df <- c(which(is.na(raster::extract(predictorSum, occGPS_train_noNA))))
  if(length(remove_df) > 0) {
    occGPS_train_activity_noNA <- occGPS_train_noNA[-remove_df,]
  } else {
    occGPS_train_activity_noNA <- occGPS_train_noNA
  }
  
  remove_df <- c(which(is.na(raster::extract(predictorSum, occGPS_eval_noNA))))
  if(length(remove_df) > 0) {
    occGPS_eval_activity_noNA <- occGPS_eval_noNA[-remove_df,] 
  } else {
    occGPS_eval_activity_noNA <- occGPS_eval_noNA
  }
  
  activity_season_points <- sum(nrow(occGPS_train_activity_noNA), nrow(occGPS_eval_activity_noNA))
  
  
  # Restrict activity season points to acquire points within the given species' background sampling frame
  print(paste0("Isolating training and evaluation cells, sampling frame restricted, for ", SpeciesOfInterest_Names[i]))
  remove_df <- c(which(is.na(raster::extract(sampling_maps[[i]], occGPS_train_activity_noNA))))
  if(length(remove_df) > 0) {
    occGPS_train_sampframe_noNA <- occGPS_train_activity_noNA[-remove_df,]
  } else {
    occGPS_train_sampframe_noNA <- occGPS_train_activity_noNA
  }
  
  remove_df <- c(which(is.na(raster::extract(sampling_maps[[i]], occGPS_eval_activity_noNA))))
  if(length(remove_df) > 0) {
    occGPS_eval_sampframe_noNA <- occGPS_eval_activity_noNA[-remove_df,] 
  } else {
    occGPS_eval_sampframe_noNA <- occGPS_eval_activity_noNA
  }
  
  sampling_frame_points <- sum(nrow(occGPS_train_sampframe_noNA), nrow(occGPS_eval_sampframe_noNA))
  
  
  # Restrict occurrences to unique, non-NA cells of 1km x 1km, and then back-acquire the centroid (x,y) of the cells
  cells_train <- xyFromCell(predictors, cellFromXY(predictors, occGPS_train_sampframe_noNA) %>% 
                              unique) %>% as.data.frame()
  colnames(cells_train) <- c("decimalLongitude","decimalLatitude")
  
  cells_eval <- xyFromCell(predictors, cellFromXY(predictors, occGPS_eval_sampframe_noNA) %>% 
                             unique) %>% as.data.frame()
  colnames(cells_eval) <- c("decimalLongitude","decimalLatitude")
  
  unique_points <- sum(nrow(cells_train), nrow(cells_eval))
  
  
  
  
  # Random sample background points from the region(s) on which the species of interest resides
  print(paste0("Sampling background cells for ", SpeciesOfInterest_Names[i]))
  sampleNum <- 100000 # Over-sample to account for the NA's
  select_trainBg <- nrow(cells_train) # Same number of background points as occurrence points
  select_evalBg <- nrow(cells_eval)
  
  
  # Select background points for training
  set.seed(seedNum)
  cells_train_bg_raw <- randomPoints(sampling_maps[[i]], sampleNum, p = cells_train, excludep = T, prob = F)
  cells_train_bg_predictors <- cbind(data.frame(raster::extract(predictors, cells_train_bg_raw)),
                                     cells_train_bg_raw) %>%
    .[complete.cases(.), ] %>%
    .[sample(nrow(.), select_trainBg),]  # Workflow for selecting non-NAs since the randomPoints() function does not drop NA's
  cells_train_bg <- cells_train_bg_predictors[,c(10:11)]
  

  colnames(cells_train_bg) <- c("decimalLatitude","decimalLongitude")
  cells_train_bg <- cells_train_bg[, c("decimalLongitude","decimalLatitude")]
  
  
  # Select background points for evaluation
  set.seed(seedNum)
  cells_eval_bg_raw <- randomPoints(sampling_maps[[i]], sampleNum, p = cells_eval, excludep = T, prob = F)
  cells_eval_bg_predictors <- cbind(data.frame(raster::extract(predictors, cells_eval_bg_raw)),
                                    cells_eval_bg_raw) %>%
    .[complete.cases(.), ] %>%
    .[sample(nrow(.), select_evalBg),]
  cells_eval_bg <- cells_eval_bg_predictors[,c(10:11)]
  
  colnames(cells_eval_bg) <- c("decimalLatitude","decimalLongitude")
  cells_eval_bg <- cells_eval_bg[, c("decimalLongitude","decimalLatitude")]
  
  print(paste0("Training_Occ: ", nrow(cells_train), "; Training_Bg: ", nrow(cells_train_bg),
               "; Evaluation_Occ: ",nrow(cells_eval), "; Evaluation_Bg: ", nrow(cells_eval_bg)))
  
  
  
  
  # Extract covariates for training points: occurrence and background
  print(paste0("Extracting covariate info from training points for ", SpeciesOfInterest_Names[i]))
  
  train_occ <- cbind(c(rep(1, nrow(cells_train))),
                     data.frame(raster::extract(predictors, cells_train)),
                     cells_train,
                     c(rep("Training",nrow(cells_train))))
  colnames(train_occ)[1] <- "Occ1_or_Bg0"
  colnames(train_occ)[13] <- "dataSplit"
  
  train_bg <- cbind(c(rep(0, nrow(cells_train_bg))),
                    data.frame(raster::extract(predictors, cells_train_bg)),
                    cells_train_bg,
                    c(rep("Training",nrow(cells_train_bg))))
  colnames(train_bg)[1] <- "Occ1_or_Bg0"
  colnames(train_bg)[13] <- "dataSplit"
  
  predictors_train_df <- rbind(train_occ, train_bg)
  
  
  
  # Extract covariates for evaluation points: occurrence and background
  print(paste0("Extracting covariate info from evaluation points for ", SpeciesOfInterest_Names[i]))
  
  eval_occ <- cbind(c(rep(1, nrow(cells_eval))),
                    data.frame(raster::extract(predictors, cells_eval)),
                    cells_eval,
                    c(rep("Evaluation",nrow(cells_eval))))
  colnames(eval_occ)[1] <- "Occ1_or_Bg0"
  colnames(eval_occ)[13] <- "dataSplit"
  
  eval_bg <- cbind(c(rep(0, nrow(cells_eval_bg))),
                    data.frame(raster::extract(predictors, cells_eval_bg)),
                   cells_eval_bg,
                    c(rep("Evaluation",nrow(cells_eval_bg))))
  colnames(eval_bg)[1] <- "Occ1_or_Bg0"
  colnames(eval_bg)[13] <- "dataSplit"
                               
  predictors_eval_df <- rbind(eval_occ, eval_bg)
  
  
  
  # Bind occurrences and background, allocate 1 to occ and 0 to bg, drop NA's
  # Complete cases should remove nothing; this is a safety check to ensure that sdmData_raw = sdmData
  sdmData_raw <- rbind(predictors_train_df, predictors_eval_df)
  sdmData <- sdmData_raw[complete.cases(sdmData_raw), ] %>%  
    mutate(species = SpeciesOfInterest_Names[i])
  
  
  # Save the data for each species into a list, to compile into a single csv below
  if(SpeciesOfInterest_Names[i] == "Aedes aegypti" |
     SpeciesOfInterest_Names[i] == "Anopheles stephensi" |
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
  
  
  
  # Save summary statistics by species
  summaryStats[[1]][[i]] <- SpeciesOfInterest_Names[i]
  summaryStats[[2]][[i]] <- ActivitySeason_Type[[i]]
  summaryStats[[3]][[i]] <- nrow(cells_train)
  summaryStats[[4]][[i]] <- nrow(cells_train_bg)
  summaryStats[[5]][[i]] <- nrow(cells_eval)
  summaryStats[[6]][[i]] <- nrow(cells_eval_bg)
  
  
  # Save filter flowchart statistics by species
  filterStats[[1]][[i]] <- SpeciesOfInterest_Names[i]
  filterStats[[2]][[i]] <- ActivitySeason_Type[[i]]
  filterStats[[3]][[i]] <- raw_gbif_points
  filterStats[[4]][[i]] <- landmass_points
  filterStats[[5]][[i]] <- activity_season_points
  filterStats[[6]][[i]] <- sampling_frame_points
  filterStats[[7]][[i]] <- unique_points
}



# Create consolidated dataframes to output as .csv's
df_yearRound <- sdmData_yearRound[[1]][FALSE, ]
df_photoSeason <- sdmData_photoSeason[[1]][FALSE, ]
df_precipSeason <- sdmData_precipSeason[[1]][FALSE, ]

counter_YR <- 1
counter_PhS <- 1
counter_PrS <- 1


for (i in 1:length(SpeciesOfInterest_Names)) {
  if(SpeciesOfInterest_Names[i] == "Aedes aegypti" |
     SpeciesOfInterest_Names[i] == "Anopheles stephensi" |
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


# Re-order the columns of the dataframe before exporting the .csv
df_yearRound <- df_yearRound[, c(14,11:12,13,1:10)]
df_photoSeason <- df_photoSeason[, c(14,11:12,13,1:7,10,8:9)]
df_precipSeason <- df_precipSeason[, c(14,11:12,13,1:7,10,8:9)]

df_yearRound_merge <- df_yearRound
colnames(df_yearRound_merge) <- c("Species","Longitude","Latitude","DataSplit","Occ1_or_Bg0","ELEV","EVIM",
                                  "EVISD","FC","HP","PDQ","PWQ","TAM","TASD")

df_photoSeason_merge <- df_photoSeason
colnames(df_photoSeason_merge) <- c("Species","Longitude","Latitude","DataSplit","Occ1_or_Bg0","ELEV","EVIM",
                                  "EVISD","FC","HP","PDQ","PWQ","TAM","TASD")

df_precipSeason_merge <- df_precipSeason
colnames(df_precipSeason_merge) <- c("Species","Longitude","Latitude","DataSplit","Occ1_or_Bg0","ELEV","EVIM",
                                  "EVISD","FC","HP","PDQ","PWQ","TAM","TASD")

df_final <- rbind(df_yearRound_merge, df_photoSeason_merge, df_precipSeason_merge)




# Output .csv files
write.csv(summaryStats, file = "Summary Statistics by Species.csv", row.names=FALSE)
write.csv(filterStats, file = "Filter Statistics by Species.csv", row.names=FALSE)
write.csv(df_final, file = "SDM Data.csv", row.names=FALSE)
write.csv(df_yearRound, file = "SDM Data - Year Round.csv", row.names = FALSE)
write.csv(df_photoSeason, file = "SDM Data - Photoperiod Activity Season.csv", row.names = FALSE)
write.csv(df_precipSeason, file = "SDM Data - Precipitation Activity Season.csv", row.names = FALSE)


# Print elapsed time for cumulative raster data extraction
toc <- Sys.time()
toc - tic
beep(3)



