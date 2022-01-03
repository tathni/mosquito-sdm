#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Produce the sampling maps and bias masks
#######################################################

source("E:/Documents/GitHub/mosquito-sdm/0-config.R")


#------------------------------------------------------
# Create stacked raster sums
#------------------------------------------------------
predictor_sum_yearRound <- sum(predictors_yearRound) %>%
  reclassify(cbind(-Inf, 0, NA), right=T)

predictor_sum_photoSeason <- sum(predictors_photoSeason) %>%
  reclassify(cbind(-Inf, 0, NA), right=T)

predictor_sum_precipSeason <- sum(predictors_precipSeason) %>%
  reclassify(cbind(-Inf, 0, NA), right=T)

writeRaster(predictor_sum_yearRound, filename = "Predictor_Sum_YearRound.tif", format = "GTiff", overwrite=T)
writeRaster(predictor_sum_photoSeason, filename = "Predictor_Sum_PhotoSeason.tif", format = "GTiff", overwrite=T)
writeRaster(predictor_sum_precipSeason, filename = "Predictor_Sum_PrecipSeason.tif", format = "GTiff", overwrite=T)


#------------------------------------------------------
# Create background sampling maps
#------------------------------------------------------
predictor_sum_yearRound <- raster("Predictor_Sum_YearRound.tif")
predictor_sum_photoSeason <- raster("Predictor_Sum_PhotoSeason.tif")
predictor_sum_precipSeason <- raster("Predictor_Sum_PrecipSeason.tif")

SouthAmerica_list <- c("Colombia", "Venezuela", "Suriname", "Guyana", "French Guiana",
                       "Ecuador", "Peru", "Bolivia", "Chile", "Argentina", "Uruguay",
                       "Paraguay", "Brazil", "Falkland Islands (Malvinas)")
SouthAmerica <- wrld_simpl[wrld_simpl$NAME %in% SouthAmerica_list, ]
NorthAmerica <- wrld_simpl[wrld_simpl$REGION==19,]
NorthAmerica <- NorthAmerica[!NorthAmerica$NAME %in% SouthAmerica_list, ]
Africa <- wrld_simpl[wrld_simpl$REGION==2,]
Oceania <- wrld_simpl[wrld_simpl$REGION==9,]
Europe <- wrld_simpl[wrld_simpl$REGION==150,]
Europe_noRussia <- wrld_simpl[wrld_simpl$REGION==150 & !wrld_simpl$NAME == "Russia", ]
Asia <- wrld_simpl[wrld_simpl$REGION==142,]
SouthAsia_list <- c("India","Pakistan","Nepal","Bangladesh","Sri Lanka", "Bhutan")
SouthAsia <- wrld_simpl[wrld_simpl$NAME %in% SouthAsia_list, ]
PipiensOnly <- rbind(NorthAmerica, Europe_noRussia,
                     wrld_simpl[wrld_simpl$NAME %in% "Japan", ],
                     wrld_simpl[wrld_simpl$NAME == "Australia", ])

AedesAegypti_map <- rbind(NorthAmerica, SouthAmerica, Africa, Asia, Oceania)
AedesAlbopictus_map <- rbind(NorthAmerica, SouthAmerica, Europe_noRussia, Africa, Asia)
AnophelesGambiae_map <- Africa
AnophelesStephensi_map <- SouthAsia
CulexAnnulirostris_map <- Oceania
CulexPipiens_map <- PipiensOnly
CulexQuinquefasciatus_map <- rbind(NorthAmerica, Oceania, Asia)
CulexTarsalis_map <- NorthAmerica


#------------------------------------------------------
# Create cropped, summed predictor raster for each species of interest only within regions/continents of occurrence
#------------------------------------------------------
map_1 <- crop(predictor_sum_yearRound, extent(AedesAegypti_map)) %>%
  mask(AedesAegypti_map)
map_2 <- crop(predictor_sum_photoSeason, extent(AedesAlbopictus_map)) %>%
  mask(AedesAlbopictus_map)
map_3 <- crop(predictor_sum_precipSeason, extent(AnophelesGambiae_map)) %>%
  mask(AnophelesGambiae_map)
map_4 <- crop(predictor_sum_yearRound, extent(AnophelesStephensi_map)) %>%
  mask(AnophelesStephensi_map)
map_5 <- crop(predictor_sum_yearRound, extent(CulexAnnulirostris_map)) %>%
  mask(CulexAnnulirostris_map)
map_6 <- crop(predictor_sum_photoSeason, extent(CulexPipiens_map)) %>%
  mask(CulexPipiens_map)
map_7 <- crop(predictor_sum_yearRound, extent(CulexQuinquefasciatus_map)) %>%
  mask(CulexQuinquefasciatus_map)
map_8 <- crop(predictor_sum_photoSeason, extent(CulexTarsalis_map)) %>%
  mask(CulexTarsalis_map)

writeRaster(map_1, filename = "Sampling Range Maps/AeAegypti_SamplingRange.tif", format = "GTiff", overwrite=T)
writeRaster(map_2, filename = "Sampling Range Maps/AeAlbopictus_SamplingRange.tif", format = "GTiff", overwrite=T)
writeRaster(map_3, filename = "Sampling Range Maps/AnGambiae_SamplingRange.tif", format = "GTiff", overwrite=T)
writeRaster(map_4, filename = "Sampling Range Maps/AnStephensi_SamplingRange.tif", format = "GTiff", overwrite=T)
writeRaster(map_5, filename = "Sampling Range Maps/CxAnnulirostris_SamplingRange.tif", format = "GTiff", overwrite=T)
writeRaster(map_6, filename = "Sampling Range Maps/CxPipiens_SamplingRange.tif", format = "GTiff", overwrite=T)
writeRaster(map_7, filename = "Sampling Range Maps/CxQuinquefasciatus_SamplingRange.tif", format = "GTiff", overwrite=T)
writeRaster(map_8, filename = "Sampling Range Maps/CxTarsalis_SamplingRange.tif", format = "GTiff", overwrite=T)


#------------------------------------------------------
# Load in the sampling range maps
#------------------------------------------------------
sampling_ranges <- alply(list.files("Sampling Range Maps",
                                    pattern = ".tif",
                                    full.names = TRUE), 1, function(file){
                                      print(file)
                                      rast <- raster(file)
                                      return(rast)
                                    })


#------------------------------------------------------
# Create bias masks for sampling
#------------------------------------------------------
biasMask_1 <- sampling_ranges[[1]]
biasMask_1[!is.na(biasMask_1)] <- 0
biasMask_2 <- sampling_ranges[[2]]
biasMask_2[!is.na(biasMask_2)] <- 0
biasMask_3 <- sampling_ranges[[3]]
biasMask_3[!is.na(biasMask_3)] <- 0
biasMask_4 <- sampling_ranges[[4]]
biasMask_4[!is.na(biasMask_4)] <- 0
biasMask_5 <- sampling_ranges[[5]]
biasMask_5[!is.na(biasMask_5)] <- 0
biasMask_6 <- sampling_ranges[[6]]
biasMask_6[!is.na(biasMask_6)] <- 0
biasMask_7 <- sampling_ranges[[7]]
biasMask_7[!is.na(biasMask_7)] <- 0
biasMask_8 <- sampling_ranges[[8]]
biasMask_8[!is.na(biasMask_8)] <- 0

writeRaster(biasMask_1, filename = "Bias Masks/AeAegypti_BiasMask.tif", format = "GTiff", overwrite=T)
writeRaster(biasMask_2, filename = "Bias Masks/AeAlbopictus_BiasMask.tif", format = "GTiff", overwrite=T)
writeRaster(biasMask_3, filename = "Bias Masks/AnGambiae_BiasMask.tif", format = "GTiff", overwrite=T)
writeRaster(biasMask_4, filename = "Bias Masks/AnStephensi_BiasMask.tif", format = "GTiff", overwrite=T)
writeRaster(biasMask_5, filename = "Bias Masks/CxAnnulirostris_BiasMask.tif", format = "GTiff", overwrite=T)
writeRaster(biasMask_6, filename = "Bias Masks/CxPipiens_BiasMask.tif", format = "GTiff", overwrite=T)
writeRaster(biasMask_7, filename = "Bias Masks/CxQuinquefasciatus_BiasMask.tif", format = "GTiff", overwrite=T)
writeRaster(biasMask_8, filename = "Bias Masks/CxTarsalis_BiasMask.tif", format = "GTiff", overwrite=T)


