#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Create a summed, stair-step stack of environmental covariates for checking NAs
#######################################################

source("E:/Documents/GitHub/mosquito-sdm/0-config.R")


#------------------------------------------------------
# Load in environmental predictors
#------------------------------------------------------
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
predictors_yearRound_check <- predictors_preStack[c(1:6,11:13)] %>% stack()
predictors_photoSeason_check <- predictors_preStack[c(1:6,11,7:8)] %>% stack()
predictors_precipSeason_check <- predictors_preStack[c(1:6,11,9:10)] %>% stack()


#------------------------------------------------------
# Read in raster sums
#------------------------------------------------------
predictor_sum_yearRound <- raster("Environmental Predictors Summed/Predictor_Sum_YearRound.tif")
predictor_sum_photoSeason <- raster("Environmental Predictors Summed/Predictor_Sum_PhotoSeason.tif")
predictor_sum_precipSeason <- raster("Environmental Predictors Summed/Predictor_Sum_PrecipSeason.tif")


#------------------------------------------------------
# Check which layers have most NAs by assigning each layer's NA an increasing stairstep value, 0 otherwise
#------------------------------------------------------
incrementer <- c(1,10,100,1000,10000,100000,1000000,10000000,100000000)

for(i in 1:9) {
  predictors_yearRound_check[[i]][is.na(predictors_yearRound_check[[i]])] <- incrementer[i]
  predictors_yearRound_check[[i]][!is.na(predictors_yearRound_check[[i]])] <- 0
  
  predictors_photoSeason_check[[i]][is.na(predictors_photoSeason_check[[i]])] <- incrementer[i]
  predictors_photoSeason_check[[i]][!is.na(predictors_photoSeason_check[[i]])] <- 0
  
  predictors_precipSeason_check[[i]][is.na(predictors_precipSeason_check[[i]])] <- incrementer[i]
  predictors_precipSeason_check[[i]][!is.na(predictors_precipSeason_check[[i]])] <- 0
}


#------------------------------------------------------
# Sum the raster stacks
#------------------------------------------------------
predictor_sum_yearRound_check <- sum(predictors_yearRound_check)
predictor_sum_photoSeason_check <- sum(predictors_photoSeason_check)
predictor_sum_precipSeason_check <- sum(predictors_precipSeason_check)


#------------------------------------------------------
# Save the raster sum checks
#------------------------------------------------------
writeRaster(predictor_sum_yearRound_check, filename = "Environmental Predictors Summed/Predictor_Sum_YearRound_Check.tif", format = "GTiff", overwrite=T)
writeRaster(predictor_sum_photoSeason_check, filename = "Environmental Predictors Summed/Predictor_Sum_PhotoSeason_Check.tif", format = "GTiff", overwrite=T)
writeRaster(predictor_sum_precipSeason_check, filename = "Environmental Predictors Summed/Predictor_Sum_PrecipSeason_Check.tif", format = "GTiff", overwrite=T)


