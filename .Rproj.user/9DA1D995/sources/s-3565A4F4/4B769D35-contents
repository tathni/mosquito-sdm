#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Sum the stack of environmental covariates by activity season
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
predictors_yearRound <- predictors_preStack[c(1:6,11:13)] %>% stack()
predictors_photoSeason <- predictors_preStack[c(1:6,11,7:8)] %>% stack()
predictors_precipSeason <- predictors_preStack[c(1:6,11,9:10)] %>% stack()


#------------------------------------------------------
# Create stacked raster sums
#------------------------------------------------------
predictor_sum_yearRound <- sum(predictors_yearRound)
predictor_sum_photoSeason <- sum(predictors_photoSeason)
predictor_sum_precipSeason <- sum(predictors_precipSeason)


#------------------------------------------------------
# Save raster sums
#------------------------------------------------------
writeRaster(predictor_sum_yearRound, filename = "Environmental Predictors Summed/Predictor_Sum_YearRound.tif", format = "GTiff", overwrite=T)
writeRaster(predictor_sum_photoSeason, filename = "Environmental Predictors Summed/Predictor_Sum_PhotoSeason.tif", format = "GTiff", overwrite=T)
writeRaster(predictor_sum_precipSeason, filename = "Environmental Predictors Summed/Predictor_Sum_PrecipSeason.tif", format = "GTiff", overwrite=T)


