#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Create filter masks for landmass and predictor sum non-NA
#######################################################

source("E:/Documents/GitHub/mosquito-sdm/0-config.R")


#------------------------------------------------------
## DATA LOAD-IN ##
#------------------------------------------------------
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
rasterNames <- c("CD","EVIM","EVISD","FC","HPD","PDQ","PhotoASTM","PhotoASTSD","PrecipASTM","PrecipASTSD","PWQ","SW","TAM","TASD","WS")
predictors_preStack <- setNames(predictors_preStack, rasterNames)



#------------------------------------------------------
## LANDMASS MASK ##
#------------------------------------------------------
#------------------------------------------------------
# Choose a raster with known global completeness and cut to continents (PDQ)
#------------------------------------------------------
landmass_mask <- predictors_preStack[[6]]

#------------------------------------------------------
# Set all cells with values to 1, and keep NAs
#------------------------------------------------------
landmass_mask[!is.na(landmass_mask)] <- 1
writeRaster(landmass_mask, filename = "Filter Masks/Landmass/Landmass.tif")



#------------------------------------------------------
## PREDICTOR SUM NON-NA MASK ##
#------------------------------------------------------
#------------------------------------------------------
# Compress predictors into a stacked raster for each activity season combination
#------------------------------------------------------
predictors_yearRound <- predictors_preStack[c(1:6,11:12,15,13:14)] %>% stack()
predictors_photoSeason <- predictors_preStack[c(1:6,11:12,15,7:8)] %>% stack()
predictors_precipSeason <- predictors_preStack[c(1:6,11:12,15,9:10)] %>% stack()

#------------------------------------------------------
# Create stacked raster sums
#------------------------------------------------------
predictor_sum_yearRound <- raster::calc(predictors_yearRound, fun=sum, na.rm=FALSE)
writeRaster(predictor_sum_yearRound, filename = "Filter Masks/Predictor Sum Non-NA/Predictor_Sum_YearRound.tif")

predictor_sum_photoSeason <- raster::calc(predictors_photoSeason, fun=sum, na.rm=FALSE)
writeRaster(predictor_sum_photoSeason, filename = "Filter Masks/Predictor Sum Non-NA/Predictor_Sum_PhotoSeason.tif")

predictor_sum_precipSeason <- raster::calc(predictors_precipSeason, fun=sum, na.rm=FALSE)
writeRaster(predictor_sum_precipSeason, filename = "Filter Masks/Predictor Sum Non-NA/Predictor_Sum_PrecipSeason.tif")



