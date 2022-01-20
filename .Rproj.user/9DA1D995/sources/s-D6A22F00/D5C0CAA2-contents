#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Merge all individual Google Earth Engine rasters
#######################################################

source("E:/Documents/GitHub/mosquito-sdm/0-config.R")
activity_ssn = FALSE  # Merging environmental predictors or activity season rasters?


#------------------------------------------------------
# Merge rasters by folders housing individual GEE sub-folders
#------------------------------------------------------
tic <- Sys.time()
# Place sub-folders that contain the individual, unmerged continental rasters into the temporary folder
# If the merged rasters aren't needed in the environment, this can be switch to an a_ply
inputPredictors <- alply(list.dirs("Environmental Raster Temporary", full.names = TRUE, recursive = FALSE), 1,
                         function(rast_folder){ 
                           print(rast_folder)
                           
                           # For every file in that folder, read in the raster
                           rasters <- alply(list.files(rast_folder,  full.names = TRUE), 1, function(rast_file){
                             print(rast_file)
                             raster::raster(rast_file)
                           })
                           
                           # Name the rasters to allow do.call to be run over them
                           names(rasters) <- c("x", "y")
                           
                           # Output should be a list with one merged raster for every directory of the raw environmental rasters
                           output <- do.call(raster::merge, rasters)
                           return(output)
                         })
toc <- Sys.time()
toc - tic



#------------------------------------------------------
# Processing for environmental predictor rasters
#------------------------------------------------------
if(activity_ssn == FALSE) {
  # rasterNames <- c("CD","EVIM","EVISD","FC","HPD","PDQ","PhotoASTM","PhotoASTSD","PrecipASTM","PrecipASTSD","PWQ","TAM","TASD","WS")
  rasterNames <-c("CD","WS")
  inputPredictors %<>% setNames(rasterNames) 
  
  # #------------------------------------------------------
  # # Crop the temperature rasters to landmasses and remove ocean background
  # #------------------------------------------------------
  # inputPredictors[[7]] <- raster::mask(inputPredictors[[7]], wrld_simpl) # PhotoASTM
  # inputPredictors[[8]] <- raster::mask(inputPredictors[[8]], wrld_simpl) # PhotoASTSD
  # inputPredictors[[9]] <- raster::mask(inputPredictors[[9]], wrld_simpl) # PrecipASTM
  # inputPredictors[[10]] <- raster::mask(inputPredictors[[10]], wrld_simpl) # PrecipASTSD
  # inputPredictors[[12]] <- raster::mask(inputPredictors[[12]], wrld_simpl) # TAM
  # inputPredictors[[13]] <- raster::mask(inputPredictors[[13]], wrld_simpl) # TASD
}



#------------------------------------------------------
# Processing for activity season rasters
#------------------------------------------------------
if(activity_ssn == TRUE) {
  rasterNames <- c("PhotoAS_LastDay","PhotoAS_Length","PhotoAS_StartDay","PrecipAS_LastDay","PrecipAS_Length","PrecipAS_StartDay")
  inputPredictors %<>% setNames(rasterNames)
  
  #------------------------------------------------------
  # Change Africa precipitation activity season's raster extents to global
  #------------------------------------------------------
  inputPredictors[[4]] %<>% raster::extend(inputPredictors[[1]], value=NA)
  inputPredictors[[5]] %<>% raster::extend(inputPredictors[[1]], value=NA)
  inputPredictors[[6]] %<>% raster::extend(inputPredictors[[1]], value=NA)
}



#------------------------------------------------------
# Save each individual merged covariate raster as a .tif file
#------------------------------------------------------
for (i in 1:length(inputPredictors)) {
  writeRaster(inputPredictors[[i]], filename = paste0("Environmental Predictors Merged/",rasterNames[i]), format = "GTiff")
}

