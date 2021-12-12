#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Merge all of the individual Google Earth Engine-derived environmental covariates
#######################################################

source("E:/Documents/GitHub/mosquito-sdm/0-config.R")
setwd("E:/SynologyDrive/Tejas_Server/! Research/! Mordecai Lab/! Mosquito SDM Thermal Dependence/")


#------------------------------------------------------
# Merge rasters by folders housing individual GEE sub-folders
#------------------------------------------------------
tic <- Sys.time()
# Place sub-folders of individual, unmerged continental rasters in the temporary folder
# If the merged rasters aren't needed in the environment, this can be switch to an a_ply
inputPredictors <- alply(list.dirs("Environmental Raster Temporary", full.names = TRUE, recursive = FALSE), 1,
                        function(rast_folder){ 
                          print(rast_folder)
                          
                          # For every file in that folder, read in the file list-wise, then rasterize and reproject
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
# Name the environmental covariates in the list
#------------------------------------------------------
rasterNames <- c("ELEV","EVIM","EVISD","FC","HPD","PDQ","PhotoASTM","PhotoASTSD","PrecipASTM","PrecipASTSD","PWQ","TAM","TASD")
 


#------------------------------------------------------
# Crop only the temperature rasters to landmasses and remove ocean background
#------------------------------------------------------
inputPredictors[[7]] <- raster::mask(inputPredictors[[7]], wrld_simpl) # PhotoASTM
inputPredictors[[8]] <- raster::mask(inputPredictors[[8]], wrld_simpl) # PhotoASTSD
inputPredictors[[9]] <- raster::mask(inputPredictors[[9]], wrld_simpl) # PrecipASTM
inputPredictors[[10]] <- raster::mask(inputPredictors[[10]], wrld_simpl) # PrecipASTSD
inputPredictors[[12]] <- raster::mask(inputPredictors[[12]], wrld_simpl) # TAM
inputPredictors[[13]] <- raster::mask(inputPredictors[[13]], wrld_simpl) # TASD



#------------------------------------------------------
# Save each individual merged covariate raster as a .tif file
#------------------------------------------------------
for (i in 1:length(inputPredictors)) {
  writeRaster(inputPredictors[[i]], filename = rasterNames[i], format = "GTiff")
}

