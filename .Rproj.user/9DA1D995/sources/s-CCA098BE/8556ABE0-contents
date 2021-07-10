# This script merges all of the environmental covariates for use in the SDM

# Setup
library(raster)
library(plyr)
library(sp)
library(maptools)
setwd("E:/SynologyDrive/Tejas_Server/! Research/! Mordecai Lab/! Mosquito SDM MaxEnt Mechanistic/")
tic <- Sys.time()

# Apply a function to every different folder name in the Environmental Raster Data folder
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


# Name the environmental covariates in the list
# rasterNames <- c("ELEV","EVIM","EVISD","FC","HPD","PDQ","PhotoASTM","PhotoASTR","PrecipASTM","PrecipASTR","PWQ")
rasterNames <- c("PhotoASTM","PhotoASTSD","PrecipASTM","PrecipASTSD","TAM","TASD")
 
# Cropping certain rasters to landmasses only and removing ocean background
data(wrld_simpl)
inputPredictors[[1]] <- raster::mask(inputPredictors[[1]], wrld_simpl) # PhotoASTM
inputPredictors[[2]] <- raster::mask(inputPredictors[[2]], wrld_simpl) # PhotoASTSD
inputPredictors[[3]] <- raster::mask(inputPredictors[[3]], wrld_simpl) # PrecipASTM
inputPredictors[[4]] <- raster::mask(inputPredictors[[4]], wrld_simpl) # PrecipASTSD
inputPredictors[[5]] <- raster::mask(inputPredictors[[5]], wrld_simpl) # TAM
inputPredictors[[6]] <- raster::mask(inputPredictors[[6]], wrld_simpl) # TASD


# Save each individual merged covariate raster as a .tif file
for (i in 1:length(inputPredictors)) {
  writeRaster(inputPredictors[[i]], filename = rasterNames[i], format = "GTiff")
}

