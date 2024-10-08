#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Merge all individual Google Earth Engine rasters
#######################################################

source("C:/Users/tejas/Documents/GitHub/mosquito-sdm/0-config.R")
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
  rasterNames <- c("CD","EVIM","EVISD","FC","HPD","PDQ","PhotoASTM","PhotoASTSD","PrecipASTM","PrecipASTSD","PWQ","SW","TAM","TASD","WS")
  inputPredictors %<>% setNames(rasterNames) 
  
  #------------------------------------------------------
  # Mask temperature rasters to landmass-only, using a complete raster known to be cut to continents
  #------------------------------------------------------
  inputPredictors[[7]] %<>% raster::mask(inputPredictors[[6]])
  inputPredictors[[8]] %<>% raster::mask(inputPredictors[[6]])
  inputPredictors[[9]] %<>% raster::mask(inputPredictors[[6]])
  inputPredictors[[10]] %<>% raster::mask(inputPredictors[[6]])
  inputPredictors[[13]] %<>% raster::mask(inputPredictors[[6]])
  inputPredictors[[14]] %<>% raster::mask(inputPredictors[[6]])
}



#------------------------------------------------------
# Processing for activity season rasters
#------------------------------------------------------
if(activity_ssn == TRUE) {
  rasterNames <- c("PhotoAS_FirstDay","PhotoAS_LastDay","PhotoAS_Length","PrecipAS_FirstDay","PrecipAS_LastDay","PrecipAS_Length")
  inputPredictors %<>% setNames(rasterNames)
  
  #------------------------------------------------------
  # Change extent of Africa's precipitation activity season rasters to global
  #------------------------------------------------------
  inputPredictors[[4]] %<>% raster::extend(inputPredictors[[1]], value=NA)
  inputPredictors[[5]] %<>% raster::extend(inputPredictors[[1]], value=NA)
  inputPredictors[[6]] %<>% raster::extend(inputPredictors[[1]], value=NA)
}



#------------------------------------------------------
# Save each individual merged covariate raster as a .tif file
#------------------------------------------------------
for (i in 1:length(inputPredictors)) {
  writeRaster(inputPredictors[[i]], filename = paste0("Environmental Predictors Merged/",rasterNames[i],".tif"))
}



#------------------------------------------------------
# Load in the merged rasters for testing and debugging
#------------------------------------------------------
rasterList <- purrr::map_chr(list.files(path = "Environmental Predictors Merged", pattern='.tif$', all.files=TRUE, full.names=FALSE),
                             ~ paste0("Environmental Predictors Merged/", .))

inputPredictors <- lapply(rasterList, raster)


