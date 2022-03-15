#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Create a summed stack of binary-mask (e.g., 0/1) environmental covariates for checking NAs
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
rasterNames <- c("CD","EVIM","EVISD","FC","HPD","PDQ","PhotoASTM","PhotoASTSD","PrecipASTM","PrecipASTSD","PWQ","SW","TAM","TASD","WS")
predictors_preStack <- setNames(predictors_preStack, rasterNames)


#------------------------------------------------------
# Initialize and process binary-mask rasters, where 0 = value, 1 = NA or missing value
#------------------------------------------------------
binary_mask_preStack <- list()

for(i in 1:length(predictors_preStack)) {
  test <- predictors_preStack[[i]]
  
  blank_rast <- setValues(test, rep(0, ncell(test))) # Make a blank raster filled with zeros with the same structure as the read-in raster
  binary_mask <- mask(blank_rast, mask = test, # Everywhere the raster layer was masked, set it to 1 in the binary-mask raster
                      mask_value = test@file@nodatavalue, 
                      updatevalue = 1)
  
  binary_mask_preStack <- c(binary_mask_preStack, binary_mask)
}


#------------------------------------------------------
# Stack raster layers, sum 0/1 layers, and save
#------------------------------------------------------
binary_mask_stack <- binary_mask_preStack %>% stack()
binary_mask_sum <- sum(binary_mask_stack)
writeRaster(predictor_sum_precipSeason_check, filename = "Environmental Predictors Sum Checks/Binary_Mask_Sum.tif", format = "GTiff", overwrite=T)


#------------------------------------------------------
# Read in summed binary mask raster
#------------------------------------------------------
binary_mask_sum <- raster("Environmental Predictors Sum Checks/Binary_Mask_Sum.tif")


#------------------------------------------------------
# Count number of total landmass cells and number of NA cells in each raster layer
#------------------------------------------------------
complete_raster <- predictors_preStack[[6]][!is.na(predictors_preStack[[6]])] <- 1  # Choose random raster with known completeness to acquire total cells
total_landmass_cells <- freq(complete_raster, value = 1)

for(i in 1:length(binary_mask_preStack)) {
  # Mask the NAs in the ocean with the known complete raster
  my_layer <- mask(binary_mask_preStack[[i]], mask = complete_raster)
  
  # Count the number of 1's in each binary mask layer, which signifies landmass NA cells
  layer_na_cells <- freq(my_layer, value = 1)
  na_cells_list %<>% c(layer_na_cells)
}


#------------------------------------------------------
# Plot individual binary mask raster layers
#------------------------------------------------------
for(i in 1:nlayers(binary_mask_preStack)) {
  save_name <- paste0("Environmental Predictors Sum Checks/",
                      names(binary_mask_preStack)[i],".png")
  png(save_name)
  plot(binary_mask_preStack[[i]])
  dev.off()
}


#------------------------------------------------------
# Plot the binary mask sum
#------------------------------------------------------
png("Environmental Predictors Sum Checks/Binary_Mask_Sum_NA.png")
plot(binary_mask_sum, width = 500, height = 500, maxpixels=1e8)
dev.off()


## ??? delete year round folder and predictor_sum_check.tif from sum checks/ subdirectory



