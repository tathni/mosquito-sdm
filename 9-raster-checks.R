#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Create a stack of binary (e.g., 0/1) environmental covariate rasters for checking NAs
# Assess correlation between variables using a pairs analysis
#######################################################

source("E:/Documents/GitHub/mosquito-sdm/0-config.R")


#------------------------------------------------------
## DATA LOAD-IN ##
#------------------------------------------------------
#------------------------------------------------------
# Load in environmental predictors
#------------------------------------------------------
predictors <- alply(list.files("Environmental Predictors Merged",
                               pattern = ".tif",
                               full.names = TRUE), 1, function(file){
                                 print(file)
                                 rast <- raster(file)
                                 return(rast)
                               })
rasterNames <- c("CD","EVIM","EVISD","FC","HPD","PDQ","PhotoASTM","PhotoASTSD","PrecipASTM","PrecipASTSD","PWQ","SW","TAM","TASD","WS")
predictors %<>% setNames(rasterNames) %>% stack()

Mosquitoes_SpeciesOfInterest <- read.csv("GBIF_Datasets_Cleaned/Mosquitoes_SpeciesOfInterest.csv", header = TRUE,
                                         encoding = "UTF-8", stringsAsFactors = FALSE)



#------------------------------------------------------
## BINARY CHECKS ##
#------------------------------------------------------
#------------------------------------------------------
# Initialize and process binary rasters, where 0 = value, 1 = NA or missing value
#------------------------------------------------------
binary_rast_list <- list()

for(i in 1:nlayers(predictors)) {
  test <- predictors[[i]]
  
  blank_rast <- setValues(test, rep(0, ncell(test))) # Make a blank raster filled with zeros with the same structure as the read-in raster
  binary_rast <- mask(blank_rast, mask = test, # Everywhere the raster layer was masked, set it to 1 in the binary-mask raster
                      mask_value = test@file@nodatavalue, 
                      updatevalue = 1)
  
  binary_rast_list %<>% c(binary_rast)
}


#------------------------------------------------------
# Plot individual binary raster layers
#------------------------------------------------------
binary_rast_stack <- binary_rast_list %>% stack()

for(i in 1:nlayers(binary_rast_stack)) {
  save_name <- paste0("Environmental Predictors Checks/",
                      names(binary_rast_stack)[i],".png")
  png(save_name)
  plot(binary_rast_stack[[i]])
  dev.off()
}


#------------------------------------------------------
# Sum binary raster layers and plot
#------------------------------------------------------
binary_rast_sum <- sum(binary_rast_stack)

png("Environmental Predictors Checks/Binary_Raster_Sum_NA.png")
plot(binary_rast_sum)
dev.off()



#------------------------------------------------------
## PAIRS CORRELATION PLOT ##
#------------------------------------------------------
#------------------------------------------------------
# Acquire occurrence cell centroids
#------------------------------------------------------
occ_points <- Mosquitoes_SpeciesOfInterest %>%
  dplyr::select(c(decimalLongitude, decimalLatitude))

rast <- predictors[[1]] # Choose any generic raster to acquire cells and centroids from

occ_longlat <- cellFromXY(rast, occ_points) %>% as.data.frame() %>%
  setNames("cell") %>% unique() %>%
  mutate(longitude = xFromCell(rast, cell),  # Acquire longitude (x) and latitude (y) from cell centroids
         latitude = yFromCell(rast, cell)) %>%
  dplyr::select(-cell) %>% # Cell number is now obsolete if working from (x,y) as an sf object
  filter(!is.na(longitude) & !is.na(latitude)) # Remove the NA locations

occ_sf <- st_as_sf(occ_longlat, coords = c("longitude","latitude"),
                   crs = 4326, agr = "constant")


#------------------------------------------------------
# Extract raster data from 10k randomly chosen occurrence points
#------------------------------------------------------
set.seed(seedNum)
occ_15k <- occ_sf %>%
  .[sample(nrow(.),
           size = 15000,
           replace = FALSE),] # Oversample to acquire 15k occ points
raster_15k <- raster::extract(predictors, occ_15k)

set.seed(seedNum)
raster_10k <- raster_15k %>% na.omit() %>%
  .[sample(nrow(.),
           size = 10000,
           replace = FALSE),] # Select 10k occ points


#------------------------------------------------------
# Visualize pairs plot
#------------------------------------------------------
pdf("Environmental Predictors Checks/Pairs Correlation Covariates.pdf", width=10, height=8)
corrplot(cor(raster_10k),
         method = "color",
         addCoef.col = "black",
         tl.col = "black", tl.srt = 45,
         type = "upper")
dev.off()

