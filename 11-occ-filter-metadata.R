#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Gather filter metadata for each mosquito species' occurrence points as they pass through sieves of filters
#######################################################

source("C:/Users/tejas/Documents/GitHub/mosquito-sdm/0-config.R")


#------------------------------------------------------
## DATA LOAD-IN ##
#------------------------------------------------------
#------------------------------------------------------
# Load in occurrence data
#------------------------------------------------------
Mosquitoes_SpeciesOfInterest <- read.csv("GBIF_Datasets_Cleaned/Mosquitoes_SpeciesOfInterest.csv", header = TRUE,
                                         encoding = "UTF-8", stringsAsFactors = FALSE)


#------------------------------------------------------
# Load in environmental predictors and stack by activity season
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
# Load in landmass mask
#------------------------------------------------------
landmass_mask <- raster("Filter Masks/Landmass/Landmass.tif") %>%
  setNames(c("Landmass"))


#------------------------------------------------------
# Load in activity season length masks
#------------------------------------------------------
activity_lengths <- alply(list.files("Filter Masks/Activity Season Lengths",
                                     pattern = ".tif",
                                     full.names = TRUE), 1, function(file){
                                       print(file)
                                       rast <- raster(file)
                                       return(rast)
                                     }) %>%
  setNames(c("Photoperiod","Precipitation"))
activity_lengths_index <- c(NA,1,2,NA,NA,1,NA,1)


#------------------------------------------------------
# Load in predictor sum non-NA masks
#------------------------------------------------------
predictor_sum_mask <- alply(list.files("Filter Masks/Predictor Sum Non-NA",
                                       pattern = ".tif",
                                       full.names = TRUE), 1, function(file){
                                         print(file)
                                         rast <- raster(file)
                                         return(rast)
                                       }) %>%
  setNames(c("Year-Round","Photoseason","Precipitation"))
predictor_sum_index <- c(1,2,3,1,1,2,1,2)


#------------------------------------------------------
# Create lists and containers to house filter metadata
#------------------------------------------------------
filter_metadata_pre <- readRDS("Metadata/filter_metadata_pre.RDS")
filter_metadata_shell <- data.frame(matrix(ncol = 5, nrow = 8))
colnames(filter_metadata_shell) <- c("Activity_Season","Landmass_Points","Activity_Season_Points","Predictor_Sum_Non_NA_Points","Unique_Cell_Centroids")
filter_metadata <- cbind(filter_metadata_pre, filter_metadata_shell) %>%
  .[,c(1,8,2:7,9:12)]




#------------------------------------------------------
## RESTRICT OCCURRENCES THROUGH PROGRESSIVE FILTERS ##
#------------------------------------------------------
for(i in 1:length(SpeciesOfInterest_Names)) {
  print(SpeciesOfInterest_Names[[i]])
  
  #------------------------------------------------------
  # Acquire occurrences for species of interest
  #------------------------------------------------------
  occ_points <- Mosquitoes_SpeciesOfInterest %>%
  filter(species == SpeciesOfInterest_Names[[i]]) %>%
  dplyr::select(c(decimalLongitude, decimalLatitude))


  #------------------------------------------------------
  # Sieve 1: landmass
  #------------------------------------------------------
  landmass_values <- raster::extract(landmass_mask, occ_points)
  occ_landmass <- occ_points[which(!is.na(landmass_values)),]
  
  num_landmass <- nrow(occ_landmass)
  
  
  #------------------------------------------------------
  # Sieve 2: activity season
  #------------------------------------------------------
  if(SpeciesOfInterest_Names[[i]] %in% c("Aedes albopictus",
                                         "Anopheles gambiae",
                                         "Culex pipiens",
                                         "Culex tarsalis")) {
    occ_activity_length <- raster::extract(activity_lengths[[(activity_lengths_index[[i]])]], occ_landmass)
    occ_activity <- occ_landmass[which(occ_activity_length > 0),]
  } else {
    occ_activity <- occ_landmass
  }
  
  num_activity <- nrow(occ_activity)
  
  
  #------------------------------------------------------
  # Sieve 3: predictor sum non-NA
  #------------------------------------------------------
  occ_predictor_sum_vals <- raster::extract(predictor_sum_mask[[(predictor_sum_index[[i]])]], occ_activity)
  occ_predictor_sum <- occ_activity[which(!is.na(occ_predictor_sum_vals)),]
  
  num_predictor_sum <- nrow(occ_predictor_sum)
  
  
  #------------------------------------------------------
  # Sieve 4: unique cell centroids
  #------------------------------------------------------
  rast <- predictors_preStack[[1]] # Choose any generic raster to acquire cells and centroids from
  
  occ_cell_centroids <- cellFromXY(rast, occ_predictor_sum) %>% as.data.frame() %>%
    setNames("cell") %>% unique() %>%
    mutate(longitude = xFromCell(rast, cell),  # Acquire longitude (x) and latitude (y) from cell centroids
           latitude = yFromCell(rast, cell)) %>%
    dplyr::select(-cell) %>% # Cell number is now obsolete if working from (x,y) as an sf object
    filter(!is.na(longitude) & !is.na(latitude)) # Remove the NA locations
  
  num_cell_centroids <- nrow(occ_cell_centroids)
  
  
  #------------------------------------------------------
  # Sieve check: Does reversing the order, by first taking cell centroids of occ (sieve 4) and then progressing through sieves 1-3, yield same number?
  #------------------------------------------------------
  occ_cell_centroids <- cellFromXY(rast, occ_points) %>% as.data.frame() %>%
    setNames("cell") %>% unique() %>%
    mutate(longitude = xFromCell(rast, cell), 
           latitude = yFromCell(rast, cell)) %>%
    dplyr::select(-cell) %>%
    filter(!is.na(longitude) & !is.na(latitude)) %>% 
    st_as_sf(coords = c("longitude","latitude"),
             crs = 4326, agr = "constant")
  
  landmass_values <- raster::extract(landmass_mask, occ_cell_centroids)
  occ_landmass <- occ_cell_centroids[which(!is.na(landmass_values)),] 
  
  if(SpeciesOfInterest_Names[[i]] %in% c("Aedes albopictus",
                                         "Anopheles gambiae",
                                         "Culex pipiens",
                                         "Culex tarsalis")) {
    occ_activity_length <- raster::extract(activity_lengths[[(activity_lengths_index[[i]])]], occ_landmass)
    occ_activity <- occ_landmass[which(occ_activity_length > 0),]
  } else {
    occ_activity <- occ_landmass
  }
  
  occ_predictor_sum_vals <- raster::extract(predictor_sum_mask[[(predictor_sum_index[[i]])]], occ_activity)
  occ_predictor_sum <- occ_activity[which(!is.na(occ_predictor_sum_vals)),]
  
  num_cell_centroids_new <- nrow(occ_predictor_sum)
  print(paste0("Final number of unique occurrence cell centroids: ",num_cell_centroids_new))
  print(paste0("Reversing sieve order yields same number of cells?: ",num_cell_centroids == num_cell_centroids_new))
  
  
  #------------------------------------------------------
  # Record the filter metadata for occurrences
  #------------------------------------------------------
  filter_metadata[[2]][[i]] <- ActivitySeason_Type[[i]]
  filter_metadata[[9]][[i]] <- num_landmass
  filter_metadata[[10]][[i]] <- num_activity
  filter_metadata[[11]][[i]] <- num_predictor_sum
  filter_metadata[[12]][[i]] <- num_cell_centroids

}


#------------------------------------------------------
# Export .csv file
#------------------------------------------------------
write.csv(filter_metadata, file = "Metadata/Filter Metadata by Species.csv", row.names=FALSE)


