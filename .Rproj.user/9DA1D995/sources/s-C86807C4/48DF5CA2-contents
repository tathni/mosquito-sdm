#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Create ecoregion-based sampling range maps
#######################################################

source("E:/Documents/GitHub/mosquito-sdm/0-config.R")
setwd("E:/SynologyDrive/Tejas_Server/! Research/! Mordecai Lab/! Mosquito SDM Thermal Dependence/")


#------------------------------------------------------
## LOAD IN DATA ##
#------------------------------------------------------
tic <- Sys.time()

speciesList <- c("AedesAegypti",
                 "AedesAlbopictus",
                 "AnophelesGambiae",
                 "AnophelesStephensi",
                 "CulexPipiens",
                 "CulexQuinquefasciatus",
                 "CulexTarsalis")

SpeciesOfInterest_Names <- c("Aedes aegypti",
                             "Aedes albopictus",
                             "Anopheles gambiae",
                             "Anopheles stephensi",
                             "Culex pipiens",
                             "Culex quinquefasciatus",
                             "Culex tarsalis")

ecoregions <- read_sf(dsn = "./RESOLVE Ecoregions/Ecoregions2017", layer = "Ecoregions2017") %>%
  as("Spatial") %>%
  spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))

Mosquitoes_SpeciesOfInterest <- read.csv("GBIF Datasets Cleaned/Mosquitoes_SpeciesOfInterest.csv", header = TRUE,
                                         encoding = "UTF-8", stringsAsFactors = FALSE)




### select ecoregions that overlap and are adjacent
ecoregions_st <- ecoregions
ecoregions_sf <- ecoregions_st %>% st_as_sf()
ecoregions_sf %>% st_is_valid()
ecoregions_sf %<>% st_make_valid()



for(i in 1:length(SpeciesOfInterest_Names)) {
  
  #### get species points
  thisSpecies <- filter(Mosquitoes_SpeciesOfInterest, species == SpeciesOfInterest_Names[i])
  paste0("The species of interest is: ", SpeciesOfInterest_Names[i])
  occGPS <- dplyr::select(thisSpecies, c(decimalLongitude, decimalLatitude))
  
  occGPS_sf <- st_as_sf(occGPS, coords = c("decimalLongitude", "decimalLatitude"), 
                   crs = 4326, agr = "constant")
  
  
  #### buffer, 1 degree = 111 km, so 200 km = 1.802 degrees
  occGPS_buffered <- st_buffer(occGPS_sf, 200)
  paste0("Buffered the points")
  
  
  
  # st_bbox(ecoregions)
  
  ecoregion_inds <- list()
  tic <- Sys.time()
  occ_by <- 1000
  
  
  ## dataframe with 2 columns, start index and end index, loop through the rows
  # second column, lead by 1, and remove the last row
  
  indices <- data.frame(start_index = c(seq(from=1, to=nrow(occGPS_buffered), by=occ_by), nrow(occGPS_buffered)+1)) %>%
    mutate(end_index = lead(start_index) - 1) %>%
    na.omit()

  
  for(k in 1:nrow(indices)) {
    ecoregions_temp <- st_intersects(ecoregions_sf, occGPS_buffered[indices[k,1]:indices[k,2],])
    ecoregion_inds <- c(ecoregion_inds, 
                        purrr::map_dbl(ecoregions_temp, function(x) length(x)) %>% 
                          magrittr::is_greater_than(0) %>% 
                          which())
  }
  ecoregion_intersected_inds <- ecoregion_inds %>% Reduce("c", .) %>% unique
  toc <- Sys.time()
  
  newTic <- Sys.time()
  ecoregion_cut <- ecoregions_sf[ecoregion_intersected_inds, ] %>% st_union
  newToc <- Sys.time()
  
  png(paste0("Ecoregion Outputs/Ecoregions_",speciesList[i],"_Dots.png"), width=1000, height=1000)
  plot(st_geometry(ecoregion_cut))
  plot(st_geometry(occGPS_sf), col="red", add=T)
  dev.off()
  
  png(paste0("Ecoregion Outputs/Ecoregions_",speciesList[i],".png"), width=1000, height=1000)
  plot(st_geometry(ecoregion_cut))
  dev.off()
  
  saveRDS(ecoregion_cut, paste0("Ecoregion Outputs/Shapefile_",speciesList[i],".RDS"))
  saveRDS(ecoregion_intersected_inds, paste0("Ecoregion Outputs/Indices_",speciesList[i],".RDS"))
  saveRDS(occGPS_sf, paste0("Ecoregion Outputs/BufferedPoints_",speciesList[i],".RDS"))

}


## Load in gambiae shapefile and points for diagnostic check
gambiae_shapefile <- readRDS("Ecoregion Outputs/Shapefile_AnophelesGambiae.RDS")
gambiae_points <- readRDS("Ecoregion Outputs/BufferedPoints_AnophelesGambiae.RDS")

## Plot shapefile and superimpose points, and manually add a 200 km scalebar to assess buffering
plot(gambiae_shapefile)
plot(occGPS_sf, col="red", add=T)
raster::scalebar(200, xy=click())





## UNUSED

# tic <- Sys.time()
# ecoregions_intersect <- st_intersects(ecoregions_sf, occGPS_buffered[1:2,] %>% st_union())
# toc <- Sys.time()
# paste0("Time for 2 points = ",toc - tic)
# 
# tic <- Sys.time()
# ecoregions_intersect <- st_intersects(ecoregions_sf, occGPS_buffered[1:10,] %>% st_union())
# toc <- Sys.time()
# paste0("Time for 10 points = ",toc - tic)
# 
# 
# tic <- Sys.time()
# for(i in c(seq(from=1, to=length(occGPS_buffered), by=1000), length(occGPS_buffered))) {
#   ecoregions_temp <- st_intersects(ecoregions_sf, occGPS_buffered[i:(i+1000),] %>% st_union)
#   ecoregions_combined <- st_union(ecoregions_temp, ecoregions_combined)
# }
# 
# tic <- Sys.time()
# ecoregions_intersect <- st_intersects(ecoregions_sf, occGPS_buffered %>% st_union())
# toc <- Sys.time()
# paste0("Time for all points = ",toc - tic)
# 
# 
# eco_eco_intersect <- st_intersects(ecoregions_sf, ecoregions_sf)
# saveRDS(eco_eco_intersect, "eco_eco_intersect.RDS")
# readRDS("eco_eco_intersect.RDS")
# 
# 
# eco_index <- which(purrr::map_dbl(ecoregions_intersect, length) > 0)
# ecoregions_new <- ecoregions_sf[eco_index,]
# plot(st_geometry(ecoregions_new))
# plot(st_geometry(occGPS_buffered[1:10,]), border="red", lwd=5, add=T)
# 
# 
# eco_adjacent_index <- unique(Reduce(c,eco_eco_intersect[eco_index,]))
# ecoregions_adjacent <- ecoregions_sf[eco_adjacent_index,]
# ecoregions_final <- ecoregions_adjacent %>% st_union()
# 
# saveRDS(ecoregions_final, "ecoregions_final.RDS")
# 
# 
# plot(ecoregions_final %>% st_simplify() %>% st_geometry())
#   
# 
# ### calculate time elapsed
# toc <- Sys.time()
# toc - tic
  
  
  
  