#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Create ecoregion maps based on buffered species occurrences
#######################################################

testing = FALSE # For testing purposes, set testing = TRUE, which will allow things to run faster while debugging


if(Sys.getenv('SLURM_JOB_ID') != ""){ # Check if the script is running on Sherlock remote computing cluster
  library(dplyr)
  library(magrittr)
  library(tidyr)
  library(geosphere)
  library(sf)
  library(sp)
  library(rgeos)
  library(maptools)
  data(wrld_simpl)

  # Use the command line arguments supplied to set which species we'll be running 
  args <- commandArgs(TRUE) 
  species_inds <- as.numeric(args[1]) 
  
} else {
  source("E:/Documents/GitHub/mosquito-sdm/0-config.R")
  species_inds <- 1:8 
}


#------------------------------------------------------
# Load in lists and ecoregions
#------------------------------------------------------
tic <- Sys.time()
SpeciesOfInterest_NoSpace <- c("AedesAegypti",
                               "AedesAlbopictus",
                               "AnophelesGambiae",
                               "AnophelesStephensi",
                               "CulexAnnulirostris",
                               "CulexPipiens",
                               "CulexQuinquefasciatus",
                               "CulexTarsalis")

SpeciesOfInterest_Names <- c("Aedes aegypti",
                             "Aedes albopictus",
                             "Anopheles gambiae",
                             "Anopheles stephensi",
                             "Culex annulirostris",
                             "Culex pipiens",
                             "Culex quinquefasciatus",
                             "Culex tarsalis")

ecoregions <- read_sf(dsn = "./RESOLVE_Ecoregions/Ecoregions2017", layer = "Ecoregions2017")
my_crs <- "+proj=longlat +datum=WGS84 +no_defs"

Mosquitoes_SpeciesOfInterest <- read.csv("GBIF_Datasets_Cleaned/Mosquitoes_SpeciesOfInterest.csv", header = TRUE,
                                         encoding = "UTF-8", stringsAsFactors = FALSE)

toc <- Sys.time()
print("Loaded in lists and ecoregions")
print(toc - tic)


#------------------------------------------------------
# Create continent-specific shapefiles from wrld_simpl
#------------------------------------------------------
SouthAmerica_list <- c("Colombia", "Venezuela", "Suriname", "Guyana", "French Guiana",
                       "Ecuador", "Peru", "Bolivia", "Chile", "Argentina", "Uruguay",
                       "Paraguay", "Brazil", "Falkland Islands (Malvinas)")
SouthAmerica <- wrld_simpl[wrld_simpl$NAME %in% SouthAmerica_list, ]
NorthAmerica <- wrld_simpl[wrld_simpl$REGION==19,]
NorthAmerica <- NorthAmerica[!NorthAmerica$NAME %in% c(SouthAmerica_list,"Greenland"), ]
Greenland <- wrld_simpl[wrld_simpl$NAME=="Greenland",]
Africa <- wrld_simpl[wrld_simpl$REGION==2,]
Oceania <- wrld_simpl[wrld_simpl$REGION==9,]
Europe <- wrld_simpl[wrld_simpl$REGION==150,]
Asia <- wrld_simpl[wrld_simpl$REGION==142,]
Antarctica <- wrld_simpl[wrld_simpl$REGION==0,]


#------------------------------------------------------
# Split up the 'Rock and Ice' ecoregion by continent, and append these back into ecoregions
#------------------------------------------------------
tic <- Sys.time()
sf::sf_use_s2(FALSE)
Antarctica_rockice <- st_intersection(ecoregions %>% filter(ECO_NAME == "Rock and Ice"),
                                      st_as_sf(Antarctica) %>% st_set_crs(my_crs)) %>%
  dplyr::select(c(1:15,27))
NorthAmerica_rockice <- st_intersection(ecoregions %>% filter(ECO_NAME == "Rock and Ice"),
                                        st_as_sf(NorthAmerica) %>% st_set_crs(my_crs)) %>%
  dplyr::select(c(1:15,27))
Greenland_rockice <- st_intersection(ecoregions %>% filter(ECO_NAME == "Rock and Ice"),
                                     st_as_sf(Greenland) %>% st_set_crs(my_crs)) %>%
  dplyr::select(c(1:15,27))
Asia_rockice <- st_intersection(ecoregions %>% filter(ECO_NAME == "Rock and Ice"),
                                st_as_sf(Asia) %>% st_set_crs(my_crs)) %>%
  dplyr::select(c(1:15,27))

ecoregions %<>% filter(!ECO_NAME == "Rock and Ice")
ecoregions %<>% rbind(Antarctica_rockice, NorthAmerica_rockice, Asia_rockice)

toc <- Sys.time()
print("Split up the 'Rock and Ice' ecoregion by continent")
print(toc - tic)


#------------------------------------------------------
# Tidy and validate the sf geometry of ecoregions
#------------------------------------------------------
tic <- Sys.time()
ecoregions_check <- ecoregions %>% st_is_valid()
ecoregions_sf <- ecoregions %>% st_make_valid()

toc <- Sys.time()
print("Tidied and validated sf geometry of ecoregions")
print(toc - tic)


#------------------------------------------------------
# Define a function to buffer points with 200 km radius
#------------------------------------------------------
geosphere_buffer <- function(sf_points, 
                             buffer_deg = 0:360, # Degrees around the circle from which to acquire points
                             dist # Distance in meters
){
  buff_ll <- destPoint(st_coordinates(sf_points), # Acquire pairs of points in a circle
                       b = rep(buffer_deg, each = nrow(sf_points)), 
                       d = dist) %>% 
    as.data.frame() %>%
    st_as_sf(coords=c("lon","lat"), remove = FALSE) %>% 
    dplyr::mutate(order = 1:n(), 
                  id = order %% nrow(sf_points))
  
  # Split the buffered shapefiles if they cross longitude +/- 180 (i.e., edges of the map)
  buff_ll %<>% 
    group_by(id) %>% 
    mutate(world_edge_x = sign(lon) != sign(lag(lon))) %>% 
    replace_na(list(world_edge_x = FALSE)) %>% 
    mutate(id = id + 0.1*(cumsum(world_edge_x) %% 2)) %>% # If on opposite side of map, treat these point pairs as new ID
    dplyr::select(-lat, -lon, -world_edge_x) %>% 
    ungroup()
  
  # Create polygon buffer
  buff_polys = st_sf(
    aggregate(
      buff_ll$geometry,
      list(buff_ll$id),
      function(g){
        st_cast(st_combine(g),"POLYGON")
      }
    ))
  return(buff_polys)
}



#------------------------------------------------------
# Run loop through each species
#------------------------------------------------------
for(i in species_inds) {
  
  #------------------------------------------------------
  # Acquire occurrence points
  #------------------------------------------------------
  thisSpecies <- filter(Mosquitoes_SpeciesOfInterest, species == SpeciesOfInterest_Names[[i]])
  paste0("The species of interest is: ", SpeciesOfInterest_Names[[i]])
  occGPS <- dplyr::select(thisSpecies, c(decimalLongitude, decimalLatitude))
  
  occGPS_sf <- st_as_sf(occGPS, coords = c("decimalLongitude", "decimalLatitude"), 
                        crs = 4326, agr = "constant")
  
  
  #------------------------------------------------------
  # Call function to buffer occurrence points with 200 km
  #------------------------------------------------------
  if(testing == TRUE){
    occGPS_sf <- occGPS_sf[1:100,]
  }
  
  tic <- Sys.time()
  occGPS_buffered <- geosphere_buffer(occGPS_sf, dist = 200000)
  st_crs(occGPS_buffered) <- "+proj=longlat +datum=WGS84 +no_defs" 
  
  toc <- Sys.time()
  print(paste0("Buffered the occurrence points for ",SpeciesOfInterest_Names[[i]]))
  print(toc - tic)
  
  
  
  #------------------------------------------------------
  # Create a dataframe with start and end indices to loop through rows of buffered points
  # Splits up intersecting into smaller component tasks to save computation time
  #------------------------------------------------------
  ecoregion_inds <- list()
  occ_by <- 1000
  
  # Second column, lead by 1, and remove the last row
  indices <- data.frame(start_index = c(seq(from=1, to=nrow(occGPS_buffered), by=occ_by), nrow(occGPS_buffered)+1)) %>%
    mutate(end_index = lead(start_index) - 1) %>%
    na.omit()

  
  #------------------------------------------------------
  # Piece-wise intersecting all ecoregions with buffered points and acquire indices
  #------------------------------------------------------
  tic <- Sys.time()
  for(k in 1:nrow(indices)) {
    ecoregions_temp <- st_intersects(ecoregions_sf, occGPS_buffered[indices[k,1]:indices[k,2],])
    ecoregion_inds <- c(ecoregion_inds, 
                        purrr::map_dbl(ecoregions_temp, function(x) length(x)) %>% 
                          magrittr::is_greater_than(0) %>% 
                          which())
  }
  
  ecoregion_intersected_inds <- ecoregion_inds %>% Reduce("c", .) %>% unique
  
  toc <- Sys.time()
  print(paste0("Intersected ecoregions with buffered points and acquired indices for ",SpeciesOfInterest_Names[[i]]))
  print(toc - tic)
  
  
  #------------------------------------------------------
  # Save indices
  #------------------------------------------------------
  tic <- Sys.time()
  saveRDS(ecoregion_intersected_inds, paste0("Ecoregion_Outputs/Indices_",SpeciesOfInterest_NoSpace[i],".RDS"))
  
  toc <- Sys.time()
  print(paste0("Saved indices for ",SpeciesOfInterest_Names[[i]]))
  print(toc - tic)
  
  
  #------------------------------------------------------
  # Select and union the intersected ecoregions
  #------------------------------------------------------
  tic <- Sys.time()
  ecoregion_cut <- ecoregions_sf[ecoregion_intersected_inds, ] %>% st_make_valid() %>% 
    as_Spatial %>% gBuffer(byid = FALSE, width = 0) %>% st_as_sf
  
  toc <- Sys.time()
  print(paste0("Selected and unioned the intersected ecoregions for ",SpeciesOfInterest_Names[[i]]))
  print(toc - tic)
  
  
  #------------------------------------------------------
  # Save ecoregion shapefiles and buffered points
  #------------------------------------------------------
  tic <- Sys.time()
  saveRDS(ecoregion_intersected_inds, paste0("Ecoregion_Outputs/Indices_",SpeciesOfInterest_NoSpace[i],".RDS"))
  saveRDS(ecoregion_cut, paste0("Ecoregion_Outputs/Shapefile_",SpeciesOfInterest_NoSpace[i],".RDS"))
  saveRDS(occGPS_sf, paste0("Ecoregion_Outputs/BufferedPoints_",SpeciesOfInterest_NoSpace[i],".RDS"))
  
  toc <- Sys.time()
  print(paste0("Saved ecoregion shapefiles and buffered points for ",SpeciesOfInterest_Names[[i]]))
  print(toc - tic)
  
  
  #------------------------------------------------------
  # Save ecoregion maps, with and without points plotted
  #------------------------------------------------------
  tic <- Sys.time()
  png(paste0("Ecoregion_Outputs/Ecoregions_",SpeciesOfInterest_NoSpace[i],".png"), width=1000, height=1000)
  plot(st_geometry(ecoregion_cut))
  dev.off()
  
  png(paste0("Ecoregion_Outputs/Ecoregions_",SpeciesOfInterest_NoSpace[i],"_Dots.png"), width=1000, height=1000)
  plot(st_geometry(ecoregion_cut))
  plot(st_geometry(occGPS_sf), col="red", add=T)
  dev.off()
  
  png(paste0("Ecoregion_Outputs/Ecoregions_",SpeciesOfInterest_NoSpace[i],"_Buffered.png"), width=1000, height=1000)
  plot(st_geometry(ecoregion_cut))
  plot(st_geometry(occGPS_buffered), col="red", add=T)
  dev.off()
  
  toc <- Sys.time()
  print(paste0("Saved ecoregion maps for ",SpeciesOfInterest_Names[[i]]))
  print(toc - tic)

}



  