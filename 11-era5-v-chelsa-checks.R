#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Compare correlation of ERA5 and CHELSA temperature datasets for Jan and June, 1981-2010 aggregated period
#######################################################

source("E:/Documents/GitHub/mosquito-sdm/0-config.R")


#------------------------------------------------------
## DATA LOAD-IN ##
#------------------------------------------------------
#------------------------------------------------------
# Load in occurrence data
#------------------------------------------------------
Mosquitoes_SpeciesOfInterest <- read.csv("GBIF_Datasets_Cleaned/Mosquitoes_SpeciesOfInterest.csv", header = TRUE,
                                         encoding = "UTF-8", stringsAsFactors = FALSE)


#------------------------------------------------------
# Load in ERA5 and CHELSA temperature rasters for Jan and June
#------------------------------------------------------
era5_jan <- raster("CHELSA Data/TAM_ERA5_Jan.tif")
era5_june <- raster("CHELSA Data/TAM_ERA5_June.tif")
chelsa_jan <- raster("CHELSA Data/CHELSA_tas_01_1981-2010_V.2.1.tif")
chelsa_june <- raster("CHELSA Data/CHELSA_tas_06_1981-2010_V.2.1.tif")


#------------------------------------------------------
# Check extents for each temperature raster
#------------------------------------------------------
crs(era5_jan)
crs(era5_june)
crs(chelsa_jan)
crs(chelsa_june)



#------------------------------------------------------
## OCCURRENCE POINTS: ERA5 v. CHELSA ##
#------------------------------------------------------
#------------------------------------------------------
# Extract 10k occurrence points
#------------------------------------------------------
occ_points <- Mosquitoes_SpeciesOfInterest %>%
  dplyr::select(c(decimalLongitude, decimalLatitude)) %>%
  sample_n(100000)  # Oversample to then subset to 10k uniques

rast <- era5_jan  # Choose any generic raster to acquire cells and centroids from

occ_longlat <- cellFromXY(rast, occ_points) %>% as.data.frame() %>%
  setNames("cell") %>% unique() %>%
  mutate(longitude = xFromCell(rast, cell),  # Acquire longitude (x) and latitude (y) from cell centroids
         latitude = yFromCell(rast, cell)) %>%
  dplyr::select(-cell) %>% # Cell number is now obsolete if working from (x,y) as an sf object
  filter(!is.na(longitude) & !is.na(latitude)) # Remove the NA locations

occ_sf <- st_as_sf(occ_longlat, coords = c("longitude","latitude"),
                   crs = 4326, agr = "constant") %>%
  sample_n(10000)


#------------------------------------------------------
# Raster extract
#------------------------------------------------------
era5_chelsa_jan <- data.frame(raster::extract(era5_jan, occ_sf)) %>%
  cbind(data.frame(raster::extract(chelsa_jan, occ_sf))) %>%
  cbind(st_coordinates(occ_sf)) %>%
  setNames(c("ERA5","CHELSA","X","Y")) %>%
  na.omit() %>%
  dplyr::select(-c("X","Y"))

era5_chelsa_june <- data.frame(raster::extract(era5_june, occ_sf)) %>%
  cbind(data.frame(raster::extract(chelsa_june, occ_sf))) %>%
  cbind(st_coordinates(occ_sf)) %>%
  setNames(c("ERA5","CHELSA","X","Y")) %>%
  na.omit() %>%
  dplyr::select(-c("X","Y"))


#------------------------------------------------------
# Pairs correlation analysis
#------------------------------------------------------
pdf("CHELSA Data/ERA5_CHELSA_Occ_January_Pairs.pdf")
ggpairs(era5_chelsa_jan)
dev.off()


pdf("CHELSA Data/ERA5_CHELSA_Occ_June_Pairs.pdf")
ggpairs(era5_chelsa_june)
dev.off()


