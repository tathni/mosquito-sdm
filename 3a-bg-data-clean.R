#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Clean the Class Insecta and supplemental Culicidae background points
# Compile weighted bias mask that will be used to acquire bg according to sampling effort
#######################################################

if(Sys.getenv('SLURM_JOB_ID') != ""){ # Check if the script is running on Sherlock remote computing cluster (scratch folder)
  library(readxl)
  library(dplyr)
  library(magrittr)
  library(raster)
  library(sp)
  library(rgdal)
  library(data.table)
  
  decimalNums <- function(x) {
    if ((x %% 1) != 0) {
      nchar(strsplit(sub('0+$', '', as.character(format(x, scientific=FALSE, digits=20))), ".", fixed=TRUE)[[1]][[2]])
    } else {
      return(0)
    }
  }
  
  options(scipen = 100000)
  
} else {
  source("C:/Users/tejas/Documents/GitHub/mosquito-sdm/0-config.R")
}


#------------------------------------------------------
## BACKGROUND CLEANING ##
#------------------------------------------------------
#------------------------------------------------------
# Read in data for Class Insecta (GBIF) and supplemental Culicidae (ALA, Sinka, Wiebe)  points
#------------------------------------------------------
Background_GBIF_Raw <- fread("0083519-210914110416597.csv",
                                sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F)
Background_ALA_Raw <- read.csv("Culicidae_ALA_Raw.csv", sep = ",", header = T, encoding = "UTF-8", stringsAsFactors = F)
Background_Sinka_Raw <- read.csv("AnophelesStephensi_Background_Sinka2020.csv", sep = ",", header = T, stringsAsFactors = F)
Background_Wiebe_Raw <- read_excel("AnophelesGambiae_Wiebe2017.xlsx", sheet=8)

print("Loaded in background points")


#------------------------------------------------------
# Filter GBIF background from 2000-2019, by <= 1000m uncertainty, and by >= 2 decimal points
#------------------------------------------------------
Background_GBIF <- Background_GBIF_Raw %>%
  filter(!species == "",
         !is.na(decimalLongitude),
         !as.numeric(as.character(decimalLongitude)) %in% c(NA, NaN, Inf, -Inf),
         !is.na(decimalLatitude),
         !as.numeric(as.character(decimalLatitude)) %in% c(NA, NaN, Inf, -Inf),
         basisOfRecord != "FOSSIL_SPECIMEN",
         basisOfRecord != "UNKNOWN",
         year >= 2000 & year <= 2019,
         is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 1000) %>%
  dplyr::mutate(rowNum = row_number()) %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode, year, month, rowNum)
names(Background_GBIF)[names(Background_GBIF) == "countryCode"] <- "country"

indexRows <- list()
counter <- 1
for(i in 1:nrow(Background_GBIF)) {
  lon_decimals <- try(decimalNums(Background_GBIF$decimalLongitude[[i]]))
  lat_decimals <- try(decimalNums(Background_GBIF$decimalLatitude[[i]]))
  
  if(is.numeric(lon_decimals) & is.numeric(lat_decimals)){
    if(lon_decimals < 2 & lat_decimals < 2) {
      indexRows[[counter]] <- i
      counter <- counter+1
    } 
  } else {
    print(paste0("decimalNums failed for row ", i))
    print(lon_decimals[1])
    print(lat_decimals[1])
    print(str(Background_GBIF[i,]))
    print(Background_GBIF[i,])
    stop()
  }
}

Background_GBIF <- Background_GBIF[!Background_GBIF$rowNum %in% indexRows, ] %>%
  dplyr::select(-rowNum)

Background_GBIF %<>% mutate(source = "GBIF")

print("Cleaned GBIF (Class Insecta) background")



#------------------------------------------------------
# Clean Sinka background for Anopheles stephensi
#------------------------------------------------------
Background_Sinka <- Background_Sinka_Raw %>%
  filter(!Species == "",
         !is.na(Longitude),
         !is.na(Latitude),
         Start_Year >= 2000 & Start_Year <= 2019 |
           is.na(End_Year) & Publication_date >= 2004) %>%
  dplyr::mutate(rowNum = row_number()) %>%
  dplyr::select(Species, Longitude, Latitude, Country, Start_Year, Start_month, rowNum)
colnames(Background_Sinka) <- c("species","decimalLongitude","decimalLatitude","country","year","month","rowNum")
Background_Sinka$species[Background_Sinka$species == "funestus_group"] <- "Anopheles funestus"
Background_Sinka$species[Background_Sinka$species == "gambiaecomplex"] <- "Anopheles gambiae complex"

indexRows <- list()
counter <- 1
for(i in 1:nrow(Background_Sinka)) {
  if(decimalNums(Background_Sinka$decimalLongitude[[i]]) < 2 &
     decimalNums(Background_Sinka$decimalLatitude[[i]]) < 2) {
    indexRows[[counter]] <- i
    counter <- counter+1
  }
}

Background_Sinka <- Background_Sinka[!Background_Sinka$rowNum %in% indexRows, ] %>%
  dplyr::select(-rowNum)

Background_Sinka %<>% mutate(source = "Sinka et al., 2020")

print("Cleaned Sinka (Anopheles stephensi) background")


#------------------------------------------------------
# Clean Wiebe background for Anopheles gambiae
#------------------------------------------------------
Background_Wiebe <- Background_Wiebe_Raw %>%
  filter(!is.na(Longitude),
         !is.na(Latitude),
         Start_Year >= 2000 & Start_Year <= 2019) %>%
  dplyr::mutate(rowNum = row_number(),
                species = "Anopheles {sp. ?}",
                month = NA) %>%
  dplyr::select(species, Longitude, Latitude, Country, Start_Year, month, rowNum)
colnames(Background_Wiebe) <- c("species","decimalLongitude","decimalLatitude","country","year","month","rowNum")
         

indexRows <- list()
counter <- 1
for(i in 1:nrow(Background_Wiebe)) {
  if(decimalNums(Background_Wiebe$decimalLongitude[[i]]) < 2 &
     decimalNums(Background_Wiebe$decimalLatitude[[i]]) < 2) {
    indexRows[[counter]] <- i
    counter <- counter+1
  }
}

Background_Wiebe <- Background_Wiebe[!Background_Wiebe$rowNum %in% indexRows, ] %>%
  dplyr::select(-rowNum)

Background_Wiebe %<>% mutate(source = "Wiebe et al., 2017")

print("Cleaned Wiebe (Anopheles gambiae) background")


#------------------------------------------------------
# Combine the ALA, Sinka, and Wiebe datasets with GBIF and save
#------------------------------------------------------
Background_Insecta <- rbind(Background_GBIF, Background_ALA, Background_Sinka, Background_Wiebe)
write.csv(Background_Insecta, file = "Background_Insecta.csv", row.names = F)



#------------------------------------------------------
## CREATE WEIGHTED BIAS MASK ##
#------------------------------------------------------
#------------------------------------------------------
# Read in template raster and list setup
#------------------------------------------------------
rast <- raster("EVIM.tif")

bg_species_list <- c("Main",
                     "An_gambiae",
                     "An_stephensi")


#------------------------------------------------------
# Extract number of Insecta (+ supplemental) background points per grid cell (i.e., weighted bias mask)
#------------------------------------------------------
for(i in 1:length(bg_species_list)) {
  if(bg_species_list[i] == "Main") {
    Background <- Background_Insecta %>% filter(source %in% c("GBIF"))
  }
  if(bg_species_list[i] == "An_gambiae") {
    Background <- Background_Insecta %>% filter(source %in% c("GBIF","Wiebe et al., 2017"))
  }
  if(bg_species_list[i] == "An_stephensi") {
    Background <- Background_Insecta %>% filter(source %in% c("GBIF","Sinka et al., 2020"))
  }
  
  bg_points <- Background %>% dplyr::select(c(decimalLongitude, decimalLatitude)) %>%
    as.matrix()
  
  bg_longlat <- cellFromXY(rast, bg_points) %>% as.data.frame() %>%
    mutate(count = 1) %>% setNames(c("cell","count")) %>%
    group_by(cell) %>% dplyr::summarize(count = sum(count)) %>%
    arrange(desc(count)) %>%
    mutate(longitude = xFromCell(rast, cell),  # Acquire longitude (x) and latitude (y) from cell centroids
           latitude = yFromCell(rast, cell)) %>%
    dplyr::select(-cell) %>% # Cell number is now obsolete, since will be working from (x,y) as an sf object
    filter(!is.na(longitude) & !is.na(latitude)) # Remove the NA locations
  
  save_name <- paste0("bg_longlat_",bg_species_list[i])
  assign(paste0(save_name), bg_longlat)
  
  print(paste0("Compiled weighted bias mask for ",bg_species_list[i]))
}


#------------------------------------------------------
# Save RDS file for each weighted bias mask
#------------------------------------------------------
saveRDS(bg_longlat_Main, file = "Background_Mask_Main.RDS")
saveRDS(bg_longlat_An_gambiae, file = "Background_Mask_An_Gambiae.RDS")
saveRDS(bg_longlat_An_stephensi, file = "Background_Mask_An_Stephensi.RDS")

print("Saved all weighted background bias masks")



