#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Clean the Class Insecta and supplemental Culicidae background points
# Create weighted bias mask that will be used to correct for sampling effort
#######################################################

if(Sys.getenv('SLURM_JOB_ID') != ""){ # Check if the script is running on Sherlock remote computing cluster
  library(dplyr)
  library(magrittr)
  library(raster)
} else {
  source("E:/Documents/GitHub/mosquito-sdm/0-config.R")
}


#------------------------------------------------------
## BACKGROUND CLEANING ##
#------------------------------------------------------
#------------------------------------------------------
# Read in data for Class Insecta (GBIF) and supplemental Culicidae (ALA, Sinka, Wiebe)  points
#------------------------------------------------------
Background_GBIF_Raw <- read.csv("0083519-210914110416597.csv",
                                sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F)
Background_ALA_Raw <- read.csv("Culicidae_ALA_Raw.csv", sep = ",", header = T, encoding = "UTF-8", stringsAsFactors = F)
Background_Sinka_Raw <- read.csv("AnophelesStephensi_Background_Sinka2020.csv", sep = ",", header = T, stringsAsFactors = F)
Background_Wiebe_Raw <- read_excel("AnophelesGambiae_Wiebe2017.xlsx", sheet=8)


#------------------------------------------------------
# Filter GBIF background from 2000-2019, by <= 1000m uncertainty, and by >= 2 decimal points
#------------------------------------------------------
Background_GBIF <- Background_GBIF_Raw %>%
  filter(!species == "",
         !is.na(decimalLongitude),
         !is.na(decimalLatitude),
         basisOfRecord != "FOSSIL_SPECIMEN",
         basisOfRecord != "UNKNOWN",
         year >= 2000 & year <= 2019,
         is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 1000) %>%
  dplyr::mutate(rowNum = row_number()) %>%
  dplyr::select(species, Background_GBIF, decimalLatitude, countryCode, year, month, rowNum)
names(Background_GBIF)[names(Background_Culicidae) == "countryCode"] <- "country"

indexRows <- list()
counter <- 1
for(i in 1:nrow(Background_GBIF)) {
  if(decimalNums(Background_GBIF$decimalLongitude[[i]]) < 2 &
     decimalNums(Background_GBIF$decimalLatitude[[i]]) < 2) {
    indexRows[[counter]] <- i
    counter <- counter+1
  }
}
Background_GBIF <- Background_GBIF[!Background_GBIF$rowNum %in% indexRows, ] %>%
  dplyr::select(-rowNum)

Background_GBIF %<>% mutate(source = "GBIF")


#------------------------------------------------------
# Clean Atlas of Living Australia background for Culex annulirostris
#------------------------------------------------------
Background_ALA <- Background_ALA_Raw %>%
  filter(!species == "" | (!genus == "" & !specificEpithet == ""),
         !is.na(decimalLongitude),
         !is.na(decimalLatitude),
         basisOfRecord != "FOSSIL_SPECIMEN",
         basisOfRecord != "UNKNOWN",
         year >= 2000 & year <= 2019,
         is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 1000) %>%
  dplyr::mutate(rowNum = row_number()) %>%
  dplyr::select(species, genus, specificEpithet, decimalLongitude, decimalLatitude, year, month, rowNum)
Background_ALA$species <- ifelse(!Background_ALA$species == "", Background_ALA$species,
                                 paste(Background_ALA$genus, Background_ALA$specificEpithet))

indexRows <- list()
counter <- 1
for(i in 1:nrow(Background_ALA)) {
  if(decimalNums(Background_ALA$decimalLongitude[[i]]) < 2 &
     decimalNums(Background_ALA$decimalLatitude[[i]]) < 2) {
    indexRows[[counter]] <- i
    counter <- counter+1
  }
}
Background_ALA <- Background_ALA[!Background_ALA$rowNum %in% indexRows, ] %>%
  dplyr::select(-rowNum) %>%
  dplyr::select(c(1,4:5,8,6:7))

Background_ALA %<>% mutate(source = "Atlas of Living Australia")


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


#------------------------------------------------------
# Combine the ALA, Sinka, and Wiebe datasets with GBIF
#------------------------------------------------------
Background_Insecta <- rbind(Background_GBIF, Background_ALA, Background_Sinka, Mosquitoes_Wiebe)



#------------------------------------------------------
## CREATE WEIGHTED BIAS MASK ##
#------------------------------------------------------
#------------------------------------------------------
# Read in template raster and list setup
#------------------------------------------------------
rast <- raster("EVIM.tif")

bg_species_list <- c("Main",
                     "An_gambiae",
                     "An_stephensi",
                     "Cx_annuli")


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
  if(bg_species_list[i] == "Cx_annuli") {
    Background <- Background_Insecta %>% filter(source %in% c("GBIF","Atlas of Living Australia"))
  }
  
  bg_longlat <- Background %>% dplyr::select(c(decimalLongitude, decimalLatitude)) %>%
    as.matrix()
  
  bg_cells <- cellFromXY(rast, bg_longlat) %>% as.data.frame() %>%
    mutate(count = 1) %>% setNames(c("cell","count")) %>%
    group_by(cell) %>% dplyr::summarize(count = sum(count)) %>%
    arrange(desc(count)) %>%
    mutate(longitude = xFromCell(rast, cell),
           latitude = yFromCell(rast, cell))
  
  save_name <- paste0("bg_cells_",bg_species_list[i])
  assign(paste0(save_name), bg_cells)
}


#------------------------------------------------------
# Save RDS file for each weighted bias mask
#------------------------------------------------------
saveRDS(bg_cells_Main, file = "Background_Mask_Main.RDS")
saveRDS(bg_cells_An_gambiae, file = "Background_Mask_An_Gambiae.RDS")
saveRDS(bg_cells_An_stephensi, file = "Background_Mask_An_Stephensi.RDS")
saveRDS(bg_cells_Cx_annuli, file = "Background_Mask_Cx_Annuli.RDS")



