# This script cleans the raw species occurrence datasets before the SDM

# Setup
library(dplyr)
setwd("E:/SynologyDrive/Tejas_Server/! Research/! Mordecai Lab/! Mosquito SDM MaxEnt Mechanistic/")


# Read in all mosquitoes of Culicidae for background points
Mosquitoes_All_Raw <- read.csv("GBIF Datasets Raw/AllMosquitoes_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F)


# Read in mosquito species of interest datasets (GBIF, Atlas of Living Australia, and Dryad)
Mosquitoes_SpeciesOfInterest_Raw <- rbind(
  read.csv("GBIF Datasets Raw/AedesAegypti_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F),
  read.csv("GBIF Datasets Raw/AedesAlbopictus_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F),
  read.csv("GBIF Datasets Raw/AnophelesGambiae_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F),
  read.csv("GBIF Datasets Raw/AnophelesStephensi_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F),
  read.csv("GBIF Datasets Raw/CulexPipiens_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F),
  read.csv("GBIF Datasets Raw/CulexQuinquefasciatus_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F),
  read.csv("GBIF Datasets Raw/CulexTarsalis_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F)
)


# Clean occurrence data by relevant parameters and filter occurrences from 2000-2020
Mosquitoes_All <- Mosquitoes_All_Raw %>%
  filter(basisOfRecord != "FOSSIL_SPECIMEN" &
           basisOfRecord != " LITERATURE" &
           species != "") %>%
  filter(is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 1000) %>%
  filter(year >= 2000 & year <= 2019) %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode, year, month, day, identifiedBy)



Mosquitoes_SpeciesOfInterest <- Mosquitoes_SpeciesOfInterest_Raw %>%
  filter(basisOfRecord != "FOSSIL_SPECIMEN" &
           basisOfRecord != " LITERATURE" &
           species != "") %>%
  filter(is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 1000) %>%
  filter(year >= 2000 & year <= 2019) %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode, year, month, day, identifiedBy)




# Write CSV files containing cleaned datasets for use in the SDM
write.csv(Mosquitoes_All, file = "GBIF Datasets Cleaned/Mosquitoes_All_Cleaned.csv", row.names = F)

write.csv(Mosquitoes_SpeciesOfInterest, file = "GBIF Datasets Cleaned/Mosquitoes_SpeciesOfInterest_Cleaned.csv", row.names = F)


