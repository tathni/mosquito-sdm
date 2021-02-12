# This script cleans the raw GBIF species occurrence datasets before the SDM

# Setup
library(dplyr)
setwd("E:/SynologyDrive/Tejas_Server/! Research/! Mordecai Lab/! Mosquito SDM MaxEnt Mechanistic/")


# Define a decimal places function for filtering by lat/long reporting precision uncertainty
decimalNums <- function(x) {
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}


# Read in all mosquitoes of Culicidae for background points
Mosquitoes_All_Raw <- read.csv("GBIF Datasets Raw/AllMosquitoes_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F)


# Read in mosquito species of interest datasets (GBIF and Sinka)
Mosquitoes_SpeciesOfInterest_Raw <- rbind(
  read.csv("GBIF Datasets Raw/AedesAegypti_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F),
  read.csv("GBIF Datasets Raw/AedesAlbopictus_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F),
  read.csv("GBIF Datasets Raw/AnophelesGambiae_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F),
  read.csv("GBIF Datasets Raw/AnophelesStephensi_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F),
  read.csv("GBIF Datasets Raw/CulexPipiens_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F),
  read.csv("GBIF Datasets Raw/CulexQuinquefasciatus_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F),
  read.csv("GBIF Datasets Raw/CulexTarsalis_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F)
)

Mosquitoes_Sinka_Raw <- read.csv("GBIF Datasets Raw/AnophelesStephensi_Sinka2020.csv", sep = ",", header = T, stringsAsFactors = F)


# Filter occurrences from 2000-2019, and by <= 1000m uncertainty or >= 2 decimal points for lat/long reporting precision uncertainty
Mosquitoes_All <- Mosquitoes_All_Raw[!is.na(Mosquitoes_All_Raw$decimalLatitude),] %>%
  mutate(rowNum = 1:nrow(.)) %>%
  filter(basisOfRecord != "FOSSIL_SPECIMEN" &
           basisOfRecord != " LITERATURE" &
           species != "") %>%
  filter(is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 1000) %>%
  filter(year >= 2000 & year <= 2019) %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode, year, rowNum)

indexRows <- list()
counter <- 1
for(i in 1:nrow(Mosquitoes_All)) {
  if(decimalNums(Mosquitoes_All$decimalLongitude[[i]]) < 2 &
     decimalNums(Mosquitoes_All$decimalLatitude[[i]]) < 2) {
    indexRows[[counter]] <- i
    counter <- counter+1
  }
}

Mosquitoes_All <- Mosquitoes_All[!Mosquitoes_All$rowNum %in% indexRows, ] %>%
  dplyr::select(-rowNum)



Mosquitoes_SpeciesOfInterest <- Mosquitoes_SpeciesOfInterest_Raw[!is.na(Mosquitoes_SpeciesOfInterest_Raw$decimalLatitude),] %>%
  mutate(rowNum = 1:nrow(.)) %>%
  filter(basisOfRecord != "FOSSIL_SPECIMEN" &
           basisOfRecord != " LITERATURE" &
           species != "") %>%
  filter(is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 1000) %>%
  filter(year >= 2000 & year <= 2019) %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode, year, rowNum)

indexRows <- list()
counter <- 1
for(i in 1:nrow(Mosquitoes_SpeciesOfInterest)) {
  if(decimalNums(Mosquitoes_SpeciesOfInterest$decimalLongitude[[i]]) < 2 &
     decimalNums(Mosquitoes_SpeciesOfInterest$decimalLatitude[[i]]) < 2) {
    indexRows[[counter]] <- i
    counter <- counter+1
  }
}

Mosquitoes_SpeciesOfInterest <- Mosquitoes_SpeciesOfInterest[!Mosquitoes_SpeciesOfInterest$rowNum %in% indexRows, ] %>%
  dplyr::select(-rowNum)



# Sinka dataset for Anopheles stephensi
Mosquitoes_Sinka <- Mosquitoes_Sinka_Raw %>%
  filter((year_start >= 2000 & year_end <= 2019) | (is.na(year_start) & publication_year >= 2010)) %>%
  filter (country != "Sudan" & country != "Djibouti" & country != "Ethiopia" & country != "Ehiopia") %>%
  dplyr::select(latitude, longitude, year_start, country) %>%
  mutate(species = "Anopheles stephensi")

Mosquitoes_Sinka$country[Mosquitoes_Sinka$country == "Iran"] <- "IR"
Mosquitoes_Sinka$country[Mosquitoes_Sinka$country == "Pakistan"] <- "PK"
Mosquitoes_Sinka$country[Mosquitoes_Sinka$country == "India"] <- "IN"
Mosquitoes_Sinka$country[Mosquitoes_Sinka$country == "Thailand"] <- "TH"
Mosquitoes_Sinka$country[Mosquitoes_Sinka$country == "China"] <- "CN"
Mosquitoes_Sinka$country[Mosquitoes_Sinka$country == "Myanmar"] <- "MM"
Mosquitoes_Sinka$country[Mosquitoes_Sinka$country == "Saudi Arabia"] <- "SA"
Mosquitoes_Sinka$country[Mosquitoes_Sinka$country == "Qatar"] <- "QA"
Mosquitoes_Sinka$country[Mosquitoes_Sinka$country == "Sri Lanka"] <- "LK"

names(Mosquitoes_Sinka)[names(Mosquitoes_Sinka) == "latitude"] <- "decimalLatitude"
names(Mosquitoes_Sinka)[names(Mosquitoes_Sinka) == "longitude"] <- "decimalLongitude"
names(Mosquitoes_Sinka)[names(Mosquitoes_Sinka) == "year_start"] <- "year"
names(Mosquitoes_Sinka)[names(Mosquitoes_Sinka) == "country"] <- "countryCode"

Mosquitoes_Sinka <- Mosquitoes_Sinka[, colnames(Mosquitoes_Sinka)[c(5,2,1,4,3)]]



# Combine the GBIF and Sinka datasets
Mosquitoes_All <- rbind(Mosquitoes_All, Mosquitoes_Sinka)
Mosquitoes_SpeciesOfInterest <- rbind(Mosquitoes_SpeciesOfInterest, Mosquitoes_Sinka)


# Write CSV files containing cleaned datasets for use in the SDM
write.csv(Mosquitoes_All, file = "GBIF Datasets Cleaned/Mosquitoes_All_Cleaned.csv", row.names = F)
write.csv(Mosquitoes_SpeciesOfInterest, file = "GBIF Datasets Cleaned/Mosquitoes_SpeciesOfInterest_Cleaned.csv", row.names = F)


