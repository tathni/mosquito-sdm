# This script cleans the raw GBIF, Atlas of Living Australia (ALA), and Sinka species occurrence datasets before modeling

# Setup
library(dplyr)
library(magrittr)
setwd("E:/SynologyDrive/Tejas_Server/! Research/! Mordecai Lab/! Mosquito SDM MaxEnt Mechanistic/")


# Define a decimal places function for filtering by lat/long reporting precision uncertainty
decimalNums <- function(x) {
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}


# Read in data from Culicoidea superfamily (Culicidae, Chaoboridae, Corethrellidae, and Dixidae) for background bias of mosquito sampling effort
Culicidae_Raw <- read.csv("GBIF Datasets Raw/Culicidae_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F)
Chaoboridae_Raw <- read.csv("GBIF Datasets Raw/Chaoboridae_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F)
Corethrellidae_Raw <- read.csv("GBIF Datasets Raw/Corethrellidae_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F)
Dixidae_Raw <- read.csv("GBIF Datasets Raw/Dixidae_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F)
Background_All <- rbind(Culicidae_Raw, Chaoboridae_Raw, Corethrellidae_Raw, Dixidae_Raw)


# Read in mosquito species of interest datasets (GBIF, Atlas of Living Australia, and Sinka, all of which have distinct occurrences)
Mosquitoes_SpeciesOfInterest_Raw <- rbind(
  read.csv("GBIF Datasets Raw/AedesAegypti_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F),
  read.csv("GBIF Datasets Raw/AedesAlbopictus_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F),
  read.csv("GBIF Datasets Raw/AnophelesGambiae_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F),
  read.csv("GBIF Datasets Raw/AnophelesStephensi_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F),
  read.csv("GBIF Datasets Raw/CulexAnnulirostris_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F),
  read.csv("GBIF Datasets Raw/CulexPipiens_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F),
  read.csv("GBIF Datasets Raw/CulexQuinquefasciatus_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F),
  read.csv("GBIF Datasets Raw/CulexTarsalis_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F)
)
Mosquitoes_AtlasAustralia_Raw <- read.csv("GBIF Datasets Raw/CulexAnnulirostris_ALA.csv", sep = ",", header = T, encoding = "UTF-8", stringsAsFactors = F)
Mosquitoes_Sinka_Raw <- read.csv("GBIF Datasets Raw/AnophelesStephensi_Sinka2020.csv", sep = ",", header = T, stringsAsFactors = F)



# Filter background from 2000-2019, and by <= 1000m uncertainty or >= 2 decimal points for lat/long reporting precision uncertainty
Background_All %<>%
  filter(!is.na(decimalLongitude),
         !is.na(decimalLatitude),
         basisOfRecord != "FOSSIL_SPECIMEN",
         basisOfRecord != "LITERATURE",
         basisOfRecord != "UNKNOWN",
         species != "",
         is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 1000,
         year >= 2000 & year <= 2019) %>%
  dplyr::mutate(rowNum = row_number()) %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode, year, month, day, identifiedBy, rowNum)
# Assign unique "identifiedBy" ID for NAs and blanks to prevent unnecessary filtering of duplicates when R perceives NA == NA
Background_All$identifiedBy[Background_All$identifiedBy == ""] <- sample(1000000,  
                                                                         size = sum(Background_All$identifiedBy == ""),
                                                                         replace = TRUE)
Background_All$identifiedBy[is.na(Background_All$identifiedBy)] <- sample(1000000,
                                                                          size = sum(is.na(Background_All$identifiedBy)),
                                                                          replace = TRUE)
names(Background_All)[names(Background_All) == "countryCode"] <- "country"

indexRows <- list()
counter <- 1
for(i in 1:nrow(Background_All)) {
  if(decimalNums(Background_All$decimalLongitude[[i]]) < 2 &
     decimalNums(Background_All$decimalLatitude[[i]]) < 2) {
    indexRows[[counter]] <- i
    counter <- counter+1
  }
}
Background_All <- Background_All[!Background_All$rowNum %in% indexRows, ] %>%
  dplyr::select(-rowNum)



# Filter species of interest from 2000-2019, and by <= 1000m uncertainty or >= 2 decimal points for lat/long reporting precision uncertainty
Mosquitoes_SpeciesOfInterest <- Mosquitoes_SpeciesOfInterest_Raw %>%
  filter(!species == "Culex annulirostris",
         !is.na(decimalLongitude),
         !is.na(decimalLatitude),
         basisOfRecord != "FOSSIL_SPECIMEN",
         basisOfRecord != "LITERATURE",
         basisOfRecord != "UNKNOWN",
         species != "",
         is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 1000,
         year >= 2000 & year <= 2019) %>%
  dplyr::mutate(rowNum = row_number()) %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode, year, month, day, identifiedBy, rowNum)
# Assign unique "identifiedBy" ID for NAs and blanks to prevent unnecessary filtering of duplicates when R perceives NA == NA
Mosquitoes_SpeciesOfInterest$identifiedBy[Mosquitoes_SpeciesOfInterest$identifiedBy == ""] <-
  sample(1000000,
         size = sum(Mosquitoes_SpeciesOfInterest$identifiedBy == ""),
         replace = TRUE)
Mosquitoes_SpeciesOfInterest$identifiedBy[is.na(Mosquitoes_SpeciesOfInterest$identifiedBy)] <-
  sample(1000000,
         size = sum(Mosquitoes_SpeciesOfInterest$identifiedBy == ""),
         replace = TRUE)
names(Mosquitoes_SpeciesOfInterest)[names(Mosquitoes_SpeciesOfInterest) == "countryCode"] <- "country"

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




# Clean the GBIF subset dataset for Culex annulirostris
# Exceptions allowed for small sample size: coordinateUncertainty threshold expanded to <10,000, year is.na(), year range extended to (1970,2021)
Mosquitoes_GBIFAnnulirostris <- Mosquitoes_SpeciesOfInterest_Raw %>%
  filter(species == "Culex annulirostris",
         !is.na(decimalLongitude),
         !is.na(decimalLatitude),
         basisOfRecord != "FOSSIL_SPECIMEN",
         basisOfRecord != "LITERATURE",
         basisOfRecord != "UNKNOWN",
         species != "",
         is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 10000,
         year >= 1970 & year <= 2021) %>%
  dplyr::mutate(rowNum = row_number()) %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode, year, month, day, identifiedBy, rowNum)
# Assign unique "identifiedBy" ID for NAs and blanks to prevent unnecessary filtering of duplicates when R perceives NA == NA
Mosquitoes_GBIFAnnulirostris$identifiedBy[Mosquitoes_GBIFAnnulirostris$identifiedBy == ""] <-
  sample(1000000,
         size = sum(Mosquitoes_GBIFAnnulirostris$identifiedBy == ""),
         replace = TRUE)
Mosquitoes_GBIFAnnulirostris$identifiedBy[is.na(Mosquitoes_GBIFAnnulirostris$identifiedBy)] <-
  sample(1000000,
         size = sum(Mosquitoes_GBIFAnnulirostris$identifiedBy == ""),
         replace = TRUE)
names(Mosquitoes_GBIFAnnulirostris)[names(Mosquitoes_GBIFAnnulirostris) == "countryCode"] <- "country"

indexRows <- list()
counter <- 1
for(i in 1:nrow(Mosquitoes_GBIFAnnulirostris)) {
  if(decimalNums(Mosquitoes_GBIFAnnulirostris$decimalLongitude[[i]]) < 2 &
     decimalNums(Mosquitoes_GBIFAnnulirostris$decimalLatitude[[i]]) < 2) {
    indexRows[[counter]] <- i
    counter <- counter+1
  }
}
Mosquitoes_GBIFAnnulirostris <- Mosquitoes_GBIFAnnulirostris[!Mosquitoes_GBIFAnnulirostris$rowNum %in% indexRows, ] %>%
  dplyr::select(-rowNum)




# Clean the Atlas of Living Australia dataset for Culex annulirostris
# Exceptions allowed for small sample size: coordinateUncertainty threshold expanded to <10,000, year is.na(), year range extended to (1970,2021)
Mosquitoes_AtlasAustralia <- Mosquitoes_AtlasAustralia_Raw %>%
  filter(!is.na(decimalLongitude),
         !is.na(decimalLatitude),
         basisOfRecord != "FOSSIL_SPECIMEN",
         basisOfRecord != "LITERATURE",
         basisOfRecord != "UNKNOWN",
         species != "",
         is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 10000,
         year >= 1970 & year <= 2021) %>%
  dplyr::mutate(rowNum = row_number()) %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, year, month, day, identifiedBy, rowNum) %>%
  dplyr::mutate(country = "AU")
# Assign unique "identifiedBy" ID for NAs and blanks to prevent unnecessary filtering of duplicates when R perceives NA == NA
Mosquitoes_AtlasAustralia$identifiedBy[Mosquitoes_AtlasAustralia$identifiedBy == ""] <-
  sample(1000000,
         size = sum(Mosquitoes_AtlasAustralia$identifiedBy == ""),
         replace = TRUE)
Mosquitoes_AtlasAustralia$identifiedBy[is.na(Mosquitoes_AtlasAustralia$identifiedBy)] <-
  sample(1000000,
         size = sum(is.na(Mosquitoes_AtlasAustralia$identifiedBy)),
         replace = TRUE)

indexRows <- list()
counter <- 1
for(i in 1:nrow(Mosquitoes_AtlasAustralia)) {
  if(decimalNums(Mosquitoes_AtlasAustralia$decimalLongitude[[i]]) < 2 &
     decimalNums(Mosquitoes_AtlasAustralia$decimalLatitude[[i]]) < 2) {
    indexRows[[counter]] <- i
    counter <- counter+1
  }
}
Mosquitoes_AtlasAustralia <- Mosquitoes_AtlasAustralia[!Mosquitoes_AtlasAustralia$rowNum %in% indexRows, ] %>%
  dplyr::select(-rowNum) %>%
  .[,c(1:3,8,4:7)]



# Merge the GBIF Culex annulirostris subset and ALA dataset into a singular dataset for Culex annulirostris
Mosquitoes_CulexAnnulirostris <- rbind(Mosquitoes_GBIFAnnulirostris, Mosquitoes_AtlasAustralia)




# Clean the Sinka dataset for Anopheles stephensi
Mosquitoes_Sinka <- Mosquitoes_Sinka_Raw %>%
  filter(!is.na(longitude),
         !is.na(latitude),
         year_start >= 2000 & year_end <= 2019 |
           is.na(year_start) & publication_year >= 2010,
         !country %in% c("Sudan","Djibouti","Ethiopia","Ehiopia")) %>%
  dplyr::mutate(rowNum = row_number()) %>%
  dplyr::select(longitude, latitude, year_start, country, rowNum) %>%
  dplyr::mutate(species = "Anopheles stephensi",
         month = NA,
         day = NA,
         identifiedBy = sample(1000000,
                               size = nrow(.),
                               replace = TRUE))

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

indexRows <- list()
counter <- 1
for(i in 1:nrow(Mosquitoes_Sinka)) {
  if(decimalNums(Mosquitoes_Sinka$decimalLongitude[[i]]) < 2 &
     decimalNums(Mosquitoes_Sinka$decimalLatitude[[i]]) < 2) {
    indexRows[[counter]] <- i
    counter <- counter+1
  }
}
Mosquitoes_Sinka <- Mosquitoes_Sinka[!Mosquitoes_Sinka$rowNum %in% indexRows, ] %>%
  dplyr::select(-rowNum) %>%
  .[,c(5,1:2,4,3,6:8)]




# Combine the GBIF, Culex annulirostris, and Sinka datasets
Background_All <- rbind(Background_All, Mosquitoes_CulexAnnulirostris, Mosquitoes_Sinka)
Mosquitoes_SpeciesOfInterest <- rbind(Mosquitoes_SpeciesOfInterest, Mosquitoes_CulexAnnulirostris, Mosquitoes_Sinka)



# Assign continent names to both cleaned datasets
countryCodes <- read.csv("Country Codes/country-and-continent-codes.csv", sep = ",", header = TRUE,
                         encoding = "UTF-8", stringsAsFactors = FALSE)

Background_All <- Background_All %>% 
  dplyr::mutate(continent = countryCodes$Continent_Name[match(Background_All$country, countryCodes$Two_Letter_Country_Code)]) %>%
  .[,c(1:4,9,5:8)]

Mosquitoes_SpeciesOfInterest <- Mosquitoes_SpeciesOfInterest %>% 
  dplyr::mutate(continent = countryCodes$Continent_Name[match(Mosquitoes_SpeciesOfInterest$country, countryCodes$Two_Letter_Country_Code)]) %>%
  .[,c(1:4,9,5:8)]



# Write CSV files containing cleaned datasets for use in the SDM
write.csv(Background_All, file = "GBIF Datasets Cleaned/Background_All_Cleaned.csv", row.names = F)
write.csv(Mosquitoes_SpeciesOfInterest, file = "GBIF Datasets Cleaned/Mosquitoes_SpeciesOfInterest_Cleaned.csv", row.names = F)


