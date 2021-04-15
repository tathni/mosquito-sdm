# This script cleans the raw GBIF, Atlas of Living Australia, and Sinka species occurrence datasets before modeling

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


# Read in mosquito species of interest datasets (GBIF, Atlas of Living Australia, and Sinka)
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


# Filter occurrences from 2000-2019, and by <= 1000m uncertainty or >= 2 decimal points for lat/long reporting precision uncertainty
Mosquitoes_All <- Mosquitoes_All_Raw[!is.na(Mosquitoes_All_Raw$decimalLatitude),] %>%
  dplyr::mutate(rowNum = row_number()) %>%
  filter(basisOfRecord != "FOSSIL_SPECIMEN" &
           basisOfRecord != " LITERATURE" &
           species != "") %>%
  filter(is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 1000) %>%
  filter(year >= 2000 & year <= 2019) %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode, year, month, day, identifiedBy, rowNum)
# Assign unique "identifiedBy" ID for NAs and blanks to prevent unnecessary filtering of duplicates when R perceives NA == NA
Mosquitoes_All$identifiedBy[Mosquitoes_All$identifiedBy == ""] <- sample(1000000,  
                                                                         size = sum(Mosquitoes_All$identifiedBy == ""),
                                                                         replace = TRUE)
Mosquitoes_All$identifiedBy[is.na(Mosquitoes_All$identifiedBy)] <- sample(1000000,
                                                                          size = sum(is.na(Mosquitoes_All$identifiedBy)),
                                                                          replace = TRUE)

names(Mosquitoes_All)[names(Mosquitoes_All) == "countryCode"] <- "country"

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
  dplyr::mutate(rowNum = row_number()) %>%
  filter(basisOfRecord != "FOSSIL_SPECIMEN" &
           basisOfRecord != " LITERATURE" &
           species != "") %>%
  filter(is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 1000) %>%
  filter(year >= 2000 & year <= 2019) %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode, year, month, day, identifiedBy, rowNum)
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




# Clean the Atlas of Living Australia dataset for Culex annulirostris
# Exceptions for small n: coordinateUncertainty threshold of 10,000, is.na(year) filter, year range extended to (1970,2021)
Mosquitoes_AtlasAustralia <- Mosquitoes_AtlasAustralia_Raw[!is.na(Mosquitoes_AtlasAustralia_Raw$decimalLatitude),] %>%
  dplyr::mutate(rowNum = row_number()) %>%
  filter(species != "") %>%
  filter(is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 10000) %>%
  filter(is.na(year) | (year >= 1970 & year <= 2021)) %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, year, month, day, identifiedBy, rowNum) %>%
  dplyr::mutate(country = "AU")
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




# Clean the Sinka dataset for Anopheles stephensi
Mosquitoes_Sinka <- Mosquitoes_Sinka_Raw %>%
  dplyr::mutate(rowNum = row_number()) %>%
  filter((year_start >= 2000 & year_end <= 2019) | (is.na(year_start) & publication_year >= 2010)) %>%
  filter (country != "Sudan" & country != "Djibouti" & country != "Ethiopia" & country != "Ehiopia") %>%
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




# Combine the GBIF and Sinka datasets
Mosquitoes_All <- rbind(Mosquitoes_All, Mosquitoes_AtlasAustralia, Mosquitoes_Sinka)
Mosquitoes_SpeciesOfInterest <- rbind(Mosquitoes_SpeciesOfInterest, Mosquitoes_AtlasAustralia, Mosquitoes_Sinka)



# Assign continent names to both cleaned datasets
countryCodes <- read.csv("Country Codes/country-and-continent-codes.csv", sep = ",", header = TRUE,
                         encoding = "UTF-8", stringsAsFactors = FALSE)

Mosquitoes_All <- Mosquitoes_All %>% 
  dplyr::mutate(continent = countryCodes$Continent_Name[match(Mosquitoes_All$country, countryCodes$Two_Letter_Country_Code)]) %>%
  .[,c(1:4,9,5:8)]

Mosquitoes_SpeciesOfInterest <- Mosquitoes_SpeciesOfInterest %>% 
  dplyr::mutate(continent = countryCodes$Continent_Name[match(Mosquitoes_SpeciesOfInterest$country, countryCodes$Two_Letter_Country_Code)]) %>%
  .[,c(1:4,9,5:8)]

Mosquitoes_AtlasAustralia <- Mosquitoes_AtlasAustralia %>% 
  dplyr::mutate(continent = countryCodes$Continent_Name[match(Mosquitoes_AtlasAustralia$country, countryCodes$Two_Letter_Country_Code)]) %>%
  .[,c(1:4,9,5:8)]


# Filter out duplicate occurrences, if on the same exact day/month/year, location, and identifiedBy
# This gets rid of any overlap between the datasets that have been cross-logged into GBIF
# Key for creating accurate bias/weighted masks for background sampling in the SDM
Mosquitoes_All_Final <- Mosquitoes_All %>%
  subset(!duplicated(subset(Mosquitoes_SpeciesOfInterest, select=c(species, decimalLongitude, decimalLatitude,
                                                                   year, month, day, identifiedBy)))) %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, country, continent, year)

Mosquitoes_SpeciesOfInterest_Final <- rbind(Mosquitoes_SpeciesOfInterest, Mosquitoes_AtlasAustralia) %>%
  subset(!duplicated(subset(Mosquitoes_SpeciesOfInterest, select=c(species, decimalLongitude, decimalLatitude,
                                                                   year, month, day, identifiedBy)))) %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, country, continent, year)



# Write CSV files containing cleaned datasets for use in the SDM
write.csv(Mosquitoes_All, file = "GBIF Datasets Cleaned/Mosquitoes_All_Cleaned.csv", row.names = F)
write.csv(Mosquitoes_SpeciesOfInterest, file = "GBIF Datasets Cleaned/Mosquitoes_SpeciesOfInterest_Cleaned.csv", row.names = F)


