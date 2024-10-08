#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Clean the raw GBIF, Atlas of Living Australia (ALA), Sinka, and Wiebe species occurrence data
#######################################################

source("C:/Users/tejas/Documents/GitHub/mosquito-sdm/0-config.R")


#------------------------------------------------------
## SPECIES OF INTEREST CLEANING ##
#------------------------------------------------------
#------------------------------------------------------
# Read in distinct mosquito species datasets from GBIF
#------------------------------------------------------
Mosquitoes_SpeciesOfInterest_Raw <- rbind(
  read.csv("GBIF Datasets Raw/AedesAegypti_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F),
  read.csv("GBIF Datasets Raw/AedesAlbopictus_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F),
  read.csv("GBIF Datasets Raw/AnophelesGambiae_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F),
  read.csv("GBIF Datasets Raw/AnophelesStephensi_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F),
  read.csv("GBIF Datasets Raw/CulexPipiens_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F),
  read.csv("GBIF Datasets Raw/CulexQuinquefasciatus_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F),
  read.csv("GBIF Datasets Raw/CulexTarsalis_Raw.csv", sep = "\t", header = T, encoding = "UTF-8", stringsAsFactors = F)
)


#------------------------------------------------------
# For species with low occurrences, bulk up species with fewest occurrences from Sinka and Wiebe
#------------------------------------------------------
Mosquitoes_Sinka_Raw <- read.csv("GBIF Datasets Raw/AnophelesStephensi_Sinka2020.csv", sep = ",", header = T, stringsAsFactors = F)
Mosquitoes_Wiebe_Raw <- read_excel("GBIF Datasets Raw/AnophelesGambiae_Wiebe2017.xlsx", sheet=7)


#------------------------------------------------------
# Create dataframe shell to house filter metadata by species
#------------------------------------------------------
filter_metadata_pre <- data.frame(matrix(ncol = 7, nrow=8))
colnames(filter_metadata_pre) <- c("Species","Raw_Occurrences_with_Coords","Non_Fossil_BOR","Non_Unknown_BOR",
                               "Year_Range","Coord_Uncertainty_Reported","Coord_Uncertainty_DecPlace")


#------------------------------------------------------
# Filter GBIF species of interest from 2000-2019, by <= 1000m uncertainty, and by >= 2 decimal points
#------------------------------------------------------
Mosquitoes_SpeciesOfInterest <- Mosquitoes_SpeciesOfInterest_Raw %>%
  filter(!species == "",
         !is.na(decimalLongitude), !is.na(decimalLatitude))
soi_raw <- Mosquitoes_SpeciesOfInterest  # At each step of the filtering, save a copy of the data for plotting/metrics

Mosquitoes_SpeciesOfInterest %<>%
  filter(basisOfRecord != "FOSSIL_SPECIMEN")
soi_nonfossil_bor <- Mosquitoes_SpeciesOfInterest

Mosquitoes_SpeciesOfInterest %<>%
  filter(basisOfRecord != "UNKNOWN")
soi_nonunknown_bor <- Mosquitoes_SpeciesOfInterest

Mosquitoes_SpeciesOfInterest %<>%
  filter(year >= 2000 & year <= 2019)
soi_yearrange <- Mosquitoes_SpeciesOfInterest

Mosquitoes_SpeciesOfInterest %<>%
  filter(is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 1000)
soi_coord_reported <- Mosquitoes_SpeciesOfInterest

Mosquitoes_SpeciesOfInterest %<>%
  dplyr::mutate(rowNum = row_number()) %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode, year, month, rowNum)
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
soi_coord_decplace <- Mosquitoes_SpeciesOfInterest

Mosquitoes_SpeciesOfInterest %<>% mutate(source = "GBIF")


#------------------------------------------------------
# Clean the Sinka dataset for Anopheles stephensi, excluding occurrences from invasive range (i.e., Africa)
#------------------------------------------------------
Mosquitoes_Sinka <- Mosquitoes_Sinka_Raw %>%
  filter(!is.na(longitude), !is.na(latitude))
sinka_raw <- Mosquitoes_Sinka
sinka_nonfossil_bor <- Mosquitoes_Sinka

# Only Sinka requires a non-native range filter; classify as "nonunknown BOR" to simplify variables
Mosquitoes_Sinka %<>%
  filter(!country %in% c("Sudan","Djibouti","Ethiopia","Ehiopia"))
sinka_nonunknown_bor <- Mosquitoes_Sinka  

# On average, the time from study completion --> publication takes 4 years for the studies in the Sinka dataset
mean(Mosquitoes_Sinka$publication_year - Mosquitoes_Sinka$year_end, na.rm=T) %>% round(0)

# Thus, for studies without a reported "year_end", will only include points if publication year is >= 2004
# That is, our lower bound for the study inclusion criteria year range (2000) plus 4
Mosquitoes_Sinka %<>%
  filter(year_start >= 2000 & year_end <= 2019 |
           is.na(year_end) & publication_year >= 2004)
sinka_yearrange <- Mosquitoes_Sinka
sinka_coord_reported <- Mosquitoes_Sinka

Mosquitoes_Sinka %<>%
  dplyr::mutate(rowNum = row_number()) %>%
  dplyr::select(longitude, latitude, year_start, country, rowNum) %>%
  dplyr::mutate(species = "Anopheles stephensi",
                month = NA)

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
  dplyr::select(c(5,1:2,4,3,6))
sinka_coord_decplace <- Mosquitoes_Sinka

Mosquitoes_Sinka %<>% mutate(source = "Sinka et al., 2020")


#------------------------------------------------------
# Clean the Wiebe dataset for Anopheles gambiae
#------------------------------------------------------
Mosquitoes_Wiebe <- Mosquitoes_Wiebe_Raw %>%
  filter(!is.na(Longitude), !is.na(Latitude))
wiebe_raw <- Mosquitoes_Wiebe

Mosquitoes_Wiebe %<>%
  filter(Start_Year >= 2000 & Start_Year <= 2019)
wiebe_yearrange <- Mosquitoes_Wiebe

Mosquitoes_Wiebe %<>%
  dplyr::mutate(rowNum = row_number(),
                month = NA,
                species = "Anopheles gambiae") %>%
  dplyr::select(species, Longitude, Latitude, Country, Start_Year, month, rowNum)
colnames(Mosquitoes_Wiebe) <- c("species","decimalLongitude","decimalLatitude","country","year","month","rowNum")

indexRows <- list()
counter <- 1
for(i in 1:nrow(Mosquitoes_Wiebe)) {
  if(decimalNums(Mosquitoes_Wiebe$decimalLongitude[[i]]) < 2 &
     decimalNums(Mosquitoes_Wiebe$decimalLatitude[[i]]) < 2) {
    indexRows[[counter]] <- i
    counter <- counter+1
  }
}

Mosquitoes_Wiebe <- Mosquitoes_Wiebe[!Mosquitoes_Wiebe$rowNum %in% indexRows, ] %>%
  dplyr::select(-rowNum)
wiebe_coord_decplace <- Mosquitoes_Wiebe

Mosquitoes_Wiebe %<>% mutate(source = "Wiebe et al., 2017")



#------------------------------------------------------
##  MERGING AND EXPORT ##
#------------------------------------------------------
#------------------------------------------------------
# Combine the ALA, Sinka, and Wiebe datasets with species of interest
#------------------------------------------------------
Mosquitoes_SpeciesOfInterest <- rbind(Mosquitoes_SpeciesOfInterest, Mosquitoes_ALA, Mosquitoes_Sinka,
                                      Mosquitoes_Wiebe)


#------------------------------------------------------
# Write CSV file containing cleaned dataset for use in modeling
#------------------------------------------------------
write.csv(Mosquitoes_SpeciesOfInterest, file = "GBIF_Datasets_Cleaned/Mosquitoes_SpeciesOfInterest.csv", row.names = F)



#------------------------------------------------------
## FILTER METADATA ##
#------------------------------------------------------
#------------------------------------------------------
# Aedes aegypti
#------------------------------------------------------
filter_metadata_pre[[1]][[1]] <- "Aedes aegypti"
filter_metadata_pre[[2]][[1]] <- nrow(soi_raw %>% filter(species == "Aedes aegypti"))
filter_metadata_pre[[3]][[1]] <- nrow(soi_nonfossil_bor %>% filter(species == "Aedes aegypti"))
filter_metadata_pre[[4]][[1]] <- nrow(soi_nonunknown_bor %>% filter(species == "Aedes aegypti"))
filter_metadata_pre[[5]][[1]] <- nrow(soi_yearrange %>% filter(species == "Aedes aegypti"))
filter_metadata_pre[[6]][[1]] <- nrow(soi_coord_reported %>% filter(species == "Aedes aegypti"))
filter_metadata_pre[[7]][[1]] <- nrow(soi_coord_decplace %>% filter(species == "Aedes aegypti"))


#------------------------------------------------------
# Aedes albopictus
#------------------------------------------------------
filter_metadata_pre[[1]][[2]] <- "Aedes albopictus"
filter_metadata_pre[[2]][[2]] <- nrow(soi_raw %>% filter(species == "Aedes albopictus"))
filter_metadata_pre[[3]][[2]] <- nrow(soi_nonfossil_bor %>% filter(species == "Aedes albopictus"))
filter_metadata_pre[[4]][[2]] <- nrow(soi_nonunknown_bor %>% filter(species == "Aedes albopictus"))
filter_metadata_pre[[5]][[2]] <- nrow(soi_yearrange %>% filter(species == "Aedes albopictus"))
filter_metadata_pre[[6]][[2]] <- nrow(soi_coord_reported %>% filter(species == "Aedes albopictus"))
filter_metadata_pre[[7]][[2]] <- nrow(soi_coord_decplace %>% filter(species == "Aedes albopictus"))


#------------------------------------------------------
# Anopheles gambiae
#------------------------------------------------------
filter_metadata_pre[[1]][[3]] <- "Anopheles gambiae"
filter_metadata_pre[[2]][[3]] <- nrow(soi_raw %>% filter(species == "Anopheles gambiae")) + nrow(wiebe_raw)
filter_metadata_pre[[3]][[3]] <- nrow(soi_nonfossil_bor %>% filter(species == "Anopheles gambiae")) + nrow(wiebe_raw)
filter_metadata_pre[[4]][[3]] <- nrow(soi_nonunknown_bor %>% filter(species == "Anopheles gambiae")) + nrow(wiebe_raw)
filter_metadata_pre[[5]][[3]] <- nrow(soi_yearrange %>% filter(species == "Anopheles gambiae")) + nrow(wiebe_yearrange)
filter_metadata_pre[[6]][[3]] <- nrow(soi_coord_reported %>% filter(species == "Anopheles gambiae")) + nrow(wiebe_yearrange)
filter_metadata_pre[[7]][[3]] <- nrow(soi_coord_decplace %>% filter(species == "Anopheles gambiae")) + nrow(wiebe_coord_decplace)


#------------------------------------------------------
# Anopheles stephensi
#------------------------------------------------------
filter_metadata_pre[[1]][[4]] <- "Anopheles stephensi"
filter_metadata_pre[[2]][[4]] <- nrow(soi_raw %>% filter(species == "Anopheles stephensi")) + nrow(sinka_raw)
filter_metadata_pre[[3]][[4]] <- nrow(soi_nonfossil_bor %>% filter(species == "Anopheles stephensi")) + nrow(sinka_nonfossil_bor)
filter_metadata_pre[[4]][[4]] <- nrow(soi_nonunknown_bor %>% filter(species == "Anopheles stephensi")) + nrow(sinka_nonunknown_bor)
filter_metadata_pre[[5]][[4]] <- nrow(soi_yearrange %>% filter(species == "Anopheles stephensi")) + nrow(sinka_yearrange)
filter_metadata_pre[[6]][[4]] <- nrow(soi_coord_reported %>% filter(species == "Anopheles stephensi")) + nrow(sinka_coord_reported)
filter_metadata_pre[[7]][[4]] <- nrow(soi_coord_decplace %>% filter(species == "Anopheles stephensi")) + nrow(sinka_coord_decplace)


#------------------------------------------------------
# Culex pipiens
#------------------------------------------------------
filter_metadata_pre[[1]][[6]] <- "Culex pipiens"
filter_metadata_pre[[2]][[6]] <- nrow(soi_raw %>% filter(species == "Culex pipiens"))
filter_metadata_pre[[3]][[6]] <- nrow(soi_nonfossil_bor %>% filter(species == "Culex pipiens"))
filter_metadata_pre[[4]][[6]] <- nrow(soi_nonunknown_bor %>% filter(species == "Culex pipiens"))
filter_metadata_pre[[5]][[6]] <- nrow(soi_yearrange %>% filter(species == "Culex pipiens"))
filter_metadata_pre[[6]][[6]] <- nrow(soi_coord_reported %>% filter(species == "Culex pipiens"))
filter_metadata_pre[[7]][[6]] <- nrow(soi_coord_decplace %>% filter(species == "Culex pipiens"))


#------------------------------------------------------
# Culex quinquefasciatus
#------------------------------------------------------
filter_metadata_pre[[1]][[7]] <- "Culex quinquefasciatus"
filter_metadata_pre[[2]][[7]] <- nrow(soi_raw %>% filter(species == "Culex quinquefasciatus"))
filter_metadata_pre[[3]][[7]] <- nrow(soi_nonfossil_bor %>% filter(species == "Culex quinquefasciatus"))
filter_metadata_pre[[4]][[7]] <- nrow(soi_nonunknown_bor %>% filter(species == "Culex quinquefasciatus"))
filter_metadata_pre[[5]][[7]] <- nrow(soi_yearrange %>% filter(species == "Culex quinquefasciatus"))
filter_metadata_pre[[6]][[7]] <- nrow(soi_coord_reported %>% filter(species == "Culex quinquefasciatus"))
filter_metadata_pre[[7]][[7]] <- nrow(soi_coord_decplace %>% filter(species == "Culex quinquefasciatus"))


#------------------------------------------------------
# Culex tarsalis
#------------------------------------------------------
filter_metadata_pre[[1]][[8]] <- "Culex tarsalis"
filter_metadata_pre[[2]][[8]] <- nrow(soi_raw %>% filter(species == "Culex tarsalis"))
filter_metadata_pre[[3]][[8]] <- nrow(soi_nonfossil_bor %>% filter(species == "Culex tarsalis"))
filter_metadata_pre[[4]][[8]] <- nrow(soi_nonunknown_bor %>% filter(species == "Culex tarsalis"))
filter_metadata_pre[[5]][[8]] <- nrow(soi_yearrange %>% filter(species == "Culex tarsalis"))
filter_metadata_pre[[6]][[8]] <- nrow(soi_coord_reported %>% filter(species == "Culex tarsalis"))
filter_metadata_pre[[7]][[8]] <- nrow(soi_coord_decplace %>% filter(species == "Culex tarsalis"))


#------------------------------------------------------
# Save the data file, which will complete the filter metadata in a later script
#------------------------------------------------------
saveRDS(filter_metadata_pre, "Metadata/filter_metadata_pre.RDS")


