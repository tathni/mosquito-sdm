#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Gather filter metadata for each mosquito species' occurrence points as they pass through sieves of filters
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
# Load in environmental predictors and stack by activity season
#------------------------------------------------------
predictors_preStack <- alply(list.files("Environmental Predictors Merged",
                                        pattern = ".tif",
                                        full.names = TRUE), 1, function(file){
                                          print(file)
                                          rast <- raster(file)
                                          return(rast)
                                        })
rasterNames <- c("CD","EVIM","EVISD","FC","HPD","PDQ","PhotoASTM","PhotoASTSD","PrecipASTM","PrecipASTSD","PWQ","SW","TAM","TASD","WS")
predictors_preStack <- setNames(predictors_preStack, rasterNames)

predictors_yearRound <- predictors_preStack[c(1:6,11,14,12:13)] %>% stack()
predictors_photoSeason <- predictors_preStack[c(1:6,11,14,7:8)] %>% stack()
predictors_precipSeason <- predictors_preStack[c(1:6,11,14,9:10)] %>% stack()


#------------------------------------------------------
# ?? landmass mask
#------------------------------------------------------


#------------------------------------------------------
# Load in activity season length masks
#------------------------------------------------------
activity_lengths <- alply(list.files("Filter Masks/Activity Season Lengths",
                                     pattern = ".tif",
                                     full.names = TRUE), 1, function(file){
                                       print(file)
                                       rast <- raster(file)
                                       return(rast)
                                     }) %>%
  setNames(c("Photoperiod","Precipitation"))
activity_lengths_index <- c(NA,1,2,NA,NA,1,NA,1)


#------------------------------------------------------
# ?? covariates non-NA mask
#------------------------------------------------------


#------------------------------------------------------
# ?? unique cell centroids
#------------------------------------------------------


#------------------------------------------------------
# Create lists and containers to house filter metadata
#------------------------------------------------------
filter_metadata_pre <- readRDS("Metadata/filter_metadata_pre.RDS")
filter_metadata_shell <- data.frame(matrix(ncol = 4, nrow=8))
colnames(filter_metadata_shell) <- c("Activity_Season","Unique_NonNA_Cells","Activity_Season_Cells","Covariates_NonNA_Cells")
filter_metadata <- cbind(filter_metadata_pre, filter_metadata_shell) %>%
  .[,c(1,8,2:7,9:11)]




## ??? main header



#------------------------------------------------------
# Export .csv file
#------------------------------------------------------
write.csv(filter_metadata, file = "Metadata/Filter Metadata by Species.csv", row.names=FALSE)


