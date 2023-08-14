#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Configuration file for all scripts
#######################################################


#------------------------------------------------------
# Load general utility and data handling libraries
#------------------------------------------------------
library(tidyverse)
library(dplyr)
library(plyr)
library(magrittr)
library(tidyr)
library(stringr)
library(readxl)
library(beepr)
library(here)
library(data.table)


#------------------------------------------------------
# Load raster, geospatial, and species distribution modeling libraries
#------------------------------------------------------
library(raster)
library(maptools)
library(dismo)
#library(rJava)
#library(enmSdm)
library(maxnet)
library(rgdal)
library(sf)
library(sp)
library(rgeos)
library(spThin)
library(spdep)
library(geosphere)


#------------------------------------------------------
# Load XGBoost machine learning, plotting, and visualization libraries
#------------------------------------------------------
library(xgboost)
library(rBayesianOptimization)
library(foreach)
library(ggplot2)
library(ggpubr)
library(grid)
library(gridExtra)
library(corrplot)
library(pdp)
library(pROC)
library(plotROC)
library(vip)
library(Ckmeans.1d.dp)


#------------------------------------------------------
# Load map data and set crs, memory limit, and seed
#------------------------------------------------------
data(wrld_simpl)
my_crs <- "+proj=longlat +datum=WGS84 +no_defs"
seedNum <- 250
set.seed(seedNum)


#------------------------------------------------------
# Create dataframes for species of interest and activity seasons
#------------------------------------------------------
species_inds <- 1:7

SpeciesOfInterest_Names <- c("Aedes aegypti",
                             "Aedes albopictus",
                             "Anopheles gambiae",
                             "Anopheles stephensi",
                             "Culex pipiens",
                             "Culex quinquefasciatus",
                             "Culex tarsalis")

SpeciesOfInterest_NoSpace <- c("AedesAegypti",
                               "AedesAlbopictus",
                               "AnophelesGambiae",
                               "AnophelesStephensi",
                               "CulexPipiens",
                               "CulexQuinquefasciatus",
                               "CulexTarsalis")

SpeciesOfInterest_Underscore <- c("Aedes_aegypti",
                                  "Aedes_albopictus",
                                  "Anopheles_gambiae",
                                  "Anopheles_stephensi",
                                  "Culex_pipiens",
                                  "Culex_quinquefasciatus",
                                  "Culex_tarsalis")

ActivitySeason_Type <- c("None- Year Round",
                         "Photoperiod",
                         "Precipitation",
                         "None- Year Round",
                         "Photoperiod",
                         "None- Year Round",
                         "Photoperiod")


#------------------------------------------------------
# Define local project directory for data load-in
#------------------------------------------------------
project_dir <- "D:/SynologyDrive/Tejas_Local/! Research/! Mordecai Lab/! Mosquito SDM Thermal Dependence/"
setwd(project_dir)


#------------------------------------------------------
# Define a decimal places function for filtering by latitude/longitude reporting precision uncertainty
#------------------------------------------------------
decimalNums <- function(x) {
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(format(x, scientific=FALSE, digits=20))), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}

options(scipen = 100000)

