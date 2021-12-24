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
library(rJava)
library(enmSdm)
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
library(ggplot2)
library(corrplot)
library(pdp)
library(pROC)
library(plotROC)
library(foreach)
library(vip)
library(rBayesianOptimization)


#------------------------------------------------------
# Load map data, set memory limit, and set the seed
#------------------------------------------------------
data(wrld_simpl)
memory.limit(size=56000)
seedNum <- 250


#------------------------------------------------------
# Create dataframes for species of interest and activity seasons
#------------------------------------------------------
SpeciesOfInterest_Names <- c("Aedes aegypti",
                             "Aedes albopictus",
                             "Anopheles gambiae",
                             "Anopheles stephensi",
                             "Culex annulirostris",
                             "Culex pipiens",
                             "Culex quinquefasciatus",
                             "Culex tarsalis")

ActivitySeason_Type <- c("None- Year Round",
                         "Photoperiod",
                         "Precipitation",
                         "None- Year Round",
                         "None- Year Round",
                         "Photoperiod",
                         "None- Year Round",
                         "Photoperiod")


#------------------------------------------------------
# Define local project directory for data load-in
#------------------------------------------------------
project_dir <- "E:/SynologyDrive/Tejas_Server/! Research/! Mordecai Lab/! Mosquito SDM Thermal Dependence/"
setwd(project_dir)
  

#------------------------------------------------------
# Define a decimal places function for filtering by latitude/longitude reporting precision uncertainty
#------------------------------------------------------
decimalNums <- function(x) {
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}


