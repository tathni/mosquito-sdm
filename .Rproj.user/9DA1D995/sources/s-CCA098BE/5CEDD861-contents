#######################################################
# Author: Tejas Athni
# Project: Mosquito Thermal Dependence

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
library(spThin)


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
# Create dataframes for species of interest, activity season, and commonly referenced items
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


