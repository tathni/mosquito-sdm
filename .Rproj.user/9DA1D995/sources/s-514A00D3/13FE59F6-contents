#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Run diagnostic checks to inform geographic range and background sampling method
#######################################################

source("E:/Documents/GitHub/mosquito-sdm/0-config.R")
setwd("E:/SynologyDrive/Tejas_Server/! Research/! Mordecai Lab/! Mosquito SDM Thermal Dependence/")


#------------------------------------------------------
## INITIALIZE GEOGRAPHIC RANGES ##
#------------------------------------------------------
#------------------------------------------------------
# Continents
#------------------------------------------------------
SouthAmerica_list <- c("Colombia", "Venezuela", "Suriname", "Guyana", "French Guiana",
                       "Ecuador", "Peru", "Bolivia", "Chile", "Argentina", "Uruguay",
                       "Paraguay", "Brazil", "Falkland Islands (Malvinas)")
SouthAmerica <- wrld_simpl[wrld_simpl$NAME %in% SouthAmerica_list, ]
NorthAmerica <- wrld_simpl[wrld_simpl$REGION==19,] %>% .[!NorthAmerica$NAME %in% SouthAmerica_list, ]
Africa <- wrld_simpl[wrld_simpl$REGION==2,]
Oceania <- wrld_simpl[wrld_simpl$REGION==9,]
Europe <- wrld_simpl[wrld_simpl$REGION==150,]
Asia <- wrld_simpl[wrld_simpl$REGION==142,]


#------------------------------------------------------
# Regions
#------------------------------------------------------
SouthAsia_list <- c("India","Pakistan","Nepal","Bangladesh","Sri Lanka", "Bhutan")
SouthAsia <- wrld_simpl[wrld_simpl$NAME %in% SouthAsia_list, ]
Europe_noRussia <- wrld_simpl[wrld_simpl$REGION==150 & !wrld_simpl$NAME == "Russia", ]


#------------------------------------------------------
# Hemispheres
#------------------------------------------------------
LeftHemisphere <- rbind(SouthAmerica, NorthAmerica)
RightHemisphere <- rbind(Africa, Oceania, Europe, Asia)


#------------------------------------------------------
# Entire globe
#------------------------------------------------------
Globe <- rbind(SouthAmerica, NorthAmerica, Africa, Oceania, Europe, Asia)


#------------------------------------------------------
# Load in environmental predictors
#------------------------------------------------------
predictors_preStack <- alply(list.files("Environmental Predictors Merged",
                                        pattern = ".tif",
                                        full.names = TRUE), 1, function(file){
                                          print(file)
                                          rast <- raster(file)
                                          return(rast)
                                        })
rasterNames <- c("ELEV","EVIM","EVISD","FC","HPD","PDQ","PhotoASTM","PhotoASTSD","PrecipASTM","PrecipASTSD","PWQ","TAM","TASD")
predictors_preStack <- setNames(predictors_preStack, rasterNames)


#------------------------------------------------------
# Compress predictors into a stacked raster for each activity season combination
#------------------------------------------------------
predictors_yearRound <- predictors_preStack[c(1:6,11:13)] %>% stack()
predictors_photoSeason <- predictors_preStack[c(1:8,11)] %>% stack()
predictors_precipSeason <- predictors_preStack[c(1:6,9:11)] %>% stack()
predictor_sum_yearRound <- raster("Predictor_Sum_YearRound.tif")
predictor_sum_photoSeason <- raster("Predictor_Sum_PhotoSeason.tif")
predictor_sum_precipSeason <- raster("Predictor_Sum_PrecipSeason.tif")



#------------------------------------------------------
## CREATE CONTINENT, REGION, HEMISPHERE, GLOBE MAPS BY ACTIVITY SEASON ##
#------------------------------------------------------
#------------------------------------------------------
# Year round
#------------------------------------------------------
year_SouthAmerica <- predictors_yearRound %>% mask(predictor_sum_yearRound) %>%
  crop(., extent(SouthAmerica)) %>% mask(SouthAmerica)
year_NorthAmerica <- predictors_yearRound %>% mask(predictor_sum_yearRound) %>%
  crop(., extent(NorthAmerica)) %>% mask(NorthAmerica)
year_Africa <- predictors_yearRound %>% mask(predictor_sum_yearRound) %>%
  crop(., extent(Africa)) %>% mask(Africa)
year_Oceania <- predictors_yearRound %>% mask(predictor_sum_yearRound) %>%
  crop(., extent(Oceania)) %>% mask(Oceania)
year_Europe <- predictors_yearRound %>% mask(predictor_sum_yearRound) %>%
  crop(., extent(Europe)) %>% mask(Europe)
year_Europe_noRussia <- predictors_yearRound %>% mask(predictor_sum_yearRound) %>%
  crop(., extent(Europe_noRussia)) %>% mask(Europe_noRussia)
year_Asia <- predictors_yearRound %>% mask(predictor_sum_yearRound) %>%
  crop(., extent(Asia)) %>% mask(Asia)
year_SouthAsia <- predictors_yearRound %>% mask(predictor_sum_yearRound) %>%
  crop(., extent(SouthAsia)) %>% mask(SouthAsia)
year_LeftHemisphere <- predictors_yearRound %>% mask(predictor_sum_yearRound) %>%
  crop(., extent(LeftHemisphere)) %>% mask(LeftHemisphere)
year_RightHemisphere <- predictors_yearRound %>% mask(predictor_sum_yearRound) %>%
  crop(., extent(RightHemisphere)) %>% mask(RightHemisphere)
year_Globe <- predictors_yearRound %>% mask(predictor_sum_yearRound) %>%
  crop(., extent(Globe)) %>% mask(Globe)


#------------------------------------------------------
# Photoperiod activity season
#------------------------------------------------------
photo_SouthAmerica <- predictors_photoSeason %>% mask(predictor_sum_photoSeason) %>%
  crop(., extent(SouthAmerica)) %>% mask(SouthAmerica)
photo_NorthAmerica <- predictors_photoSeason %>% mask(predictor_sum_photoSeason) %>%
  crop(., extent(NorthAmerica)) %>% mask(NorthAmerica)
photo_Africa <- predictors_photoSeason %>% mask(predictor_sum_photoSeason) %>%
  crop(., extent(Africa)) %>% mask(Africa)
photo_Oceania <- predictors_photoSeason %>% mask(predictor_sum_photoSeason) %>%
  crop(., extent(Oceania)) %>% mask(Oceania)
photo_Europe <- predictors_photoSeason %>% mask(predictor_sum_photoSeason) %>%
  crop(., extent(Europe)) %>% mask(Europe)
photo_Europe_noRussia <- predictors_photoSeason %>% mask(predictor_sum_photoSeason) %>%
  crop(., extent(Europe_noRussia)) %>% mask(Europe_noRussia)
photo_Asia <- predictors_photoSeason %>% mask(predictor_sum_photoSeason) %>%
  crop(., extent(Asia)) %>% mask(Asia)
photo_SouthAsia <- predictors_photoSeason %>% mask(predictor_sum_photoSeason) %>%
  crop(., extent(SouthAsia)) %>% mask(SouthAsia)
photo_LeftHemisphere <- predictors_photoSeason %>% mask(predictor_sum_photoSeason) %>%
  crop(., extent(LeftHemisphere)) %>% mask(LeftHemisphere)
photo_RightHemisphere <- predictors_photoSeason %>% mask(predictor_sum_photoSeason) %>%
  crop(., extent(RightHemisphere)) %>% mask(RightHemisphere)
photo_Globe <- predictors_photoSeason %>% mask(predictor_sum_photoSeason) %>%
  crop(., extent(Globe)) %>% mask(Globe)


#------------------------------------------------------
# Precipitation activity season
#------------------------------------------------------
precip_Africa <- predictors_precipSeason %>% mask(predictor_sum_precipSeason) %>%
  crop(., extent(Africa)) %>% mask(Africa)
precip_RightHemisphere <- predictors_precipSeason %>% mask(predictor_sum_precipSeason) %>%
  crop(., extent(RightHemisphere)) %>% mask(RightHemisphere)
precip_Globe <- predictors_precipSeason %>% mask(predictor_sum_precipSeason) %>%
  crop(., extent(Globe)) %>% mask(Globe)


#------------------------------------------------------
# Save rasters
#------------------------------------------------------
writeRaster(year_SouthAmerica, filename = "Diagnostic Continent Maps/Year_SouthAmerica.tif", format = "GTiff", overwrite=T)
writeRaster(year_NorthAmerica, filename = "Diagnostic Continent Maps/Year_NorthAmerica.tif", format = "GTiff", overwrite=T)
writeRaster(year_Africa, filename = "Diagnostic Continent Maps/Year_Africa.tif", format = "GTiff", overwrite=T)
writeRaster(year_Oceania, filename = "Diagnostic Continent Maps/Year_Oceania.tif", format = "GTiff", overwrite=T)
writeRaster(year_Europe, filename = "Diagnostic Continent Maps/Year_Europe.tif", format = "GTiff", overwrite=T)
writeRaster(year_Europe_noRussia, filename = "Diagnostic Continent Maps/Year_Europe_noRussia.tif", format = "GTiff", overwrite=T)
writeRaster(year_Asia, filename = "Diagnostic Continent Maps/Year_Asia.tif", format = "GTiff", overwrite=T)
writeRaster(year_SouthAsia, filename = "Diagnostic Continent Maps/Year_SouthAsia.tif", format = "GTiff", overwrite=T)
writeRaster(year_LeftHemisphere, filename = "Diagnostic Continent Maps/Year_LeftHemisphere.tif", format = "GTiff", overwrite=T) 
writeRaster(year_RightHemisphere, filename = "Diagnostic Continent Maps/Year_RightHemisphere.tif", format = "GTiff", overwrite=T) 
writeRaster(year_Globe, filename = "Diagnostic Continent Maps/Year_Globe.tif", format = "GTiff", overwrite=T) 

writeRaster(photo_SouthAmerica, filename = "Diagnostic Continent Maps/Photo_SouthAmerica.tif", format = "GTiff", overwrite=T)
writeRaster(photo_NorthAmerica, filename = "Diagnostic Continent Maps/Photo_NorthAmerica.tif", format = "GTiff", overwrite=T)
writeRaster(photo_Africa, filename = "Diagnostic Continent Maps/Photo_Africa.tif", format = "GTiff", overwrite=T)
writeRaster(photo_Oceania, filename = "Diagnostic Continent Maps/Photo_Oceania.tif", format = "GTiff", overwrite=T)
writeRaster(photo_Europe, filename = "Diagnostic Continent Maps/Photo_Europe.tif", format = "GTiff", overwrite=T)
writeRaster(photo_Europe_noRussia, filename = "Diagnostic Continent Maps/Photo_Europe_noRussia.tif", format = "GTiff", overwrite=T)
writeRaster(photo_Asia, filename = "Diagnostic Continent Maps/Photo_Asia.tif", format = "GTiff", overwrite=T)
writeRaster(photo_SouthAsia, filename = "Diagnostic Continent Maps/Photo_SouthAsia.tif", format = "GTiff", overwrite=T) 
writeRaster(photo_LeftHemisphere, filename = "Diagnostic Continent Maps/Photo_LeftHemisphere.tif", format = "GTiff", overwrite=T) 
writeRaster(photo_RightHemisphere, filename = "Diagnostic Continent Maps/Photo_RightHemisphere.tif", format = "GTiff", overwrite=T) 
writeRaster(photo_Globe, filename = "Diagnostic Continent Maps/Photo_Globe.tif", format = "GTiff", overwrite=T) 

writeRaster(precip_Africa, filename = "Diagnostic Continent Maps/Precip_Africa.tif", format = "GTiff", overwrite=T)
writeRaster(precip_RightHemisphere, filename = "Diagnostic Continent Maps/Precip_RightHemisphere.tif", format = "GTiff", overwrite=T) 
writeRaster(precip_Globe, filename = "Diagnostic Continent Maps/Precip_Globe.tif", format = "GTiff", overwrite=T) 







#------------------------------------------------------
## RUN DIAGNOSTICS ##
#------------------------------------------------------
#------------------------------------------------------
# Diagnostic 1: Thermal breadth of occurrences and background by species by continent
#------------------------------------------------------


# ?? for loop, feed in select species' points, restricted to that continent, extract(), hist()
remove_df <- c(which(is.na(raster::extract(sampling_ranges[[i]], occGPS_activity_noNA))))
if(length(remove_df) > 0) {
  occGPS_samprange_noNA <- occGPS_activity_noNA[-remove_df,]
} else {
  occGPS_samprange_noNA <- occGPS_activity_noNA
}

sampling_range_points <- nrow(occGPS_samprange_noNA)

cell_centroids_unique <- xyFromCell(predictors, cellFromXY(predictors, occGPS_samprange_noNA) %>% 
                                      unique) %>% as.data.frame() ## keep unique and nrow() for diagnostic 2, but remove for diagnostic 1
x <- data.frame(raster::extract(predictors, cell_centroids_unique))

tam_aegypti_NorthAmerica
tam_aegypti_SouthAmerica
tam_aegypti_Africa
tam_aegypti_Asia  ## ??? southeast asia in reality
tam_aegypti_Oceania
tam_albopictus_NorthAmerica
tam_albopictus_SouthAmerica
tam_albopictus_Europe_noRussia
tam_albopictus_Africa
tam_albopictus_Asia
tam_gambiae_Africa
tam_stephensi_SouthAsia
tam_annulirostris_Oceania
tam_pipiens_NorthAmerica
tam_pipiens_Europe_noRussia
tam_quinque_NorthAmerica
tam_quinque_Oceania
tam_quinque_Asia
tam_tarsalis_NorthAmerica


## ?? based on column of sampleRast, extract() and then hist() each one
tam_aegypti_NorthAmerica_bg <- enmSdm::sampleRast(year_NorthAmerica, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
tam_aegypti_SouthAmerica_bg <- enmSdm::sampleRast(year_SouthAmerica, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
tam_aegypti_Africa_bg <- enmSdm::sampleRast(year_Africa, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
tam_aegypti_Asia_bg <- enmSdm::sampleRast(year_Asia, n = 10000, replace = F, prob = F) %>%
  as.data.frame()  
tam_aegypti_Oceania_bg <- enmSdm::sampleRast(year_Oceania, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
tam_albopictus_NorthAmerica_bg <- enmSdm::sampleRast(photo_NorthAmerica, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
tam_albopictus_SouthAmerica_bg <- enmSdm::sampleRast(photo_SouthAmerica, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
tam_albopictus_Europe_noRussia_bg <- enmSdm::sampleRast(photo_Europe_noRussia, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
tam_albopictus_Africa_bg <- enmSdm::sampleRast(photo_Africa, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
tam_albopictus_Asia_bg <- enmSdm::sampleRast(photo_Asia, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
tam_gambiae_Africa_bg <- enmSdm::sampleRast(precip_Africa, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
tam_stephensi_SouthAsia_bg <- enmSdm::sampleRast(year_SouthAsia, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
tam_annulirostris_Oceania_bg <- enmSdm::sampleRast(year_Oceania, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
tam_pipiens_NorthAmerica_bg <- enmSdm::sampleRast(photo_NorthAmerica, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
tam_pipiens_Europe_noRussia_bg <- enmSdm::sampleRast(photo_Europe_noRussia, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
tam_quinque_NorthAmerica_bg <- enmSdm::sampleRast(year_NorthAmerica, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
tam_quinque_Oceania_bg <- enmSdm::sampleRast(year_Oceania, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
tam_quinque_Asia_bg <- enmSdm::sampleRast(year_Asia, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
tam_tarsalis_NorthAmerica_bg <- enmSdm::sampleRast(photo_NorthAmerica, n = 10000, replace = F, prob = F) %>%
  as.data.frame()





#------------------------------------------------------
# Diagnostic 2: Unique mosquito sampling cells by species by continent
#------------------------------------------------------



#------------------------------------------------------
# Diagnostic 3: Covariate distribution of occurrences and random points by species by continent and globally
#------------------------------------------------------


## ?? run the covariate extraction and density based on each dataframe
cov_aegypti_NorthAmerica_bg <- enmSdm::sampleRast(NorthAmerica_DiagnosticMap, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
cov_aegypti_SouthAmerica_bg <- enmSdm::sampleRast(SouthAmerica_DiagnosticMap, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
cov_aegypti_Africa_bg <- enmSdm::sampleRast(Africa_DiagnosticMap, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
cov_aegypti_Asia_bg <- enmSdm::sampleRast(Asia_DiagnosticMap, n = 10000, replace = F, prob = F) %>% ## ?? aegypti is southeast asia in reality
  as.data.frame()  
cov_aegypti_Oceania_bg <- enmSdm::sampleRast(Oceania_DiagnosticMap, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
cov_albopictus_NorthAmerica_bg <- enmSdm::sampleRast(NorthAmerica_DiagnosticMap, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
cov_albopictus_SouthAmerica_bg <- enmSdm::sampleRast(SouthAmerica_DiagnosticMap, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
cov_albopictus_Europe_noRussia_bg <- enmSdm::sampleRast(Europe_noRussia_DiagnosticMap, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
cov_albopictus_Africa_bg <- enmSdm::sampleRast(Africa_DiagnosticMap, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
cov_albopictus_Asia_bg <- enmSdm::sampleRast(Asia_DiagnosticMap, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
cov_gambiae_Africa_bg <- enmSdm::sampleRast(Africa_DiagnosticMap, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
cov_stephensi_SouthAsia_bg <- enmSdm::sampleRast(SouthAsia_DiagnosticMap, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
cov_annulirostris_Oceania_bg <- enmSdm::sampleRast(Oceania_DiagnosticMap, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
cov_pipiens_NorthAmerica_bg <- enmSdm::sampleRast(NorthAmerica_DiagnosticMap, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
cov_pipiens_Europe_noRussia_bg <- enmSdm::sampleRast(Europe_noRussia_DiagnosticMap, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
cov_quinque_NorthAmerica_bg <- enmSdm::sampleRast(NorthAmerica_DiagnosticMap, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
cov_quinque_Oceania_bg <- enmSdm::sampleRast(Oceania_DiagnosticMap, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
cov_quinque_Asia_bg <- enmSdm::sampleRast(Asia_DiagnosticMap, n = 10000, replace = F, prob = F) %>%
  as.data.frame()
cov_tarsalis_NorthAmerica_bg <- enmSdm::sampleRast(NorthAmerica_DiagnosticMap, n = 10000, replace = F, prob = F) %>%
  as.data.frame()




