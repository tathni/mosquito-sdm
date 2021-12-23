#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Produce summary figures and plots from the extracted raster data
#######################################################

source("E:/Documents/GitHub/mosquito-sdm/0-config.R")


#------------------------------------------------------
# Load in the extracted dataset of covariates
#------------------------------------------------------
data <- read.csv("SDM Data.csv")



#------------------------------------------------------
# Meta-statistics on min, max, and median for covariates
#------------------------------------------------------
metastats <- data %>% filter(Occ1_or_Bg0 == 1) %>%
  .[,c(1,6:14)] %>%
  group_by(Species) %>%
  summarise_all(list(Min = min,
                     Perc2.5 = ~ quantile(x = ., probs = 0.025),
                     Mean = mean,
                     Perc97.5 = ~ quantile(x = ., probs = 0.975),
                     Max = max)) %>%
  .[,c("Species",
       "ELEV_Min","ELEV_Perc2.5","ELEV_Mean","ELEV_Perc97.5","ELEV_Max",
       "EVIM_Min","EVIM_Perc2.5","EVIM_Mean","EVIM_Perc97.5","EVIM_Max",
       "EVISD_Min","EVISD_Perc2.5","EVISD_Mean","EVISD_Perc97.5","EVISD_Max",
       "FC_Min","FC_Perc2.5","FC_Mean","FC_Perc97.5","FC_Max",
       "HPD_Min","HPD_Perc2.5","HPD_Mean","HPD_Perc97.5","HPD_Max",
       "PDQ_Min","PDQ_Perc2.5","PDQ_Mean","PDQ_Perc97.5","PDQ_Max",
       "PWQ_Min","PWQ_Perc2.5","PWQ_Mean","PWQ_Perc97.5","PWQ_Max",
       "TAM_Min","TAM_Perc2.5","TAM_Mean","TAM_Perc97.5","TAM_Max",
       "TASD_Min","TASD_Perc2.5","TASD_Mean","TASD_Perc97.5","TASD_Max")]
metastats[,c(2:46)] %<>% round(3)
write.csv(metastats, "Raster Metastatistics.csv")



#------------------------------------------------------
# Species occurrence and background maps
#------------------------------------------------------
data(wrld_simpl)
worldMap <- wrld_simpl[wrld_simpl@data$UN!="10",]

for(i in 1:length(SpeciesOfInterest_Names)) {
  points_occ <- data %>% filter(Species == SpeciesOfInterest_Names[i],
                                DataSplit == "Training",
                                Occ1_or_Bg0 == 1) %>%
    dplyr::select(Longitude, Latitude)
  
  points_bg <- data %>% filter(Species == SpeciesOfInterest_Names[i],
                               DataSplit == "Training",
                               Occ1_or_Bg0 == 0) %>%
    dplyr::select(Longitude, Latitude)
  
  saveName <- paste0(SpeciesOfInterest_Names[i]," Occurrences Map.pdf")
  pdf(saveName)
  plot(worldMap, ylim=c(-80, 65), xlim = c(-180, 180), mar = c(0,0,0,0),
       bg = "lightblue3", border = NA, col = "antiquewhite",
       main = SpeciesOfInterest_Names[i])
  points(points_occ, cex=0.04, col="firebrick1")
  legend(90, -80, c("Occurrence"),
         cex = 1, col=c("firebrick1"),
         pch = c(19,19), 
         bg = "white")
  dev.off()
  
  saveName <- paste0(SpeciesOfInterest_Names[i]," Background Map.pdf")
  pdf(saveName)
  plot(worldMap, ylim=c(-80, 65), xlim = c(-180, 180), mar = c(0,0,0,0),
       bg = "lightblue3", border = NA, col = "antiquewhite",
       main = SpeciesOfInterest_Names[i])
  points(points_bg, cex=0.04, col="darkgrey")
  legend(90, -80, c("Background"),
         cex = 1, col=c("darkgrey"),
         pch = c(19,19), 
         bg = "white")
  dev.off()
}



#------------------------------------------------------
# Plot all Culicidae background points
#------------------------------------------------------
Background_All <- read.csv("GBIF Datasets Cleaned/Background_Culicidae_Cleaned.csv", header = TRUE,
                                     encoding = "UTF-8", stringsAsFactors = FALSE) %>%
  dplyr::select(decimalLongitude, decimalLatitude)
pdf("All Background Mosquitoes.pdf")
plot(worldMap, ylim=c(-80, 65), xlim = c(-180, 180), mar = c(0,0,0,0),
     bg = "lightblue3", border = NA, col = "antiquewhite",
     main = "All Mosquitoes")
points(Background_All, cex=0.02, col="grey")
dev.off()



#------------------------------------------------------
# Density plots for covariates
#------------------------------------------------------
# For each variable, plot the density curves for occurrences and background points
for(i in 1:length(SpeciesOfInterest_Names)) {
  print(paste0("Creating density plots for ", SpeciesOfInterest_Names[i]))
  data_species <- data %>%
    mutate(HP_log = log(HPD)) %>% 
    dplyr::select(-HPD) %>%
    filter(Species == SpeciesOfInterest_Names[i]) %>%
    .[,c(1,5:14)]
  data_species <- cbind(data_species[1:2],
                        lapply(data_species[3:11], as.numeric) %>% as.data.frame())
  
  densitySave <- paste0(SpeciesOfInterest_Names[i]," Density.pdf")
  pdf(densitySave)
  
  par(mfrow = c(3,3), # Set number of rows and columns
      mar = c(3, 3.5, 1, 1) + 1, # Margins of individual panels
      oma = c(3, 3, 3, 3)) # Outer margins
  a_ply(colnames(data_species)[-c(1:2)], 1, function(p){
    occ_density <- data_species %>%
      filter(Occ1_or_Bg0 == 1) %>%
      pull(p) %>% 
      density
    bg_density <- data_species %>%
      filter(Occ1_or_Bg0 == 0) %>%
      pull(p) %>% 
      density
    
    # Set up plot
    plot(NULL, 
         las = 1,
         xlim = range(c(occ_density$x, bg_density$x)),
         ylim = range(c(occ_density$y, bg_density$y)),
         ylab = "", 
         xlab = "")
    
    # Add curves for occurrences and background
    points(occ_density$x, occ_density$y, type = "l", col = "#0d0df2")
    points(bg_density$x, occ_density$y, type = "l", col = "#1a1a1a99")
    mtext(p, side = 1, line = 2.5, cex = 0.75) # X axis label
  })
  mtext(SpeciesOfInterest_Names[i], outer = TRUE) # Add overall title
  dev.off()
}



#------------------------------------------------------
# Activity season rasters
#------------------------------------------------------
activitySeason <- alply(list.files("Activity Season Metadata Merged",
                                       pattern = ".tif",
                                       full.names = TRUE), 1, function(file){
                                         print(file)
                                         rast <- raster(file)
                                         return(rast)
                                       })

rasterNames <- c("PhotoAS_LastDay","PhotoAS_Length","PhotoAS_FirstDay","PrecipAS_LastDay","PrecipAS_Length","PrecipAS_FirstDay")
rasterNames_spaced <- c("Photoperiod Activity Season - Last Day",
                        "Photoperiod Activity Season - Length",
                        "Photoperiod Activity Season - First Day",
                        "Precipitation Activity Season - Last Day",
                        "Precipitation Activity Season - Length",
                        "Precipitation Activity Season - First Day")
activitySeason <- setNames(activitySeason, rasterNames)

for(i in 1:length(activitySeason)) {
  saveName <- paste0(rasterNames[i],".pdf")
  pdf(saveName)
  plot(activitySeason[[i]], main = rasterNames_spaced[i])
  dev.off()
}



#------------------------------------------------------
# Environmental covariates rasters
#------------------------------------------------------
predictors <- alply(list.files("Environmental Predictors Merged",
                                   pattern = ".tif",
                                   full.names = TRUE), 1, function(file){
                                     print(file)
                                     rast <- raster(file)
                                     return(rast)
                                   })

rasterNames <- c("ELEV","EVIM","EVISD","FC","HP","PDQ","PhotoASTM","PhotoASTSD","PrecipASTM","PrecipASTSD","PWQ","TAM","TASD")
rasterNames_spaced <- c("Elevation",
                        "Enhanced Vegetation Index - Mean",
                        "Enhanced Vegetation Index - Standard Deviation",
                        "Forest Cover %",
                        "Human Population",
                        "Precipitation of the Driest Quarter",
                        "Photoperiod Activity Season - Temperature Mean",
                        "Photoperiod Activity Season - Temperature Standard Deviation",
                        "Precipitation Activity Season - Temperature Mean",
                        "Precipitation Activity Season - Temperature Standard Deviation",
                        "Precipitation of the Wettest Quarter",
                        "Temperature Annual Mean",
                        "Temperature Annual Standard Deviation")
predictors <- setNames(predictors, rasterNames)

for(i in 1:length(predictors)) {
  saveName <- paste0(rasterNames[i],".pdf")
  pdf(saveName)
  plot(predictors[[i]], main = rasterNames_spaced[i])
  dev.off()
}

pdf("HP_Log.pdf")
plot(log(predictors[[5]]), main = "Log(Human Population)")
dev.off()



#------------------------------------------------------
# Sampling range map rasters
#------------------------------------------------------
samplingMaps <- alply(list.files("Sampling Range Maps",
                               pattern = ".tif",
                               full.names = TRUE), 1, function(file){
                                 print(file)
                                 rast <- raster(file)
                                 return(rast)
                               })

rasterNames <- c("AeAegpyti_SamplingMap","AeAlbopictus_SamplingMap","AnGambiae_SamplingMap","AnStephensi_SamplingMap",
                 "CxAnnulirostris_SamplingMap","CxPipiens_SamplingMap","CxQuinquefasciatus_SamplingMap","CxTarsalis_SamplingMap")
rasterNames_spaced <- c("Aedes aegypti - Background Sampling Range",
                        "Aedes albopictus - Background Sampling Range",
                        "Anopheles gambiae - Background Sampling Range",
                        "Anopheles stephensi - Background Sampling Range",
                        "Culex annulirostris - Background Sampling Range",
                        "Culex pipiens - Background Sampling Range",
                        "Culex quinquefasciatus - Background Sampling Range",
                        "Culex tarsalis - Background Sampling Range")
samplingMaps <- setNames(samplingMaps, rasterNames)

for(i in 1:length(samplingMaps)) {
  saveName <- paste0(rasterNames[i],".pdf")
  pdf(saveName)
  plot(samplingMaps[[i]], main = rasterNames_spaced[i])
  dev.off()
}





#------------------------------------------------------
## ????? work on salvaging the below figures and put them up in the mainstream code flow once i have done so
#------------------------------------------------------
#------------------------------------------------------
# Pairs correlation plot
#------------------------------------------------------

set.seed(seedNum)
pairs_possibleBg <- Mosquitoes_AllBackground %>%
  dplyr::select(decimalLongitude, decimalLatitude) %>%
  unique
set.seed(seedNum)
pairs_possibleBg <- pairs_possibleBg[sample(nrow(pairs_possibleBg), 15000), ] # Over-sample to account for NA's
pairs_bg <- raster::extract(predictors, pairs_possibleBg)
pairs_bg <- pairs_bg[complete.cases(pairs_bg), ]
g = 10000
set.seed(seedNum)
pairs_sample <- pairs_bg[sample(nrow(pairs_bg), g), ] # Select 10k bg points for pairs sampling

pdf("Pairs Correlation Covariates.pdf", width=9, height=8)
corrplot(cor(pairs_sample),
         method = "color",
         addCoef.col = "black",
         tl.col = "black", tl.srt = 45,
         type = "upper")
dev.off()


#------------------------------------------------------
# Pairs correlation plot for all temperature variables, including unused
#------------------------------------------------------
set.seed(seedNum)
temp_possibleBg <- Mosquitoes_AllBackground %>%
  select(decimalLongitude, decimalLatitude) %>%
  unique
set.seed(seedNum)
temp_possibleBg <- temp_possibleBg[sample(nrow(temp_possibleBg), 15000), ] # Over-sample to account for NA's
temp_bg <- raster::extract(tempPredictors, temp_possibleBg)
temp_bg <- temp_bg[complete.cases(temp_bg), ]
g = 10000
set.seed(seedNum)
temp_sample <- temp_bg[sample(nrow(temp_bg), g), ] # Select 10k bg points for pairs sampling

pdf("All Temperature Covariates Pairs.pdf")
corrplot(cor(temp_sample),
         method = "color",
         addCoef.col = "black",
         tl.col = "black", tl.srt = 45,
         type = "upper")
dev.off()


#------------------------------------------------------
# All species occurrences on a single map
#------------------------------------------------------
# Partition data by individual species of interest for species occurrence plotting
AedesAegypti <- filter(Mosquitoes_SpeciesOfInterest, species == "Aedes aegypti")
AedesAegypti_Points <- AedesAegypti %>% select(decimalLongitude, decimalLatitude) %>% unique
AedesAlbopictus <- filter(Mosquitoes_SpeciesOfInterest, species == "Aedes albopictus")
AedesAlbopictus_Points <- AedesAlbopictus %>% select(decimalLongitude, decimalLatitude) %>% unique
AnophelesGambiae <- filter(Mosquitoes_SpeciesOfInterest, species == "Anopheles gambiae")
AnophelesGambiae_Points <- AnophelesGambiae %>% select(decimalLongitude, decimalLatitude) %>% unique
# AnophelesStephensi <- filter(Mosquitoes_SpeciesOfInterest, species == "Anopheles stephensi")
# AnophelesStephensi_Points <- AnophelesStephensi %>% select(decimalLongitude, decimalLatitude) %>% unique
CulexAnnulirostris <- filter(Mosquitoes_SpeciesOfInterest, species == "Culex annulirostris")
CulexAnnulirostris_Points <- CulexAnnulirostris %>% select(decimalLongitude, decimalLatitude) %>% unique
CulexPipiens <- filter(Mosquitoes_SpeciesOfInterest, species == "Culex pipiens")
CulexPipiens_Points <- CulexPipiens %>% select(decimalLongitude, decimalLatitude) %>% unique
CulexQuinquefasciatus <- filter(Mosquitoes_SpeciesOfInterest, species == "Culex quinquefasciatus")
CulexQuinquefasciatus_Points <- CulexQuinquefasciatus %>% select(decimalLongitude, decimalLatitude) %>% unique
CulexTarsalis <- filter(Mosquitoes_SpeciesOfInterest, species == "Culex tarsalis")
CulexTarsalis_Points <- CulexTarsalis %>% select(decimalLongitude, decimalLatitude) %>% unique

# Plotting species occurrences
pdf("Species Occurrence Map.pdf")
worldMap <- wrld_simpl[wrld_simpl@data$UN!="10",]
plot(worldMap, ylim=c(-80, 65), xlim = c(-180, 180), mar = c(0,0,0,0),
     bg = "lightblue3", border = NA, col = "antiquewhite")

points(AedesAegypti_Points, cex=0.05, col="grey30")
points(AedesAlbopictus_Points, cex=0.05, col="firebrick1")
points(AnophelesGambiae_Points, cex=0.05, col="#00ff00")
# points(AnophelesStephensi_Points, cex=0.05, col="brown")
points(CulexAnnulirostris_Points, cex=0.05, col="darkmagenta")
points(CulexPipiens_Points, cex=0.05, col="darkgreen")
points(CulexQuinquefasciatus_Points, cex=0.05, col="blue")
points(CulexTarsalis_Points, cex=0.05, col="darkorange")

legend(100, -80, c("Aedes aegypti","Aedes albopictus","Anopheles gambiae",
                   "Culex annulirostris","Culex pipiens","Culex quinquefasciatus","Culex tarsalis"),
       cex = 0.6, col=c("grey30","firebrick1","#00ff00","brown","darkmagenta","darkgreen","blue","darkorange"),
       pch = c(19,19,19,19,19,19,19),
       bg = "white")
dev.off()



