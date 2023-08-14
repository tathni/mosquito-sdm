#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Produce plots and figures from the extracted rasters in the analysis dataset
#######################################################

source("C:/Users/tejas/Documents/GitHub/mosquito-sdm/0-config.R")


#------------------------------------------------------
## DATA LOAD-IN ##
#------------------------------------------------------
#------------------------------------------------------
# Load in analysis dataset
#------------------------------------------------------
sdm_data <- read.csv("Analysis_Dataset/SDM_Data.csv")

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
predictors_preStack %<>% setNames(rasterNames)

predictors_yearRound <- predictors_preStack[c(1:6,11:12,15,13:14)] %>% stack()
predictors_photoSeason <- predictors_preStack[c(1:6,11:12,15,7:8)] %>% stack()
predictors_precipSeason <- predictors_preStack[c(1:6,11:12,15,9:10)] %>% stack()

#------------------------------------------------------
# Load in ecoregion shapefiles
#------------------------------------------------------
ecoregions <- alply(list.files("Ecoregion_Outputs/Shapefiles",
                               pattern = ".RDS",
                               full.names = TRUE), 1, function(file){
                                 print(file)
                                 shapefile <- readRDS(file)
                                 return(shapefile)
                               }) %>%
  setNames(c("Ae_Aegypti","Ae_Albopictus","An_Gambiae","An_Stephensi",
             "Cx_Pipiens","Cx_Quinque","Cx_Tarsalis"))



#------------------------------------------------------
## 1. SPECIES OCCURRENCE AND BACKGROUND MAPS ##
#------------------------------------------------------
species_maps <- list()

data(world)
world <- world %>% dplyr::filter(!name_long == "Antarctica")
africa <- world[world$continent == "Africa",]
oceania <- world[world$continent == "Oceania",]
europe <- world[world$continent == "Europe",]
south_america <- world[world$continent == "South America",]
north_america <- world[world$continent == "North America",]
asia <- world[world$continent == "Asia",]

species_continents <- list(world, world, rbind(africa,asia), rbind(africa,asia), rbind(north_america, africa, europe, asia, oceania),
                        world, north_america)

for(i in 1:length(SpeciesOfInterest_Names)) {
  #------------------------------------------------------
  # Acquire occ and bg for species
  #------------------------------------------------------
  occ_centroids <- sdm_data %>%
    filter(Species == SpeciesOfInterest_Names[[i]],
           Occ1_or_Bg0 == 1) %>%
    dplyr::select(Centroid_Longitude, Centroid_Latitude)
  
  occ <- st_as_sf(occ_centroids, coords = c("Centroid_Longitude","Centroid_Latitude"),
                  crs = 4326, agr = "constant")
  
  bg_centroids <- sdm_data %>%
    filter(Species == SpeciesOfInterest_Names[[i]],
           Occ1_or_Bg0 == 0) %>%
    dplyr::select(Centroid_Longitude, Centroid_Latitude)
  
  bg <- st_as_sf(bg_centroids, coords = c("Centroid_Longitude","Centroid_Latitude"),
                 crs = 4326, agr = "constant")
  
  #------------------------------------------------------
  # Plot occ and bg on ecoregion maps
  #------------------------------------------------------
  species_plot <- ggplot() + 
    geom_sf(data = species_continents[[i]], color = NA, fill="lightgrey", alpha=0.3) +
    geom_sf(data = ecoregions[[i]], color = "black", fill = "tan", alpha = 0.4) +
    geom_sf(data = bg, aes(color = "Background"), size = 1.75, alpha = 0.3, show.legend = "point") + 
    geom_sf(data = occ, aes(color = "Occurrence"), size = 1.75, alpha = 0.3, show.legend = "point") + 
    scale_color_manual(name = "Centroid",
                       values = c("Occurrence" = "#b80700", "Background" = "#003f91")) +
    theme_bw() +
    theme(legend.position="bottom") +
    ggtitle(SpeciesOfInterest_Names[[i]])
  
  species_maps[[i]] <- species_plot
}

#------------------------------------------------------
# Save maps
#------------------------------------------------------
for(i in 1:length(species_maps)) {
  ggsave(species_maps[[i]], filename = paste(SpeciesOfInterest_Underscore[[i]],"_Occ_Bg_on_Ecoregion.pdf"), width=10,height=15)
}



#------------------------------------------------------
## 2. ENVIRONMENTAL COVARIATES PLOTS ##
#------------------------------------------------------
#------------------------------------------------------
# Create spaced names with units for plot titles
#------------------------------------------------------
rasterNames_spaced <- c("Cattle Density (animals per 1 sq. km)",
                        "Enhanced Vegetation Index - Mean",
                        "Enhanced Vegetation Index - Standard Deviation",
                        "Forest Cover (%)",
                        "Human Population Density (persons per 1 sq. km)",
                        "Precipitation of the Driest Quarter (mm)",
                        "Photoperiod Activity Season - Temperature Mean (?C)",
                        "Photoperiod Activity Season - Temperature Standard Deviation (?C)",
                        "Precipitation Activity Season - Temperature Mean (?C)",
                        "Precipitation Activity Season - Temperature Standard Deviation (?C)",
                        "Precipitation of the Wettest Quarter (mm)",
                        "Surface Water Seasonality (# of months)",
                        "Temperature Annual Mean (?C)",
                        "Temperature Annual Standard Deviation (?C)",
                        "Wind Speed (m/s)")

#------------------------------------------------------
# Plot rasters of environmental covariates
#------------------------------------------------------
for(i in 1:length(predictors_preStack)) {
  saveName <- paste0("Raster_Summary_Figures/Environmental_Covariates_Plots/",rasterNames[[i]],".pdf")
  pdf(saveName)
  plot(predictors[[i]], main = rasterNames_spaced[i])
  dev.off()
}

pdf("HPD_Log.pdf")
plot(log(predictors_preStack[[5]]), main = "Log of Human Population Density")
dev.off()



#------------------------------------------------------
## 4. ACTIVITY SEASON PLOTS ##
#------------------------------------------------------
activitySeason <- alply(list.files("Activity Season Rasters Merged",
                                   pattern = ".tif",
                                   full.names = TRUE), 1, function(file){
                                     print(file)
                                     rast <- raster(file)
                                     return(rast)
                                   })

rasterNames <- c("PhotoAS_FirstDay","PhotoAS_LastDay","PhotoAS_Length","PrecipAS_FirstDay","PrecipAS_LastDay","PrecipAS_Length")
rasterNames_spaced <- c("Photoperiod Activity Season - First Day",
                        "Photoperiod Activity Season - Last Day",
                        "Photoperiod Activity Season - Length",
                        "Precipitation Activity Season - First Day",
                        "Precipitation Activity Season - Last Day",
                        "Precipitation Activity Season - Length")
activitySeason %<>% setNames(rasterNames)

for(i in 1:length(activitySeason)) {
  saveName <- paste0("Raster_Summary_Figures/Activity_Season_Plots/",rasterNames[[i]],".pdf")
  pdf(saveName)
  plot(activitySeason[[i]], main = rasterNames_spaced[i])
  dev.off()
}



#------------------------------------------------------
## 5. DENSITY PLOTS ##
#------------------------------------------------------
for(i in 1:length(SpeciesOfInterest_Names)) {
  #------------------------------------------------------
  # Rename temperature predictors according to activity season
  #------------------------------------------------------
  if(SpeciesOfInterest_Names[[i]] == "Aedes aegypti" |
     SpeciesOfInterest_Names[[i]] == "Anopheles stephensi" |
     SpeciesOfInterest_Names[[i]] == "Culex quinquefasciatus") {
    colnames(sdm_data)[16] <- "TAM"
    colnames(sdm_data)[17] <- "TASD"
  }
  
  if(SpeciesOfInterest_Names[[i]] == "Aedes albopictus" |
     SpeciesOfInterest_Names[[i]] == "Culex pipiens" |
     SpeciesOfInterest_Names[[i]] == "Culex tarsalis") {
    colnames(sdm_data)[16] <- "PhotoASTM"
    colnames(sdm_data)[17] <- "PhotoASTSD"
  }
  
  if(SpeciesOfInterest_Names[[i]] == "Anopheles gambiae") {
    colnames(sdm_data)[16] <- "PrecipASTM"
    colnames(sdm_data)[17] <- "PrecipASTSD"
  }
  
  #------------------------------------------------------
  # Create df to house transformed variables
  #------------------------------------------------------
  density_df <- sdm_data %>%
    filter(Species == SpeciesOfInterest_Names[[i]]) %>%
    mutate(HPD_log = log(HPD)) %>% 
    dplyr::select(c(3,7:10,11:18))
  
  #------------------------------------------------------
  # For each variable, plot the density curves for occurrences and background points' covariates
  #------------------------------------------------------
  save_name <- paste0("Raster_Summary_Figures/Density_Plots/",SpeciesOfInterest_Underscore[[i]],"_Density.pdf")
  pdf(save_name)
  par(mfrow = c(4,3), # Set number of rows and columns
      mar = c(3, 3.5, 1, 1) + 1, # Margins of individual panels
      oma = c(3, 3, 3, 3)) # Outer margins
  a_ply(colnames(density_df)[-1], 1, function(p){
    occ_density <- density_df %>% 
      filter(Occ1_or_Bg0 == 1) %>% 
      pull(p) %>% 
      density
    bg_density <- density_df %>% 
      filter(Occ1_or_Bg0 == 0) %>% 
      pull(p) %>% 
      density
    
    plot(NULL, # Set up plot
         las = 1,
         xlim = range(c(occ_density$x, bg_density$x)),
         ylim = range(c(occ_density$y, bg_density$y)),
         ylab = "", 
         xlab = "")
    
    points(occ_density$x, occ_density$y, type = "l", col = "#0d0df2")
    points(bg_density$x, occ_density$y, type = "l", col = "#1a1a1a99")
    mtext(p, side = 1, line = 2.5, cex = 0.75) # X axis label
  })
  mtext(SpeciesOfInterest_Names[[i]], outer = TRUE) # Add overall title
  dev.off()
}





# ??? above is done #
 

#------------------------------------------------------
## 3. ENVIRONMENTAL COVARIATES METADATA ##
#------------------------------------------------------
#------------------------------------------------------
# Acquire metadata on min, max, and median
#------------------------------------------------------
# ??
raster_metadata <- data %>% filter(Occ1_or_Bg0 == 1) %>%
  .[,c(1,6:14)] %>% # ?? change col numbers
  group_by(Species) %>%
  summarise_all(list(Min = min,
                     Perc2.5 = ~ quantile(x = ., probs = 0.025),
                     Mean = mean,
                     Perc97.5 = ~ quantile(x = ., probs = 0.975),
                     Max = max)) %>%
  .[,c("Species", ## ?? change the variable names to covariates
       "ELEV_Min","ELEV_Perc2.5","ELEV_Mean","ELEV_Perc97.5","ELEV_Max",
       "EVIM_Min","EVIM_Perc2.5","EVIM_Mean","EVIM_Perc97.5","EVIM_Max",
       "EVISD_Min","EVISD_Perc2.5","EVISD_Mean","EVISD_Perc97.5","EVISD_Max",
       "FC_Min","FC_Perc2.5","FC_Mean","FC_Perc97.5","FC_Max",
       "HPD_Min","HPD_Perc2.5","HPD_Mean","HPD_Perc97.5","HPD_Max",
       "PDQ_Min","PDQ_Perc2.5","PDQ_Mean","PDQ_Perc97.5","PDQ_Max",
       "PWQ_Min","PWQ_Perc2.5","PWQ_Mean","PWQ_Perc97.5","PWQ_Max",
       "TAM_Min","TAM_Perc2.5","TAM_Mean","TAM_Perc97.5","TAM_Max",
       "TASD_Min","TASD_Perc2.5","TASD_Mean","TASD_Perc97.5","TASD_Max")]
raster_metadata[,c(2:46)] %<>% round(3)
write.csv(raster_metadata, "Raster Covariates Metadata.csv")







# ??? above in progress ??? #






#------------------------------------------------------
# Species occurrence and background maps
#------------------------------------------------------
data(wrld_simpl)
worldMap <- wrld_simpl[wrld_simpl@data$UN!="10",]

for(i in 1:length(SpeciesOfInterest_Names)) {
  points_occ <- data %>% filter(Species == SpeciesOfInterest_Names[[i]],
                                DataSplit == "Training",
                                Occ1_or_Bg0 == 1) %>%
    dplyr::select(Longitude, Latitude)
  
  points_bg <- data %>% filter(Species == SpeciesOfInterest_Names[[i]],
                               DataSplit == "Training",
                               Occ1_or_Bg0 == 0) %>%
    dplyr::select(Longitude, Latitude)
  
  saveName <- paste0(SpeciesOfInterest_Names[[i]]," Occurrences Map.pdf")
  pdf(saveName)
  plot(worldMap, ylim=c(-80, 65), xlim = c(-180, 180), mar = c(0,0,0,0),
       bg = "lightblue3", border = NA, col = "antiquewhite",
       main = SpeciesOfInterest_Names[[i]])
  points(points_occ, cex=0.04, col="firebrick1")
  legend(90, -80, c("Occurrence"),
         cex = 1, col=c("firebrick1"),
         pch = c(19,19), 
         bg = "white")
  dev.off()
  
  saveName <- paste0(SpeciesOfInterest_Names[[i]]," Background Map.pdf")
  pdf(saveName)
  plot(worldMap, ylim=c(-80, 65), xlim = c(-180, 180), mar = c(0,0,0,0),
       bg = "lightblue3", border = NA, col = "antiquewhite",
       main = SpeciesOfInterest_Names[[i]])
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
  print(paste0("Creating density plots for ", SpeciesOfInterest_Names[[i]]))
  data_species <- data %>%
    mutate(HP_log = log(HPD)) %>% 
    dplyr::select(-HPD) %>%
    filter(Species == SpeciesOfInterest_Names[[i]]) %>%
    .[,c(1,5:14)]
  data_species <- cbind(data_species[1:2],
                        lapply(data_species[3:11], as.numeric) %>% as.data.frame())
  
  densitySave <- paste0(SpeciesOfInterest_Names[[i]]," Density.pdf")
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
  mtext(SpeciesOfInterest_Names[[i]], outer = TRUE) # Add overall title
  dev.off()
}










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
                 "CxPipiens_SamplingMap","CxQuinquefasciatus_SamplingMap","CxTarsalis_SamplingMap")
rasterNames_spaced <- c("Aedes aegypti - Background Sampling Range",
                        "Aedes albopictus - Background Sampling Range",
                        "Anopheles gambiae - Background Sampling Range",
                        "Anopheles stephensi - Background Sampling Range",
                        "Culex pipiens - Background Sampling Range",
                        "Culex quinquefasciatus - Background Sampling Range",
                        "Culex tarsalis - Background Sampling Range")
samplingMaps <- setNames(samplingMaps, rasterNames)

for(i in 1:length(samplingMaps)) {
  saveName <- paste0(rasterNames[[i]],".pdf")
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
predictors %<>% stack()
set.seed(seedNum)

Mosquitoes_SpeciesOfInterest <- read.csv("GBIF_Datasets_Cleaned/Mosquitoes_SpeciesOfInterest.csv", header = TRUE,
                                         encoding = "UTF-8", stringsAsFactors = FALSE)

occ_points <- Mosquitoes_SpeciesOfInterest %>%
  dplyr::select(c(decimalLongitude, decimalLatitude))

rast <- predictors_preStack[[1]] # Choose any generic raster to acquire cells and centroids from

occ_longlat <- cellFromXY(rast, occ_points) %>% as.data.frame() %>%
  setNames("cell") %>% unique() %>%
  mutate(longitude = xFromCell(rast, cell),  # Acquire longitude (x) and latitude (y) from cell centroids
         latitude = yFromCell(rast, cell)) %>%
  dplyr::select(-cell) %>% # Cell number is now obsolete if working from (x,y) as an sf object
  filter(!is.na(longitude) & !is.na(latitude)) # Remove the NA locations

occ_sf <- st_as_sf(occ_longlat, coords = c("longitude","latitude"),
                   crs = 4326, agr = "constant")

occ_15k <- occ_sf[sample(nrow(occ_sf),
                         size = 15000,
                         replace = FALSE),] # Oversample to acquire 10k occ points for pairs sampling

raster_15k <- raster::extract(predictors, occ_oversample)

raster_10k <- raster_15k %>% na.omit() %>%
  .[sample(nrow(.),
           size = 10000,
           replace = FALSE),]

pdf("Pairs Correlation Covariates.pdf", width=9, height=8)
corrplot(cor(raster_10k),
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
  dplyr::select(decimalLongitude, decimalLatitude) %>%
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
AedesAegypti_Points <- AedesAegypti %>% dplyr::select(decimalLongitude, decimalLatitude) %>% unique
AedesAlbopictus <- filter(Mosquitoes_SpeciesOfInterest, species == "Aedes albopictus")
AedesAlbopictus_Points <- AedesAlbopictus %>% dplyr::select(decimalLongitude, decimalLatitude) %>% unique
AnophelesGambiae <- filter(Mosquitoes_SpeciesOfInterest, species == "Anopheles gambiae")
AnophelesGambiae_Points <- AnophelesGambiae %>% dplyr::select(decimalLongitude, decimalLatitude) %>% unique
# AnophelesStephensi <- filter(Mosquitoes_SpeciesOfInterest, species == "Anopheles stephensi")
# AnophelesStephensi_Points <- AnophelesStephensi %>% dplyr::select(decimalLongitude, decimalLatitude) %>% unique
CulexPipiens <- filter(Mosquitoes_SpeciesOfInterest, species == "Culex pipiens")
CulexPipiens_Points <- CulexPipiens %>% dplyr::select(decimalLongitude, decimalLatitude) %>% unique
CulexQuinquefasciatus <- filter(Mosquitoes_SpeciesOfInterest, species == "Culex quinquefasciatus")
CulexQuinquefasciatus_Points <- CulexQuinquefasciatus %>% dplyr::select(decimalLongitude, decimalLatitude) %>% unique
CulexTarsalis <- filter(Mosquitoes_SpeciesOfInterest, species == "Culex tarsalis")
CulexTarsalis_Points <- CulexTarsalis %>% dplyr::select(decimalLongitude, decimalLatitude) %>% unique

# Plotting species occurrences
pdf("Species Occurrence Map.pdf")
worldMap <- wrld_simpl[wrld_simpl@data$UN!="10",]
plot(worldMap, ylim=c(-80, 65), xlim = c(-180, 180), mar = c(0,0,0,0),
     bg = "lightblue3", border = NA, col = "antiquewhite")

points(AedesAegypti_Points, cex=0.05, col="grey30")
points(AedesAlbopictus_Points, cex=0.05, col="firebrick1")
points(AnophelesGambiae_Points, cex=0.05, col="#00ff00")
# points(AnophelesStephensi_Points, cex=0.05, col="brown")
points(CulexPipiens_Points, cex=0.05, col="darkgreen")
points(CulexQuinquefasciatus_Points, cex=0.05, col="blue")
points(CulexTarsalis_Points, cex=0.05, col="darkorange")

legend(100, -80, c("Aedes aegypti","Aedes albopictus","Anopheles gambiae",
                   "Culex pipiens","Culex quinquefasciatus","Culex tarsalis"),
       cex = 0.6, col=c("grey30","firebrick1","#00ff00","brown","darkmagenta","darkgreen","blue","darkorange"),
       pch = c(19,19,19,19,19,19,19),
       bg = "white")
dev.off()



