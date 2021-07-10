#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Compile figures and plots from the XGBoost model fit
#######################################################

source("E:/Documents/GitHub/mosquito-sdm/0-config.R")
setwd("E:/SynologyDrive/Tejas_Server/! Research/! Mordecai Lab/! Mosquito SDM Thermal Dependence/")


#------------------------------------------------------
# Read in cleaned mosquito species occurrence data and background
#------------------------------------------------------
Background_Culidiae <- read.csv("GBIF Datasets Cleaned/Background_Culicidae.csv", header = TRUE,
                                     encoding = "UTF-8", stringsAsFactors = FALSE)

Mosquitoes_SpeciesOfInterest <- read.csv("GBIF Datasets Cleaned/Mosquitoes_SpeciesOfInterest.csv", header = TRUE,
                                         encoding = "UTF-8", stringsAsFactors = FALSE)


#------------------------------------------------------
# Read in and name the environmental predictors
#------------------------------------------------------
predictorsPreStack <- alply(list.files("Environmental Predictors Merged",
                                       pattern = ".tif",
                                       full.names = TRUE), 1, function(file){
                                         print(file)
                                         rast <- raster(file)
                                         return(rast)
                                       })

rasterNames <- c("ELEV","EVIM","EVISD","FC","HP","PDQ","PhotoASTM","PhotoASTSD","PrecipASTM","PrecipASTSD","PWQ","TAM","TASD")
predictorsPreStack <- setNames(predictorsPreStack, rasterNames)
predictors <- predictorsPreStack %>% stack()



#------------------------------------------------------
# Read in the first day, last day, and length of the activity season rasters
#------------------------------------------------------
activityPreStack <- alply(list.files("Activity Season Metadata Rasters",
                                       pattern = ".tif",
                                       full.names = TRUE), 1, function(file){
                                         print(file)
                                         rast <- raster(file)
                                         return(rast)
                                       })

activityNames <- c("Photoperiod - First Day", "Photoperiod - Last Day", "Photoperiod - Length",
                   "Precipitation - First Day", "Precipitation - Last Day", "Precipitation - Length")
activityPreStack <- setNames(activityPreStack, activityNames)
activitySeason <- activityPreStack %>% stack()



data <- readRDS("MaxEnt Data - Random Bg and TAM/All Species MaxEnt Data.rds")



# # Read in and name the temperature-only covariates
# tempPreStack <- alply(list.files("All Temperature Merged Rasters",
#                                        pattern = ".tif",
#                                        full.names = TRUE), 1, function(file){
#                                          print(file)
#                                          rast <- raster(file)
#                                          return(rast)
#                                        })
# 
# tempRasterNames <- c("ATM","ATR","ATSD","TCM-DA","TCM","TCQ","THM-DA","THM","THQ")
# tempPreStack <- setNames(tempPreStack, tempRasterNames)
# tempPredictors <- tempPreStack %>% stack()
  



#### ~~ GENERAL FIGURES ~~ ####


### ENVIRONMENTAL COVARIATES PLOT ###
pdf("Environmental Covariates.pdf", width=8, height=8)
plot(predictors, legend.mar = c(8,8))
dev.off()


## ??? move pairs plot to script 7-raster-summary-figures.R

### PAIRS CORRELATION ANALYSIS ###
set.seed(seedNum)
pairs_possibleBg <- Background_Culicidae %>%
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



# ### PAIRS CORRELATION ANALYSIS - ALL TEMPERATURE VARS ###
# set.seed(seedNum)
# temp_possibleBg <- Background_Culicidae %>%
#   select(decimalLongitude, decimalLatitude) %>%
#   unique
# set.seed(seedNum)
# temp_possibleBg <- temp_possibleBg[sample(nrow(temp_possibleBg), 15000), ] # Over-sample to account for NA's
# temp_bg <- raster::extract(tempPredictors, temp_possibleBg)
# temp_bg <- temp_bg[complete.cases(temp_bg), ]
# g = 10000
# set.seed(seedNum)
# temp_sample <- temp_bg[sample(nrow(temp_bg), g), ] # Select 10k bg points for pairs sampling
# 
# pdf("All Temperature Covariates Pairs.pdf")
# corrplot(cor(temp_sample),
#          method = "color",
#          addCoef.col = "black",
#          tl.col = "black", tl.srt = 45,
#          type = "upper")
# dev.off()




### SPECIES OCCURRENCES MAP ###
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




### VECTOR DISTRIBUTION PLOTS ###
# Read in, plot, and save all of the MaxEnt vector distributions
modelProjections <- alply(list.files("MaxEnt Data Output/Vector Distribution TIFs",
                                     pattern = ".tif",
                                     full.names = TRUE), 1, function(file){
                                       print(file)
                                       rast <- raster(file)
                                       return(rast)
                                     })
modelProjections <- setNames(modelProjections, SpeciesOfInterest_Names)

for(i in 1:length(modelProjections)) {
  distPlotSave <- paste0(SpeciesOfInterest_Names[i]," Distribution.pdf")
  pdf(distPlotSave)
  plot(modelProjections[[i]])
  dev.off()
}

modelProjStack <- stack(modelProjections)
pdf("All Vectors Distribution.pdf")
plot(modelProjStack)
dev.off()






#### ~~ SPECIES PARTICULAR FIGURES ~~ ####

# Main loop to compile figures that are specific for each species
for(i in 1:length(SpeciesOfInterest_Names)) {
  
    # Load necessary data
    species_df <- data[[i]][[1]]
    bg <- data[[i]][[2]]
    occGPS_raw <- data[[i]][[3]]
    occGPS_train <- data[[i]][[4]]
    occGPS_eval <- data[[i]][[5]]
    occGPS_train_noNA <- data[[i]][[6]]
    occGPS_eval_noNA <- data[[i]][[7]]
    cells_train <- data[[i]][[8]]
    cells_eval <- data[[i]][[9]]
    predictors_train_df <- data[[i]][[10]]
    predictors_eval_df <- data[[i]][[11]]
    sdmData <- data[[i]][[12]]
    maxnet_fit <- data[[i]][[13]]
    maxnet_predict <- data[[i]][[14]]
    
  
    
    ### BIVARIATE PARTIAL DEPENDENCE PLOTS FOR TEMPERATURE VARS ###
    print(paste0("Plotting 2D and 3D bivariate temp-precip pdp's for ", SpeciesOfInterest_Names[i]))
    bivariate_TAM_PDQ_2D <- paste0(SpeciesOfInterest_Names[i]," Bivariate TAM-PDQ in 2D.pdf")
    pdf(bivariate_TAM_PDQ_2D)
    pd1 <- pdp::partial(maxnet_fit$model, train = predictors_train_df, pred.var = c("TAM","PDQ"), chull = TRUE)
    rwb <- colorRampPalette(c("red", "white", "blue"))
    print(plotPartial(pd1, col.regions = rwb))
    dev.off()
    
    bivariate_TAM_PWQ_2D <- paste0(SpeciesOfInterest_Names[i]," Bivariate TAM-PWQ in 2D.pdf")
    pdf(bivariate_TAM_PWQ_2D)
    pd2 <- pdp::partial(maxnet_fit$model, train = predictors_train_df, pred.var = c("TAM","PWQ"), chull = TRUE)
    rwb <- colorRampPalette(c("red", "white", "blue"))
    print(plotPartial(pd2, col.regions = rwb))
    dev.off()
    
    bivariate_TAM_PDQ_3D <- paste0(SpeciesOfInterest_Names[i]," Bivariate TAM-PDQ in 3D.pdf")
    pdf(bivariate_TAM_PDQ_3D)
    print(plotPartial(pd1, levelplot = FALSE, zlab = "cmedv", colorkey = TRUE, 
                      screen = list(z = -20, x = -60)))
    dev.off()
    
    bivariate_TAM_PWQ_3D <- paste0(SpeciesOfInterest_Names[i]," Bivariate TAM-PWQ in 3D.pdf")
    pdf(bivariate_TAM_PWQ_3D)
    print(plotPartial(pd2, levelplot = FALSE, zlab = "cmedv", colorkey = TRUE, 
                      screen = list(z = -20, x = -60)))
    dev.off()
    
    
  
    ### COVARIATE RESPONSES ###
    # Set up a dictionary to go from var.name to clean name for plotting
    print(paste0("Plotting covariate responses for ", SpeciesOfInterest_Names[i]))
    var.names.plotting = data.frame(
      var.names = c("ELEV","EVIM","EVISD","FC","HPD","PDQ","PWQ","TAM"),
      plotting.names = c("ELEV","EVIM","EVISD","FC %","HPD","PDQ","PWQ","TAM"),
      stringsAsFactors = FALSE)
  
    # j tracks the jth most important variable that we are looking at
    responseSave <- paste0(SpeciesOfInterest_Names[i]," Covariate Responses.pdf")
    pdf(file = responseSave, width = 8, height = 10)
    par(mfrow = c(4,3))
    par(mar=c(5, 6, 1.5, 2.5) + 0.1)
    par(oma = c(1, 2, 2, 4))
    par(cex.axis = 1.2, cex.lab = 1.2)
    for (j in 1:length(predictorsPreStack)){
      if( j %% par('mfrow')[2] == 1){left_ax = TRUE
      } else{left_ax = FALSE}
      if(j %% par('mfrow')[2] == 0){right_ax = TRUE
      } else{right_ax = FALSE}
      var.name = var.names.plotting[j,1]
      print(var.name)
  
      # Identify which of the predictor names it was
      var.data = predictors_train_df[[var.name]]
      test_resp <- pdp::partial(maxnet_fit$model, pred.var = var.name,
                                quantiles = TRUE, probs = c(1:99/100),
                                plot = FALSE,
                                train = sdmData)
      if(is.factor(var.data)) {
        test_sums = table(var.data)
        test_hist <- graphics::barplot(test_sums, col = "grey", border = "white",
                                       las = 1, ylab = "",
                                       xlab = "")
        title(xlab = paste0(letters[j], ". ",
                            mapvalues(var.name,
                                      from = var.names.plotting$var.names,
                                      to = var.names.plotting$plotting.names,
                                      warn_missing = FALSE)),
              line = 4)
        title(ylab = ifelse(left_ax, "Density", ""),
              line = 4)
        par(new = TRUE)
        plot(test_hist,
             test_resp$yhat,
             xlab = "", ylab = "", axes = FALSE, bty = "n",
             type = "l")
        axis(side=4, at = pretty(range(test_resp$yhat)), las = 1)
        mtext(ifelse(right_ax, "Marginal Effect", ""), side=4, line=4, cex = 0.75)
        box()
      } else {
        resp_x = test_resp[,1]
        test_hist <- graphics::hist(var.data,
                                    plot = F)
        plot(test_hist, freq = F, col = "grey", border = "white",
             las = 1,
             ylab = "", xlab = "",
             main = NULL)
        title(ylab = ifelse(left_ax, "Density", ""),
              line = 5)
        title(xlab = paste0(letters[j], ". ",
                            mapvalues(var.name,
                                      from = var.names.plotting$var.names,
                                      to = var.names.plotting$plotting.names,
                                      warn_missing = FALSE)),
              line = 3)
        par(new = TRUE)
        plot(resp_x,
             test_resp$yhat,
             xlab = "", ylab = "", axes = FALSE, bty = "n",
             type = "l")
        axis(side=4, at = pretty(range(test_resp$yhat)), las = 1)
        mtext(ifelse(right_ax, "Marginal Effect", ""), side=4, line=4, cex = 0.75)
        box()
      }
    }
    dev.off()
  
  
    
    
    
    ### DENSITY PLOTS ###
    # Load necessary data
    densityPlot_df <- sdmData %>%
      dplyr::mutate(HPD_log = log(HPD)) %>% 
      dplyr::select(-HPD)
    colnames(densityPlot_df)[1] <- "occurrence"
    
    # For each variable, plot the density curves for occurrences and background points
    print(paste0("Creating density plots for ", SpeciesOfInterest_Names[i]))
    density_save <- paste0(SpeciesOfInterest_Names[i]," Density.pdf")
    pdf(density_save)
    par(mfrow = c(3,3), # Set number of rows and columns
        mar = c(3, 3.5, 1, 1) + 1, # Margins of individual panels
        oma = c(3, 3, 3, 3)) # Outer margins
    a_ply(colnames(densityPlot_df)[-1], 1, function(p){
      occ_density <- densityPlot_df %>% 
        filter(occurrence == 1) %>% 
        pull(p) %>% 
        density
      bg_density <- densityPlot_df %>% 
        filter(occurrence == 0) %>% 
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
    
    
    
    
  
    ### PREDICTION PLOTS ###
    # Plot training occurrence points onto distribution
    print(paste0("Plotting training points for ", SpeciesOfInterest_Names[i]))
    compareSave_training <- paste0(SpeciesOfInterest_Names[i]," Training Prediction.pdf")
    pdf(file = compareSave_training)
    par(mfrow = c(1,1))
    plot(maxnet_predict, axes = F, box = F, legend = T)
    points(occGPS_train, pch = 16, cex = 0.25, col = "black")
    dev.off()
  
    # Plot evaluation occurrence points onto distribution
    print(paste0("Plotting evaluation prediction for ", SpeciesOfInterest_Names[i]))
    compareSave_evaluation <- paste0(SpeciesOfInterest_Names[i]," Evaluation Prediction.pdf")
    pdf(file = compareSave_evaluation)
    par(mfrow = c(1,1))
    plot(maxnet_predict, axes = F, box = F, legend = T)
    points(occGPS_eval, pch = 16, cex = 0.35, col = "black")
    dev.off()
  
  
  
  
  
    ### ROC AUC CURVES ###
    # Calculate ROC and AUC for training and evaluation data
    print(paste0("Calculating the training AUC for ", SpeciesOfInterest_Names[i]))
    roc_auc_save <- paste0(SpeciesOfInterest_Names[i]," AUC.pdf")
    pdf(roc_auc_save)
    plot.roc(c(rep(1, nrow(cells_train)), rep(0, nrow(bg))), # Training data
             as.vector(predict(maxnet_fit$model, predictors_train_df)),
             print.auc = TRUE,
             print.auc.y = 0.4,
             identity.col = "grey50",
             col = "firebrick1",
             legacy.axes = TRUE,
             grid = TRUE)
    plot.roc(c(rep(1, nrow(cells_eval)), rep(0, nrow(bg))), # Evaluation data
             as.vector(predict(maxnet_fit$model, predictors_eval_df)),
             add = TRUE,
             print.auc = TRUE,
             print.auc.y = 0.35,
             col = "#1c61b6")
    legend("bottomright", legend=c("Training", "Evaluation"),
           col=c("firebrick1", "#1c61b6"), lwd=2)
    dev.off()
  

  
}






