#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Compile figures and plots from the XGBoost model fits
#######################################################

if(Sys.getenv('SLURM_JOB_ID') != ""){ # Check if the script is running on Sherlock remote computing cluster
  library(dplyr)
  library(magrittr)
  library(tidyr)
  library(xgboost)
  library(rBayesianOptimization)
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

  # Use the command line arguments supplied to set which species we'll be running 
  args <- commandArgs(TRUE) 
  species_inds <- as.numeric(args[1]) 
  
} else {
  source("C:/Users/tejas/Documents/GitHub/mosquito-sdm/0-config.R")
}



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

yearRound_index <- c(1:6,11:12,15,13:14)
photoSeason_index <- c(1:6,11:12,15,7:8)
precipSeason_index <- c(1:6,11:12,15,9:10)

predictors_yearRound <- predictors_preStack[yearRound_index] %>% stack()
predictors_photoSeason <- predictors_preStack[photoSeason_index] %>% stack()
predictors_precipSeason <- predictors_preStack[precipSeason_index] %>% stack()


#------------------------------------------------------
# Loop through each species, or if running cluster computing, select the given species
#------------------------------------------------------
for(i in species_inds) {
  print(paste0("Species of interest is ", SpeciesOfInterest_Names[[i]]))
  
  #------------------------------------------------------
  # Data load-in
  #------------------------------------------------------
  xgb.fit <- xgb.load(paste0("XGBoost_Outputs/",SpeciesOfInterest_Underscore[[i]],"_XGBoost.model"))
  sdm_data <- readRDS(paste0("XGBoost_Outputs/",SpeciesOfInterest_Underscore[[i]],"_SDM_Data_Predictions.RDS"))
  
  train_data <- sdm_data %>% filter(Data_Split == "Training")
  train_xgb_dmatrix <- xgb.DMatrix(data = as.matrix(train_data %>% dplyr::select(c(7:17))),
                                   label = as.matrix(train_data %>% dplyr::select(3)))
  
  eval_data <- sdm_data %>% filter(Data_Split == "Evaluation")
  eval_xgb_dmatrix <- xgb.DMatrix(data = as.matrix(eval_data %>% dplyr::select(c(7:17))),
                                  label = as.matrix(eval_data %>% dplyr::select(3)))
  
  
  #------------------------------------------------------
  # Select predictors according to activity season
  #------------------------------------------------------
  if(SpeciesOfInterest_Names[[i]] == "Aedes aegypti" |
     SpeciesOfInterest_Names[[i]] == "Anopheles stephensi" |
     SpeciesOfInterest_Names[[i]] == "Culex quinquefasciatus") {
    predictor_names <- rasterNames[yearRound_index]
    predictors <- predictors_yearRound
    }
  
  if(SpeciesOfInterest_Names[[i]] == "Aedes albopictus" |
     SpeciesOfInterest_Names[[i]] == "Culex pipiens" |
     SpeciesOfInterest_Names[[i]] == "Culex tarsalis") {
    predictor_names <- rasterNames[photoSeason_index]
    predictors <- predictors_photoSeason
    }
  
  if(SpeciesOfInterest_Names[[i]] == "Anopheles gambiae") {
    predictor_names <- rasterNames[precipSeason_index]
    predictors <- predictors_precipSeason
    }
  
  
  #------------------------------------------------------
  ## 1. ROC CURVES ##
  #------------------------------------------------------
  save_name <- paste0("XGBoost_Figures/ROC_Curves/",SpeciesOfInterest_Underscore[[i]],"_ROC.pdf")
  y_train <- train_data$Occ1_or_Bg0
  y_eval <- eval_data$Occ1_or_Bg0
  pred_train <- train_data$Predicted_Value
  pred_eval <- eval_data$Predicted_Value
  
  pdf(save_name)
  plot.roc(y_train, pred_train,
           print.auc = TRUE,
           print.auc.y = 0.4,
           identity.col = "grey50",
           col = "maroon", lwd = 3,
           legacy.axes = TRUE,
           grid = TRUE)
  
  plot.roc(y_eval, pred_eval,
           add = TRUE,
           print.auc = TRUE,
           print.auc.y = 0.35,
           identity.col = "grey50",
           col = "dodgerblue4", lwd = 3,
           legacy.axes = TRUE,
           grid = TRUE)
  
  legend("bottom",
         legend=c("Training","Evaluation","Reference"),
         col=c("maroon","dodgerblue4","grey50"),
         lwd=5, cex=0.7, xpd = TRUE, horiz = TRUE)
  dev.off()
  
 
  #------------------------------------------------------
  ## 2. VARIABLE IMPORTANCE PLOTS ##
  #------------------------------------------------------
  save_name <- paste0("XGBoost_Figures/Variable_Importance_Plots/",SpeciesOfInterest_Underscore[[i]],"_Variable_Importance_Plot.pdf")
  imp_matrix <- xgb.importance(colnames(train_xgb_dmatrix), model = xgb.fit)
  
  pdf(save_name)
  vip_plot <- xgb.ggplot.importance(imp_matrix) +
    theme_bw() +
    theme(legend.position="none") +
    ggtitle(SpeciesOfInterest_Names[[i]]) +
    ylab("Variable Importance (Gain)\n") +
    xlab("Predictor Variable\n") +
    scale_fill_grey(start=0.6, end=0.6)
  print(vip_plot)
  dev.off()
  
  
  #------------------------------------------------------
  ## 3. UNIVARIATE PARTIAL DEPENDENCE PLOTS ##
  #------------------------------------------------------
  for(k in 1:length(predictor_names)) {
    save_name <- paste0("XGBoost_Figures/Univariate_PDPs/",SpeciesOfInterest_Underscore[[i]],"_",predictor_names[[k]],"_Univariate.pdf")
    pdf(save_name)
    plot(pdp::partial(xgb.fit$handle, pred.var = predictor_names[[k]], 
                      train = as.matrix(train_data %>% dplyr::select(c(7:17))),
                      plot = TRUE, plot.engine = "ggplot2",
                      chull = TRUE, out = "pdf") +
           theme_bw())
    dev.off()
  }
  
  
  
  #------------------------------------------------------
  ## 4. BIVARIATE PARTIAL DEPENDENCE PLOTS ##
  #------------------------------------------------------
  if(SpeciesOfInterest_Names[[i]] == "Aedes aegypti" |
     SpeciesOfInterest_Names[[i]] == "Anopheles stephensi" |
     SpeciesOfInterest_Names[[i]] == "Culex quinquefasciatus") {
    save_name <- paste0("XGBoost_Figures/Bivariate_PDPs/",SpeciesOfInterest_Underscore[[i]],"_TAM_TASD_Bivariate.pdf")
    pdf(save_name)
    pdp_bivariate <- pdp::partial(xgb.fit$handle, train = as.matrix(train_data %>% dplyr::select(c(7:17))),
                                  pred.var = c("TAM","TASD"), chull = TRUE)
    color_scheme <- colorRampPalette(c("lightgrey","blue"))
    print(plotPartial(pdp_bivariate, col.regions = color_scheme))
    dev.off()
  }
  
  if(SpeciesOfInterest_Names[[i]] == "Aedes albopictus" |
     SpeciesOfInterest_Names[[i]] == "Culex pipiens" |
     SpeciesOfInterest_Names[[i]] == "Culex tarsalis") {
    save_name <- paste0("XGBoost_Figures/Bivariate_PDPs/",SpeciesOfInterest_Underscore[[i]],"_PhotoASTM_PhotoASTSD_Bivariate.pdf")
    pdf(save_name)
    pdp_bivariate <- pdp::partial(xgb.fit$handle, train = as.matrix(train_data %>% dplyr::select(c(7:17))),
                                  pred.var = c("PhotoASTM","PhotoASTSD"), chull = TRUE)
    color_scheme <- colorRampPalette(c("lightgrey","blue"))
    print(plotPartial(pdp_bivariate, col.regions = color_scheme))
    dev.off()
  }
  
  if(SpeciesOfInterest_Names[[i]] == "Anopheles gambiae") {
    save_name <- paste0("XGBoost_Figures/Bivariate_PDPs/",SpeciesOfInterest_Underscore[[i]],"_PrecipASTM_PrecipASTSD_Bivariate.pdf")
    pdf(save_name)
    pdp_bivariate <- pdp::partial(xgb.fit$handle, train = as.matrix(train_data %>% dplyr::select(c(7:17))),
                                  pred.var = c("PrecipASTM","PrecipASTSD"), chull = TRUE)
    color_scheme <- colorRampPalette(c("lightgrey","blue"))
    print(plotPartial(pdp_bivariate, col.regions = color_scheme))
    dev.off()
  }

  
  
}



#------------------------------------------------------
## 6. COMPILED ROC CURVES ##
#------------------------------------------------------
roc_data <- purrr::map_dfr(species_inds, function(i){
  #------------------------------------------------------
  # Data load-in
  #------------------------------------------------------
  xgb.fit <- xgb.load(paste0("XGBoost_Outputs/",SpeciesOfInterest_Underscore[[i]],"_XGBoost.model"))
  sdm_data <- readRDS(paste0("XGBoost_Outputs/",SpeciesOfInterest_Underscore[[i]],"_SDM_Data_Predictions.RDS"))
  
  train_data <- sdm_data %>% filter(Data_Split == "Training")
  eval_data <- sdm_data %>% filter(Data_Split == "Evaluation")
  
  y_train <- train_data$Occ1_or_Bg0
  y_eval <- eval_data$Occ1_or_Bg0
  pred_train <- train_data$Predicted_Value
  pred_eval <- eval_data$Predicted_Value
  
  
  if(SpeciesOfInterest_Names[[i]] == "Aedes aegypti" |
     SpeciesOfInterest_Names[[i]] == "Anopheles stephensi" |
     SpeciesOfInterest_Names[[i]] == "Culex quinquefasciatus") {
    predictor_names <- rasterNames[yearRound_index]
    predictors <- predictors_yearRound
  }
  
  if(SpeciesOfInterest_Names[[i]] == "Aedes albopictus" |
     SpeciesOfInterest_Names[[i]] == "Culex pipiens" |
     SpeciesOfInterest_Names[[i]] == "Culex tarsalis") {
    predictor_names <- rasterNames[photoSeason_index]
    predictors <- predictors_photoSeason
  }
  
  if(SpeciesOfInterest_Names[[i]] == "Anopheles gambiae") {
    predictor_names <- rasterNames[precipSeason_index]
    predictors <- predictors_precipSeason
  }
  
  
  #------------------------------------------------------
  # Calculate sensitivity and specificity for training and eval
  #------------------------------------------------------
  sens_train <- roc(y_train, pred_train)$sensitivities
  spec_train <- roc(y_train, pred_train)$specificities
  sens_eval <- roc(y_eval, pred_eval)$sensitivities
  spec_eval <- roc(y_eval, pred_eval)$specificities
  
  
  rbind(data.frame(species = SpeciesOfInterest_Names[i],
                   train_eval = "Training",
                   tpr = sens_train,
                   fpr = 1 - spec_train),
        data.frame(species = SpeciesOfInterest_Names[i],
                   train_eval = "Evaluation",
                   tpr = sens_eval,
                   fpr = 1 - spec_eval))
  
})


auc_vals <- purrr::map_dfr(species_inds, function(i){
  #------------------------------------------------------
  # Data load-in
  #------------------------------------------------------
  xgb.fit <- xgb.load(paste0("XGBoost_Outputs/",SpeciesOfInterest_Underscore[[i]],"_XGBoost.model"))
  sdm_data <- readRDS(paste0("XGBoost_Outputs/",SpeciesOfInterest_Underscore[[i]],"_SDM_Data_Predictions.RDS"))
  
  train_data <- sdm_data %>% filter(Data_Split == "Training")
  eval_data <- sdm_data %>% filter(Data_Split == "Evaluation")
  
  y_train <- train_data$Occ1_or_Bg0
  y_eval <- eval_data$Occ1_or_Bg0
  pred_train <- train_data$Predicted_Value
  pred_eval <- eval_data$Predicted_Value
 
  
  #------------------------------------------------------
  # Calculate AUC for training and eval
  #------------------------------------------------------
  auc_train <- roc(y_train, pred_train)$auc %>% round(3)
  auc_eval <- roc(y_eval, pred_eval)$auc %>% round(3)
  
  
  rbind(data.frame(species = SpeciesOfInterest_Names[i],
                   train_eval = "Training",
                   auc = auc_train),
        data.frame(species = SpeciesOfInterest_Names[i],
                   train_eval = "Evaluation",
                   auc = auc_eval))
})


roc_plot <- ggplot(roc_data, aes(x = fpr, y = tpr, color = train_eval)) +
  geom_line(lwd=0.5) +
  facet_wrap(~ species, ncol=2) +
  geom_text(data = auc_vals %>% dplyr::filter(train_eval == "Training"),
            aes(x = .75, y = 0.23, label = paste0("Train AUC = ", auc)),
            show.legend = F) +
  geom_text(data = auc_vals %>% dplyr::filter(train_eval == "Evaluation"),
            aes(x = .75, y = 0.1, label = paste0("Eval AUC = ", auc)),
            show.legend = F) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 11, face = "italic")) +
  xlab("\nFalse Positive Rate (1 - Specificity)") +
  ylab("True Positive Rate (Sensitivity)\n") +
  scale_color_manual(values = c("Training" = "maroon",
                                "Evaluation" = "dodgerblue4")) +
  labs(color = "Partition") 


ggsave("XGBoost_Figures/Compiled ROCs/Compiled_ROCs.pdf", roc_plot, device = "pdf", width=8, height=10)
ggsave("XGBoost_Figures/Compiled ROCs/Compiled_ROCs.jpg", roc_plot, device = "jpg", width=8, height=10)





#------------------------------------------------------
## 7. COMPILED VARIABLE IMPORTANCE PLOTS ##
#------------------------------------------------------
vip <- auc_vals <- purrr::map_dfr(species_inds, function(i){
  #------------------------------------------------------
  # Data load-in
  #------------------------------------------------------
  xgb.fit <- xgb.load(paste0("XGBoost_Outputs/",SpeciesOfInterest_Underscore[[i]],"_XGBoost.model"))
  sdm_data <- readRDS(paste0("XGBoost_Outputs/",SpeciesOfInterest_Underscore[[i]],"_SDM_Data_Predictions.RDS"))
  
  train_data <- sdm_data %>% filter(Data_Split == "Training")
  eval_data <- sdm_data %>% filter(Data_Split == "Evaluation")
  
  train_xgb_dmatrix <- xgb.DMatrix(data = as.matrix(train_data %>% dplyr::select(c(7:17))),
                                   label = as.matrix(train_data %>% dplyr::select(3)))
  eval_xgb_dmatrix <- xgb.DMatrix(data = as.matrix(eval_data %>% dplyr::select(c(7:17))),
                                  label = as.matrix(eval_data %>% dplyr::select(3)))
  
  if(SpeciesOfInterest_Names[[i]] == "Aedes aegypti" |
     SpeciesOfInterest_Names[[i]] == "Anopheles stephensi" |
     SpeciesOfInterest_Names[[i]] == "Culex quinquefasciatus") {
    predictor_names <- rasterNames[yearRound_index]
    predictors <- predictors_yearRound
  }
  
  if(SpeciesOfInterest_Names[[i]] == "Aedes albopictus" |
     SpeciesOfInterest_Names[[i]] == "Culex pipiens" |
     SpeciesOfInterest_Names[[i]] == "Culex tarsalis") {
    predictor_names <- rasterNames[photoSeason_index]
    predictors <- predictors_photoSeason
  }
  
  if(SpeciesOfInterest_Names[[i]] == "Anopheles gambiae") {
    predictor_names <- rasterNames[precipSeason_index]
    predictors <- predictors_precipSeason
  }
  
  
  #------------------------------------------------------
  # Calculate VIP matrix
  #------------------------------------------------------
  imp_matrix <- xgb.importance(colnames(train_xgb_dmatrix), model = xgb.fit) %>%
    cbind(Species = SpeciesOfInterest_Names[i])
  
})


vip %<>% 
  mutate(color = case_when(Feature %in% c("TAM", "PhotoASTM", "PrecipASTM") ~ "red", 
                           Feature %in% c("TASD", "PhotoASTSD", "PrecipASTSD") ~ "blue",
                           T ~ "grey"))

vip_plot <- ggplot(vip, aes(xmin = 0, xmax = Gain, y = reorder_within(Feature, +Gain, Species), color = I(color), fill = I(color))) + 
  geom_linerange(lwd = 3.5) + 
  scale_y_reordered() +
  facet_wrap(~Species, ncol = 2, scales = "free") + 
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.x = element_text(size = 11, face = "italic")) +
  xlab("\nGain") +
  ylab("Feature\n")

ggsave("XGBoost_Figures/Compiled VIPs/Compiled_VIPs.pdf", vip_plot, device = "pdf", width=8, height=10)
ggsave("XGBoost_Figures/Compiled VIPs/Compiled_VIPs.jpg", vip_plot, device = "jpg", width=8, height=10)


# ????
#------------------------------------------------------
## 8. SCATTERPLOT OF THERMAL MINIMA FROM M(T) vs. PDP ##
#------------------------------------------------------
pdp_storage = data.frame(species = c(),
                         pdp_min = c())

for(i in species_inds) {
  #------------------------------------------------------
  # Data load-in
  #------------------------------------------------------
  xgb.fit <- xgb.load(paste0("XGBoost_Outputs/",SpeciesOfInterest_Underscore[[i]],"_XGBoost.model"))
  sdm_data <- readRDS(paste0("XGBoost_Outputs/",SpeciesOfInterest_Underscore[[i]],"_SDM_Data_Predictions.RDS"))
  
  train_data <- sdm_data %>% filter(Data_Split == "Training")
  eval_data <- sdm_data %>% filter(Data_Split == "Evaluation")
  
  if(SpeciesOfInterest_Names[[i]] == "Aedes aegypti" |
     SpeciesOfInterest_Names[[i]] == "Anopheles stephensi" |
     SpeciesOfInterest_Names[[i]] == "Culex quinquefasciatus") {
    predictor_names <- rasterNames[yearRound_index]
    predictors <- predictors_yearRound
  }
  
  if(SpeciesOfInterest_Names[[i]] == "Aedes albopictus" |
     SpeciesOfInterest_Names[[i]] == "Culex pipiens" |
     SpeciesOfInterest_Names[[i]] == "Culex tarsalis") {
    predictor_names <- rasterNames[photoSeason_index]
    predictors <- predictors_photoSeason
  }
  
  if(SpeciesOfInterest_Names[[i]] == "Anopheles gambiae") {
    predictor_names <- rasterNames[precipSeason_index]
    predictors <- predictors_precipSeason
  }
  
  
  #------------------------------------------------------
  # Create PDP and extract the temperature value at minimum yhat (Tmin from PDP)
  #------------------------------------------------------
  pdp_df <- pdp::partial(xgb.fit$handle, pred.var = predictor_names[predictor_names %in% c("TAM","PhotoASTM","PrecipASTM")], 
                         train = as.matrix(train_data %>% dplyr::select(c(7:17))))
  pdp_min <- pdp_df[which.min(pdp_df$yhat),] %>% dplyr::select(1) %>% as.numeric() %>% round(1)
  
  
  #------------------------------------------------------
  # Add the Tmin value to storage dataframe
  #------------------------------------------------------
  pdp_storage %<>% rbind(c(SpeciesOfInterest_Names[i], pdp_min))
  
}


thermal_minima_df <- data.frame(Species = c("Aedes aegypti",
                                            "Aedes albopictus",
                                            "Anopheles gambiae",
                                            "Anopheles stephensi",
                                            "Culex pipiens",
                                            "Culex quinquefasciatus",
                                            "Culex tarsalis"),
                                Mt = c(16, 15.5, 18, 15, 11, 13, 9),
                                PDP = c(11, 9.5, 18, 16, 8.5, 11, 7.5))

## ?? plot scatterplot and recreate figure from manuscript
## ?? get spearman and R2 value
## ?? once i get proper min values, then replace PDP values



#------------------------------------------------------
## 9. SPATIAL PREDICTION MAPS OF MOSQUITO OCCURRENCE ##
#------------------------------------------------------
xgb_pred <- function(model, data, ...) {
  predict(model, newdata = as.matrix(data), ...)
}

for(i in species_inds) {
  tic <- Sys.time()
  
  #------------------------------------------------------
  # Select predictors according to activity season
  #------------------------------------------------------
  if(SpeciesOfInterest_Names[[i]] == "Aedes aegypti" |
     SpeciesOfInterest_Names[[i]] == "Anopheles stephensi" |
     SpeciesOfInterest_Names[[i]] == "Culex quinquefasciatus") {
    predictor_names <- rasterNames[yearRound_index]
    predictors <- predictors_yearRound
  }
  
  if(SpeciesOfInterest_Names[[i]] == "Aedes albopictus" |
     SpeciesOfInterest_Names[[i]] == "Culex pipiens" |
     SpeciesOfInterest_Names[[i]] == "Culex tarsalis") {
    predictor_names <- rasterNames[photoSeason_index]
    predictors <- predictors_photoSeason
  }
  
  if(SpeciesOfInterest_Names[[i]] == "Anopheles gambiae") {
    predictor_names <- rasterNames[precipSeason_index]
    predictors <- predictors_precipSeason
  }
  
  
  #------------------------------------------------------
  # Acquire model fit and spatial prediction
  #------------------------------------------------------
  xgb.fit <- xgb.load(paste0("XGBoost_Outputs/",SpeciesOfInterest_Underscore[[i]],"_XGBoost.model"))
  p <- predict(predictors, model=xgb.fit, fun=xgb_pred)
  save_name <- paste0(SpeciesOfInterest_Underscore[i],"_Spatial_Prediction_Map.pdf")
  pdf(paste0("! Figures/Spatial Prediction Maps/",save_name,".pdf"))
  plot(p)
  dev.off()
  
}


#------------------------------------------------------
# 10. OVERLAYED M(T) AND PDP PLOTS
#------------------------------------------------------







## ?????? need to clean and delete below





## ??? save the model fit and save optimal params as a single list, write.RDS() 



  

### NEW SCRIPT FOR FIGURES/PLOTTING FROM BELOW:

## ?? can save all the best-fitting models as .RDS  .. new script to make all the plots for each best-fit model
## vip, pdp, bivariate pdps, maps of probability of occurrence, predict for the test set of 20% evals and how well we did on them


# Variable importance plots
save_name <- paste0(SpeciesOfInterest_Names[i]," Variable Importance Plot.pdf")
pdf(save_name)
plot(vip(xgb.fit, num_features = 10))
dev.off()


#------------------------------------------------------
# Univariate partial dependence plots
#------------------------------------------------------
for(k in 1:length(predictors)) {
  save_name <- paste0(SpeciesOfInterest_Names[i]," ",predictors[[k]],".pdf")
  pdf(save_name)
  plot(partial(xgb.fit, pred.var = predictors[[k]],
               plot = T, chull = T, plot.engine = "ggplot2",
               train = as.matrix(train_data %>% dplyr::select(-Occ1_or_Bg0)),
               out="pdf"))
  dev.off()
}


#------------------------------------------------------
# Bivariate pdp for the 2 temperature variables
#------------------------------------------------------
if(SpeciesOfInterest_Names[[i]] == "Aedes aegypti" |
   SpeciesOfInterest_Names[[i]] == "Anopheles stephensi" |
   SpeciesOfInterest_Names[[i]] == "Culex quinquefasciatus") {
  save_name <- paste0(SpeciesOfInterest_Names[i]," TAM-TASD Bivariate.pdf")
  pdf(save_name)
  pd <- pdp::partial(xgb.fit, train = as.matrix(train_data %>% dplyr::select(-Occ1_or_Bg0)),
                     pred.var = c("TAM","TASD"), chull = TRUE)
  rwb <- colorRampPalette(c("red", "white", "blue"))
  print(plotPartial(pd, col.regions = rwb))
  dev.off()
}

if(SpeciesOfInterest_Names[[i]] == "Aedes aegypti" |
   SpeciesOfInterest_Names[[i]] == "Anopheles stephensi" |
   SpeciesOfInterest_Names[[i]] == "Culex quinquefasciatus") {
  save_name <- paste0(SpeciesOfInterest_Names[i]," TAM-TASD Bivariate.pdf")
  pdf(save_name)
  pd <- pdp::partial(xgb.fit, train = as.matrix(train_data %>% dplyr::select(-Occ1_or_Bg0)),
                     pred.var = c("TAM","TASD"), chull = TRUE)
  rwb <- colorRampPalette(c("red", "white", "blue"))
  print(plotPartial(pd, col.regions = rwb))
  dev.off()
}






## ??? predict out-of-sample points, compare model performance on training v testing data (to what extent are we overfitting? diverge = overfit to training)
## essentially, raster of predicted probabilities



## ??? map of predictions


#------------------------------------------------------
# ## ??? SALVAGE THESE BELOW AND MODIFY
#------------------------------------------------------


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



# Print elapsed time for model fit
toc <- Sys.time()
toc - tic


### variable importance plots
# ?? xgb.importance()


### ??





