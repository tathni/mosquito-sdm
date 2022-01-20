#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Compile figures and plots from the XGBoost model fits
#######################################################

source("E:/Documents/GitHub/mosquito-sdm/0-config.R")




## ??? for loop to load in the species xgboost model fits one by one

## ??? merge the training and eval roc plots into one with a  unified legend

#------------------------------------------------------
# Plot ROC curves
#------------------------------------------------------
paste0("Plotting training ROC curve")
y <- train_data[, 1]
rownames(train_data) <- NULL
pred <- predict(xgb.fit, xgb.DMatrix(as.matrix(train_data[2:10])))
save_name <- paste0(SpeciesOfInterest_Names[i]," ROC Training.pdf")
pdf(save_name)
plot.roc(y, pred,
         print.auc = TRUE,
         print.auc.y = 0.4,
         identity.col = "grey50",
         col = "maroon",
         legacy.axes = TRUE,
         grid = TRUE)
dev.off()

paste0("Plotting evaluation ROC curve")
eval_data <- data %>% filter(Species == SpeciesOfInterest_Names[i], DataSplit == "Evaluation") %>% dplyr::select(c(5:14))
y <- eval_data[, 1]
rownames(eval_data) <- NULL
pred <- predict(xgb.fit, xgb.DMatrix(as.matrix(eval_data[2:10])))
save_name <- paste0(SpeciesOfInterest_Names[i]," ROC Evaluation.pdf")
pdf(save_name)
plot.roc(y, pred,
         print.auc = TRUE,
         print.auc.y = 0.4,
         identity.col = "grey50",
         col = "dodgerblue4",
         legacy.axes = TRUE,
         grid = TRUE)
dev.off()



## ??? save the model fit and save optimal params as a single list, write.RDS() 

vip_data <- vip(xgb.fit)
xgb.importance(model = xgb.fit)

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
save_name <- paste0(SpeciesOfInterest_Names[i]," TAM-TASD Bivariate.pdf")
pdf(save_name)
pd <- pdp::partial(xgb.fit, train = as.matrix(train_data %>% dplyr::select(-Occ1_or_Bg0)), pred.var = c("TAM","TASD"), chull = TRUE)
rwb <- colorRampPalette(c("red", "white", "blue"))
print(plotPartial(pd, col.regions = rwb))
dev.off()




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




