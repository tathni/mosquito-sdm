#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Run the XGBoost machine learning model by species
#######################################################


if(Sys.getenv('SLURM_JOB_ID') != ""){ # Check if the script is running on Sherlock remote computing cluster
  library(dplyr)
  library(magrittr)
  library(tidyr)
  library(xgboost)
  library(rBayesianOptimization)
  library(ggplot2)
  library(pROC)
  library(plotROC)
  usable.cores <- Sys.getenv("SLURM_NTASKS_PER_NODE")
  seedNum <- 250
  
  # Use the command line arguments supplied to set which species we'll be running 
  args <- commandArgs(TRUE) 
  species_inds <- as.numeric(args[1]) 
  
} else {
  source("E:/Documents/GitHub/mosquito-sdm/0-config.R")
  usable.cores <- 2
  species_inds <- 1:8 
}


#------------------------------------------------------
# Load in lists
#------------------------------------------------------
SpeciesOfInterest_Names <- c("Aedes aegypti",
                             "Aedes albopictus",
                             "Anopheles gambiae",
                             "Anopheles stephensi",
                             "Culex annulirostris",
                             "Culex pipiens",
                             "Culex quinquefasciatus",
                             "Culex tarsalis")

SpeciesOfInterest_Underscore <- c("Aedes_aegypti",
                                  "Aedes_albopictus",
                                  "Anopheles_gambiae",
                                  "Anopheles_stephensi",
                                  "Culex_annulirostris",
                                  "Culex_pipiens",
                                  "Culex_quinquefasciatus",
                                  "Culex_tarsalis")


#------------------------------------------------------
# Read in predictor data
#------------------------------------------------------
sdm_data <- read.csv("Analysis_Dataset/SDM_Data.csv")


#------------------------------------------------------
# Set bounds for XGBoost parameters
#------------------------------------------------------
param_bounds <- list(eta = c(0.0001, 0.3),
                     gamma = c(0, 100),
                     max_depth = c(2L, 50L),
                     subsample = c(0.25, 1),
                     colsample_bytree = c(0.5, 1),
                     min_child_weight = c(1L, 50L))

max_xgb_rounds <- 10000
bayes_opt_n_init <- 24
bayes_opt_n_iter <- 24


#------------------------------------------------------
# Loop through each species, or if running cluster computing, select the given species
#------------------------------------------------------
for(i in species_inds) {
  print(paste0("Species of interest is ", SpeciesOfInterest_Names[[i]]))
  
  #------------------------------------------------------
  # Select the appropriate predictors for the current species
  #------------------------------------------------------
  if(SpeciesOfInterest_Names[[i]] == "Aedes aegypti" |
     SpeciesOfInterest_Names[[i]] == "Anopheles stephensi" |
     SpeciesOfInterest_Names[[i]] == "Culex annulirostris" |
     SpeciesOfInterest_Names[[i]] == "Culex quinquefasciatus") {
    names(sdm_data)[16] <- "TAM"
    names(sdm_data)[17] <- "TASD" }
  
  if(SpeciesOfInterest_Names[[i]] == "Aedes albopictus" |
     SpeciesOfInterest_Names[[i]] == "Culex pipiens" |
     SpeciesOfInterest_Names[[i]] == "Culex tarsalis") {
    names(sdm_data)[16] <- "PhotoASTM"
    names(sdm_data)[17] <- "PhotoASTSD" }
  
  if(SpeciesOfInterest_Names[[i]] == "Anopheles gambiae") {
    names(sdm_data)[16] <- "PrecipASTM"
    names(sdm_data)[17] <- "PrecipASTSD" }

  
  #------------------------------------------------------
  # Acquire training and evaluation sets
  #------------------------------------------------------
  train_data <- sdm_data %>% filter(Species == SpeciesOfInterest_Names[[i]], Data_Split == "Training")
  eval_data <- sdm_data %>% filter(Species == SpeciesOfInterest_Names[[i]], Data_Split == "Evaluation")

  train_xgb_dmatrix <- xgb.DMatrix(data = as.matrix(train_data %>% dplyr::select(c(7:17))),
                                   label = as.matrix(train_data %>% dplyr::select(3)))
  eval_xgb_dmatrix <- xgb.DMatrix(data = as.matrix(eval_data %>% dplyr::select(c(7:17))),
                                  label = as.matrix(eval_data %>% dplyr::select(3)))

  
  #------------------------------------------------------
  # Define a cross-validation function to be used as the 'engine' for Bayesian optimization
  # Runs CV, generates a list of test loglosses averaged over all folds, and picks the logloss from the best iteration
  #------------------------------------------------------
  xgb_opt_fun <- function(eta, gamma, max_depth, subsample, colsample_bytree,
                          min_child_weight, xgb_dmatrix, rounds_max = 100, nfold = 5, nthread = 2) {
    set.seed(seedNum)
    
    xgb_cv <- xgb.cv(
      params = list(booster = "gbtree",
                    eta = eta,
                    gamma = gamma,
                    max_depth = max_depth,
                    subsample = subsample,
                    colsample_bytree = colsample_bytree,
                    min_child_weight = min_child_weight,
                    scale_pos_weight = 2,
                    objective = "binary:logistic",
                    eval_metric = "logloss"),
      data = xgb_dmatrix,
      nrounds = rounds_max,
      nthread = nthread,
      nfold = nfold,
      early_stopping_rounds = 10,
      print_every_n = 20,
      verbose = TRUE)
    
    list(Score = xgb_cv$evaluation_log %>%
           filter(iter == xgb_cv$best_iteration) %>%
           pull(test_logloss_mean) %>%
           multiply_by(-1),  # Make the score negative, since Bayesian optimization is a maximization function
         Pred = xgb_cv$best_iteration) %>%
      return
  }
  
  
  #------------------------------------------------------
  # Acquire optimal XGBoost hyperparameters with Bayesian optimization
  #------------------------------------------------------
  set.seed(seedNum)
  
  bayes_opt_params <- BayesianOptimization(
    function(eta, gamma, max_depth, subsample,
             colsample_bytree, min_child_weight) {
      xgb_opt_fun(eta, gamma, max_depth, subsample,
                  colsample_bytree, min_child_weight,
                  train_xgb_dmatrix, rounds_max = max_xgb_rounds,
                  nfold = 5, nthread = usable.cores) },
    bounds = param_bounds,
    init_points = bayes_opt_n_init,
    n_iter = bayes_opt_n_iter)
  
  bayes_opt_nrounds <-  bayes_opt_params$History %>%
    filter(Value == bayes_opt_params$Best_Value) %>%
    pull(Round) %>%
    magrittr::extract(unlist(bayes_opt_params$Pred), .)
  
  
  #------------------------------------------------------
  # Train the full XGBoost model with the suite of optimized hyperparameters
  #------------------------------------------------------
  set.seed(seedNum)
  
  xgb.fit <- xgb.train(
    params = c(as.list(bayes_opt_params$Best_Par),
               booster = "gbtree",
               scale_pos_weight = 2,
               objective = "binary:logistic",
               eval_metric = "logloss"),
    data = train_xgb_dmatrix,
    nrounds = bayes_opt_nrounds,
    verbose = TRUE)
  
  
  #------------------------------------------------------
  # Save XGBoost model object, CV, and best parameters
  #------------------------------------------------------
  save_name <- paste0("XGBoost_Outputs/",SpeciesOfInterest_Underscore[[i]],"_XGBoost.model")
  xgb.save(xgb.fit, save_name)

  save_name <- paste0("XGBoost_Outputs/",SpeciesOfInterest_Underscore[[i]],"_Bayes_Optimization.RDS")
  saveRDS(bayes_opt_params, save_name)
  
  
  #------------------------------------------------------
  # Predict values for training and evaluation sets
  #------------------------------------------------------
  train_predictions <- predict(xgb.fit, train_xgb_dmatrix)
  eval_predictions <- predict(xgb.fit, eval_xgb_dmatrix)
  
  train_data %<>% mutate(Predicted_Value = train_predictions)
  eval_data %<>% mutate(Predicted_Value = eval_predictions)
  
  
  #------------------------------------------------------
  # Merge data for train and eval sets and save
  #------------------------------------------------------
  sdm_data_predictions <- rbind(train_data, eval_data)
  
  save_name <- paste0("XGBoost_Outputs/",SpeciesOfInterest_Underscore[[i]],"_SDM_Data_Predictions.RDS")
  saveRDS(sdm_data_predictions, save_name)
  
  
  #------------------------------------------------------
  # Plot preliminary ROC curve for training and evaluation sets to assess model fit
  #------------------------------------------------------
  save_name <- paste0("XGBoost_Outputs/",SpeciesOfInterest_Underscore[[i]],"_ROC_Preliminary.pdf")
  
  y_train <- train_data$Occ1_or_Bg0
  y_eval <- eval_data$Occ1_or_Bg0
  pred_train <- train_data$Predicted_Value
  pred_eval <- eval_data$Predicted_Value
  
  pdf(save_name)
  plot.roc(y_train, pred_train,
           print.auc = TRUE,
           print.auc.y = 0.4,
           identity.col = "grey50",
           col = "maroon",
           legacy.axes = TRUE,
           grid = TRUE)
  
  plot.roc(y_eval, pred_eval,
           add = TRUE,
           print.auc = TRUE,
           print.auc.y = 0.3,
           identity.col = "grey50",
           col = "dodgerblue4",
           legacy.axes = TRUE,
           grid = TRUE)
  dev.off()
  
}


