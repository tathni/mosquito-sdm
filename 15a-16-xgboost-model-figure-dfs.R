#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Run the XGBoost machine learning model by species
#######################################################


# if(Sys.getenv('SLURM_JOB_ID') != ""){ # Check if the script is running on Sherlock remote computing cluster
  library(dplyr)
  library(magrittr)
  library(tidyr)
  library(xgboost)
  library(rBayesianOptimization)
  library(ggplot2)
  library(pROC)
  #library(plotROC)
  library(caret)
  library(rsample)
#   usable.cores <- Sys.getenv("SLURM_NTASKS_PER_NODE")
#   seedNum <- 250
#   
#   # Use the command line arguments supplied to set which species we'll be running 
#   args <- commandArgs(TRUE) 
#   species_inds <- as.numeric(args[1]) 
#   
# } else {
#   source("E:/Documents/GitHub/mosquito-sdm/0-config.R")
#   usable.cores <- 2
#   species_inds <- 1:8 
# }

seedNum <- 250
species_inds <- 1:8 
usable.cores <- Sys.getenv("SLURM_NTASKS_PER_NODE")

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
sdm_data <- read.csv("SDM_Data.csv")
# sdm_data <- read.csv("Analysis Dataset/SDM_Data.csv")


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
# create all storage objects
#------------------------------------------------------

bayes_opt_params_all <- c()
sdm_predictions_all <- c()
sdm_auc_all <- c()
sdm_roc_all <- c()
vimp_all <- c()
uni_pdp_all <- c()
bivar_pdp_all <- c()

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

  species_data <- sdm_data %>% filter(Species == SpeciesOfInterest_Names[[i]])
  
  #set up storage objects
  bayes_opt_params_species <- c()
  predictions_species <- c()
  auc_species <- c()
  roc_species <- c()
  vimp_species <- c()
  uni_pdp_species <- c()
  bivar_pdp_species <-c()
  
  for(j in 1:20){
  
  set.seed(9*j) # set new seed number to get different data splits
  
  #------------------------------------------------------
  # Acquire training and evaluation sets
  #------------------------------------------------------
  
  #train_data <- sdm_data %>% filter(Species == SpeciesOfInterest_Names[[i]], Data_Split == "Training")
  #eval_data <- sdm_data %>% filter(Species == SpeciesOfInterest_Names[[i]], Data_Split == "Evaluation")
  rows <- sample(nrow(species_data))
  analysis_data <- species_data[rows, ]
  
  # save indices
  
  ##create test & train split
  train_index <- createDataPartition(analysis_data[, 3], p = .8, list = FALSE)
  train_data <- species_data[train_index,]
  eval_data <- species_data[-train_index,]
    
  train_xgb_dmatrix <- xgb.DMatrix(data = as.matrix(train_data %>% dplyr::select(c(7:17))),
                                   label = as.matrix(train_data %>% dplyr::select(3)))
  eval_xgb_dmatrix <- xgb.DMatrix(data = as.matrix(eval_data %>% dplyr::select(c(7:17))),
                                  label = as.matrix(eval_data %>% dplyr::select(3)))

  
  #------------------------------------------------------
  # Define a cross-validation function to be used as the 'engine' for Bayesian optimization
  # Runs CV, generates a list of test loglosses averaged over all folds, and picks the logloss from the best iteration
  #------------------------------------------------------
  xgb_opt_fun <- function(eta, gamma, max_depth, subsample, colsample_bytree,
                          min_child_weight, xgb_dmatrix, rounds_max = 100, nfold = 5) {
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
                  nfold = 5) },
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
  bayes_opt_params_df <- as.data.frame(bayes_opt_params$Best_Par) 
  bayes_opt_params_df$iteration <- j
  bayes_opt_params_species <- rbind(bayes_opt_params_species, bayes_opt_params_df)
  
  #------------------------------------------------------
  # Predict values for training and evaluation sets
  #------------------------------------------------------
  train_predictions <- predict(xgb.fit, train_xgb_dmatrix)
  eval_predictions <- predict(xgb.fit, eval_xgb_dmatrix)
  
  train_data %<>% mutate(Predicted_Value = train_predictions); train_data$set <- "train"
  eval_data %<>% mutate(Predicted_Value = eval_predictions); eval_data$set <- "eval"
  
  #------------------------------------------------------
  # Merge data for train and eval sets and save
  #------------------------------------------------------
  predictions_df <- rbind(train_data, eval_data)
  predictions_df$iteration <- j

  predictions_species <- rbind(predictions_species, predictions_df)
  
  #------------------------------------------------------
  # Plot preliminary ROC curve for training and evaluation sets to assess model fit
  #------------------------------------------------------
  y_train <- train_data$Occ1_or_Bg0
  y_eval <- eval_data$Occ1_or_Bg0
  pred_train <- train_data$Predicted_Value
  pred_eval <- eval_data$Predicted_Value
  
  #### AUC DATAFRAME
  auc_df <- data.frame(species = SpeciesOfInterest_Underscore[[i]],
                       in_sample_auc = auc(y_train, pred_train),
                       out_sample_auc = auc(y_eval, pred_eval),
                       iteration = j)
  
  auc_species <- rbind(auc_species, auc_df)
  
  #### ROC DATAFRAME
  train_roc <- roc(y_train, pred_train)
  train_roc_df <- data.frame(set = rep("train", length(train_roc$sensitivities)),
                             sensitivities = train_roc$sensitivities,
                             specificities =  train_roc$specificities, 
                             iteration = rep(j, length(train_roc$sensitivities)))
  
  eval_roc <- roc(y_eval, pred_eval)
  eval_roc_df <- data.frame(set = rep("eval", length(eval_roc$sensitivities)),
    sensitivities = eval_roc$sensitivities,
    specificities =  eval_roc$specificities, 
    iteration = rep(j, length(eval_roc$sensitivities)))
  
  roc_df <- rbind(train_roc_df, eval_roc_df)
  # roc for train
  
  roc_species <- rbind(roc_species, roc_df)
  
  #------------------------------------------------------
  # Var importance pdp
  #------------------------------------------------------
  imp_matrix <- xgb.importance(colnames(train_xgb_dmatrix), model = xgb.fit)
  vimp_df <- as.data.frame(imp_matrix)
  vimp_df$iteration <- j
  
  vimp_species <- rbind(vimp_species, vimp_df)
  
  #------------------------------------------------------
  # PDP
  #------------------------------------------------------
  predictor_names <- names(train_data[7:17])
  
  uni_pdp <- c()
  for(k in 1:length(predictor_names)) {
  output <- as.data.frame(pdp::partial(xgb.fit, pred.var = predictor_names[[k]], 
                      prob = TRUE, #type = "classification",
                      chull = TRUE,
                      train = train_data %>% dplyr::select(c(7:17))))
  output$feature <- predictor_names[[k]]
  names(output)[1] <- "value"
  uni_pdp <- rbind(uni_pdp, output)
  }
  
  uni_pdp$iteration <- j
  uni_pdp_species <- rbind(uni_pdp_species, uni_pdp)
  
  #------------------------------------------------------
  # BIVARIATE PDP
  #------------------------------------------------------
  if(SpeciesOfInterest_Names[[i]] == "Aedes aegypti" |
     SpeciesOfInterest_Names[[i]] == "Anopheles stephensi" |
     SpeciesOfInterest_Names[[i]] == "Culex annulirostris" |
     SpeciesOfInterest_Names[[i]] == "Culex quinquefasciatus") {
    pdp_bivariate <- as.data.frame(pdp::partial(xgb.fit$handle, train = as.matrix(train_data %>% dplyr::select(c(7:17))),
                                  pred.var = c("TAM","TASD"), chull = TRUE))

  }
  
  if(SpeciesOfInterest_Names[[i]] == "Aedes albopictus" |
     SpeciesOfInterest_Names[[i]] == "Culex pipiens" |
     SpeciesOfInterest_Names[[i]] == "Culex tarsalis") {
    pdp_bivariate <- as.data.frame(pdp::partial(xgb.fit$handle, train = as.matrix(train_data %>% dplyr::select(c(7:17))),
                                  pred.var = c("PhotoASTM","PhotoASTSD"), chull = TRUE))

  }
  
  if(SpeciesOfInterest_Names[[i]] == "Anopheles gambiae") {
    pdp_bivariate <- as.data.frame(pdp::partial(xgb.fit$handle, train = as.matrix(train_data %>% dplyr::select(c(7:17))),
                                  pred.var = c("PrecipASTM","PrecipASTSD"), chull = TRUE))
  }
  
  pdp_bivariate$iteration <- j
  bivar_pdp_species <- rbind(bivar_pdp_species, pdp_bivariate)
  
  }
  
  ######### hyper params
  bayes_opt_params_species$species <- SpeciesOfInterest_Underscore[[i]]
  bayes_opt_params_all <- rbind(bayes_opt_params_all, bayes_opt_params_species)
  
  ######## predictions
  predictions_species$species <- SpeciesOfInterest_Underscore[[i]]
  sdm_predictions_all <- rbind(sdm_predictions_all, predictions_species)

  ######## auc
  auc_species$species <- SpeciesOfInterest_Underscore[[i]]
  sdm_auc_all <- rbind(sdm_auc_all, auc_species)

  ######## roc
  roc_species$species <- SpeciesOfInterest_Underscore[[i]]
  sdm_roc_all <- rbind(sdm_roc_all, roc_species)

  ######## var imp
  vimp_species$species <- SpeciesOfInterest_Underscore[[i]]
  vimp_all <- rbind(vimp_all, vimp_species)

  ####### univariate PDP
  uni_pdp_species$species <- SpeciesOfInterest_Underscore[[i]]
  uni_pdp_all <- rbind(uni_pdp_all, uni_pdp_species)

  ####### bivariate PDP
  bivar_pdp_species$species <- SpeciesOfInterest_Underscore[[i]]
  bivar_pdp_all <- rbind(bivar_pdp_all, bivar_pdp_species)

}

saveRDS(bayes_opt_params_all, "bayes_optimization_hyperparameters.rds")
saveRDS(sdm_predictions_all, "SDM_Data_Predictions.RDS")
saveRDS(sdm_auc_all, "AUC.RDS")
saveRDS(sdm_roc_all, "ROC_Preliminary_DF.RDS")
saveRDS(vimp_all, "vimp_dataframe.RDS")
saveRDS(uni_pdp_all, "univariate_pdp_dataframe.RDS")
saveRDS(bivar_pdp_all, "bivariate_pdp_dataframe.RDS")

