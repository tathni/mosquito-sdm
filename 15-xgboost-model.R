#######################################################
# Author: Tejas Athni
# Project: Mosquito SDM Thermal Dependence

# Description: Run the XGBoost machine learning model for each species
#######################################################

source("E:/Documents/GitHub/mosquito-sdm/0-config.R")


#------------------------------------------------------
# Read in data and prepare for model run
#------------------------------------------------------
data <- read.csv("SDM Data.csv")
model_fits <- list()
cv_list <- list()
best_params_list <- list()


#------------------------------------------------------
# Loop through each species
#------------------------------------------------------
for(i in 1:length(SpeciesOfInterest_Names)) {
  print(paste0("Species of interest is ", SpeciesOfInterest_Names[i]))
  
  #------------------------------------------------------
  # List of predictors for the current species
  #------------------------------------------------------
  predictors <- c("ELEV","EVIM","EVISD","FC","HPD","PDQ","PWQ","TAM","TASD")
  
  # ???? make TAM/TASD for year-round species, Precip for gambiae, etc. with if loops
  
  
  
  #------------------------------------------------------
  # Acquire training data
  #------------------------------------------------------
  train_data <- data %>% filter(Species == SpeciesOfInterest_Names[i], DataSplit == "Training") %>% dplyr::select(c(5:14))
  tic <- Sys.time()
  
  
  #------------------------------------------------------
  # New cross-validation function as the engine of Bayesian optimization
  #------------------------------------------------------
  ntrees.max = 200
  set.seed(25)
  xgb_cv_bayes <- function(eta, max.depth, min.child.weight, subsample, colsample_bytree, gamma) {
    cv <- xgb.cv(params = list(booster = "gbtree",
                               eta = eta,
                               max_depth = max.depth,
                               min_child_weight = min.child.weight,
                               subsample = subsample,
                               colsample_bytree = colsample_bytree,
                               gamma = gamma,
                               objective = "binary:logistic",
                               eval_metric = "logloss",
                               seed = 25),
                 data = as.matrix(train_data %>% dplyr::select(-Occ1_or_Bg0)),
                 label = train_data$Occ1_or_Bg0,
                 nrounds = ntrees.max,
                 nfold = 5,
                 early_stopping_rounds = 10,
                 verbose = T)
    list(Score = -unlist(cv$evaluation_log[cv$best_iteration, "test_logloss_mean"]), # Ensure score is negative, since optimization maximizes
         Pred = cv$pred,
         cb.print.evaluation(period = 1))
  }
  
  
  
  ## ??? new bayesian opt function from Marissa, integrate the below:
  
  # xgb_opt_fun <- function(eta, gamma, max_depth, subsample, colsample_bytree, 
  #                         min_child_weight, xgb_mat, cv_ind = NULL, 
  #                         rounds_max = 100, nfold = 4, nthread = 2,
  #                         progress_file = ""){
  #   set.seed(10001)
  #   mod_xgb_cv <- xgb.cv(
  #     params = list(booster = "gbtree",
  #                   eta = eta,
  #                   gamma = gamma,
  #                   max_depth = max_depth,
  #                   subsample = subsample,
  #                   colsample_bytree = colsample_bytree,
  #                   min_child_weight = min_child_weight, 
  #                   objective = "reg:squarederror",
  #                   eval_metric = "rmse"),
  #     data = xgb_mat,
  #     nrounds = rounds_max,
  #     nthread = nthread,
  #     nfold = nfold,
  #     folds = cv_ind,
  #     early_stopping_rounds = 10,
  #     print_every_n = 20,
  #     verbose = TRUE)
  #   
  #   if(progress_file != ""){
  #     write(paste0("nrounds = ", mod_xgb_cv$best_iteration, 
  #                  ", eta = ", eta, 
  #                  ", gamma = ", gamma, 
  #                  ", max_depth = ", max_depth, 
  #                  ", colsample_bytree = ", colsample_bytree, 
  #                  ", subsample = ", subsample, 
  #                  ", min_child_weight = ", min_child_weight, 
  #                  ", RMSE = ", mod_xgb_cv$evaluation_log %>% 
  #                    filter(iter == mod_xgb_cv$best_iteration) %>% 
  #                    pull(test_rmse_mean),
  #                  "\n"),
  #           file = progress_file,
  #           append = T)}
  #   list(Score = mod_xgb_cv$evaluation_log %>% 
  #          filter(iter == mod_xgb_cv$best_iteration) %>% 
  #          pull(test_rmse_mean) %>% 
  #          multiply_by(-1), 
  #        Pred = mod_xgb_cv$best_iteration) %>% 
  #     return
  # }
  # bayes_opt_params <- BayesianOptimization(
  #   function(eta, gamma, max_depth, subsample,
  #            colsample_bytree, min_child_weight){
  #     xgb_opt_fun(eta, gamma, max_depth, subsample, 
  #                 colsample_bytree, min_child_weight, 
  #                 xgb_train_mat, cv_ind = cv_folds, 
  #                 rounds_max = max_xgb_rounds, nfold = length(cv_folds), 
  #                 nthread = usable.cores, 
  #                 progress_file = txt_progress_file)},
  #   bounds = param_bounds,
  #   init_points = bayes_opt_n_init, 
  #   n_iter = bayes_opt_n_iter)
  # mod_gb_final <- xgb.train(
  #   params = c(as.list(bayes_opt_params$Best_Par),
  #              booster = "gbtree",
  #              objective = "reg:squarederror",
  #              eval_metric = "rmse"),
  #   data = xgb_train_mat,
  #   nrounds = bayes_opt_params$History %>%
  #     filter(Value == bayes_opt_params$Best_Value) %>%
  #     pull(Round) %>%
  #     magrittr::extract(unlist(bayes_opt_params$Pred), .),
  #   verbose = 1)
  
  
  
  
  #------------------------------------------------------
  # Acquire optimal parameters with Bayesian optimization (maximization function) via the R package "rBayesianOptimization"
  #------------------------------------------------------
  set.seed(25)
  best_params <- BayesianOptimization(xgb_cv_bayes,
                                         bounds = list(eta = c(0.0001, 0.1),
                                                       max.depth = c(2L, 10L),
                                                       min.child.weight = c(2L, 5L),
                                                       subsample = c(0.5, 1),
                                                       colsample_bytree = c(0.5, 1),
                                                       gamma = c(0, 10)),
                                         init_grid_dt = NULL,
                                         init_points = 10,
                                         n_iter = 40,
                                         acq = "ucb",
                                         kappa = 3,
                                         eps = 1.5,
                                         verbose = T)
  
  
  
  #------------------------------------------------------
  # Using the tuned hyperparameters, run a second cross-validation to acquire nrounds
  #------------------------------------------------------
  set.seed(25)
  xgb_cv <- xgb.cv(params = best_params,
                     data = as.matrix(train_data %>% dplyr::select(-Occ1_or_Bg0)),
                     label = train_data$Occ1_or_Bg0,
                     nrounds = ntrees.max,
                     nfold = 5,
                     early_stopping_rounds = 10,
                     verbose = T)
  
  best_params$nrounds <- xgb_cv$best_ntreelimit
  
  
  
  #------------------------------------------------------
  # Check evaluation log, to see that testing and training errors are declining
  # Ensure that optimized hyperparameters are within the prespecified bounds
  #------------------------------------------------------
  xgb_cv$evaluation_log
  
  eta_check <- xgb_cv$params$Best_Par[1] %>% round(4)
  paste0("Learning rate eta optimized: ",eta_check,"; Prespecified bounds: 0.0001 and 0.1")
  
  max_depth_check <- xgb_cv$params$Best_Par[2] %>% round(4)
  paste0("Max depth optimized: ",max_depth_check,"; Prespecified bounds: 2 to 10") 
  
  min_child_check <- xgb_cv$params$Best_Par[3] %>% round(4)
  paste0("Min child weight optimized: ",min_child_check,"; Prespecified bounds: 2 to 5") 
  
  subsample_check <- xgb_cv$params$Best_Par[4] %>% round(4)
  paste0("Subsample optimized: ",subsample_check*100,"%; Prespecified bounds: 50% to 100%") 
  
  colsample_bytree_check <- xgb_cv$params$Best_Par[5] %>% round(4)
  paste0("Column subsampling optimized: ",colsample_bytree_check*100,"%; Prespecified bounds: 50% to 100%") 
  
  gamma_check <- xgb_cv$params$Best_Par[6] %>% round(4)
  paste0("Gamma optimized: ",gamma_check,"; Prespecified bounds: 0 to 10") 
  
  ntrees_check <- best_params$nrounds
  paste0("Number of trees optimized: ",ntrees_check,"; Ideal: > 1") 
  
  
  
  #------------------------------------------------------
  # Run the full xgb model with the suite of optimal parameters
  #------------------------------------------------------
  set.seed(25)
  xgb.fit <- xgboost(data = as.matrix(train_data %>% dplyr::select(-Occ1_or_Bg0)),
                     label = train_data$Occ1_or_Bg0,
                     eta = best_params$Best_Par$eta,
                     max_depth = best_params$Best_Par$max.depth,
                     min_child_weight = best_params$Best_Par$min.child.weight,
                     subsample = best_params$Best_Par$subsample,
                     colsample_bytree = best_params$Best_Par$colsample_bytree,
                     gamma = best_params$Best_Par$gamma,
                     nrounds = best_params$nrounds,
                     objective = "binary:logistic",
                     eval_metric = "logloss")
  
  model_fits[[i]] <- xgb.fit
  cv_list[[i]] <- xgb_cv
  best_params_list[[i]] <- best_params
  
  
  #------------------------------------------------------
  # Save XGBoost model objects, CV, and params by species
  #------------------------------------------------------
  save_name <- paste0(SpeciesOfInterest_Names[i]," XGBoost.model")
  xgb.save(xgb.fit, save_name)
  
  save_name <- paste0(SpeciesOfInterest_Names[i]," CV Object.RDS")
  saveRDS(xgb_cv, save_name)
  
  save_name <- paste0(SpeciesOfInterest_Names[i]," Best Params.RDS")
  saveRDS(best_params, save_name)
  
}
 

#------------------------------------------------------
# Save the list of XGBoost model objects, CV, and params
#------------------------------------------------------

# ??? if i get this to work then delete the species-specific save

