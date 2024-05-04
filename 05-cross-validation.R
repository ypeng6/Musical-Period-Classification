##### Cross Validation #####

## Cross Validate on Models
#' @param Kfold number of folds for cross-validation
#' @param iperm permutation vector for rows
#' @param datafr assume dataframe has y x1 ... xp, can modify below to be more general
#' @param nperfMeas number of performance measures
crossValidate = function(Kfold, iperm, datafr, nperfMeas, seed, selected_features) {
  set.seed(seed)
  n = nrow(datafr); 
  nhold = round(n/Kfold)
  pred = list() # save predictions in order to be able to compare them for different methods
  accuracy = matrix(0,Kfold,4)
  perfMat = matrix(0,Kfold,nperfMeas)  # for storing performance measures
  auc_objects = list() # for storing auc objects for each model in each fold
  for(k in 1:Kfold) { 
    ilow = (k-1)*nhold+1; ihigh = k*nhold
    if(k==Kfold) { 
      ihigh = n 
    }
    ifold = iperm[ilow:ihigh]
    train = datafr[-ifold,]
    holdo = datafr[ifold,]
    wt = calculate_weight(train)
    
    train_selected = train[, c(selected_features, "period")]
    holdo_selected = holdo[, c(selected_features, "period")]
    
    ### Random Forest ###
    rf_result = build_rf(train_selected, holdo_selected, wt, k, seed)
    rf_model = rf_result[[1]]
    rf_auc_object = rf_result[[2]]
    perfMat[k,1] = rf_auc_object[[1]][[6]] # Macro AUC
    perfMat[k,2] = rf_auc_object[[1]][[7]] # Micro AUC
    rf_outpred_class = rf_result[[3]]
    auc_objects[[paste0("Fold", k, "_RF")]] <- rf_auc_object
    
    
    ### Multinomial Logit ###
    multi_model = build_multinom(train_selected, holdo_selected, wt, k, seed)
    multi_auc_object = multi_model[[2]]
    perfMat[k,3] = multi_auc_object[[1]][[6]] # Macro AUC
    perfMat[k,4] = multi_auc_object[[1]][[7]] # Micro AUC
    multi_outpred_class = multi_model[[3]]
    auc_objects[[paste0("Fold", k, "_Logit")]] <- multi_auc_object
    
    ### Naive Bayes ###
    nb_model = build_nb(train_selected, holdo_selected, k, seed)
    nb_auc_object = nb_model[[2]]
    perfMat[k,5] = nb_auc_object[[1]][[6]] # Macro AUC
    perfMat[k,6] = nb_auc_object[[1]][[7]] # Micro AUC
    nb_outpred_class = nb_model[[3]]
    auc_objects[[paste0("Fold", k, "_NB")]] <- nb_auc_object
    
    ### Vote ###
    accuracy = vote(accuracy, rf_outpred_class, multi_outpred_class, nb_outpred_class, holdo, k, Kfold)
  }
  colnames(perfMat) = c("RF Macro", "RF Micro", "Logit Macro", "Logit Micro", "NB Macro", "NB Micro")
  rownames(perfMat) = paste0("Fold", 1:Kfold)
  colnames(accuracy) = c("RF", "Logit", "NB", "Vote")
  rownames(accuracy) = paste0("Fold", 1:Kfold)
  perfAverage = colMeans(perfMat)
  list(perfMat=perfMat, perfAverage=perfAverage, pred=pred, auc_objects=auc_objects, accuracy=accuracy)
}

## Cross Validate on Feature Selection Process
#' @param Kfold number of folds for cross-validation
#' @param iperm permutation vector for rows
#' @param datafr assume dataframe has y x1 ... xp, can modify below to be more general
crossValidate_feature = function(Kfold, iperm, datafr, seed) {
  set.seed(seed)
  n = nrow(datafr); 
  nhold = round(n/Kfold)
  for(k in 1:Kfold) { 
    ilow = (k-1)*nhold+1; ihigh = k*nhold
    if(k==Kfold) { 
      ihigh = n 
    }
    ifold = iperm[ilow:ihigh]
    train = datafr[-ifold,]
    holdo = datafr[ifold,]
    wt = calculate_weight(train)
    
    ### Select Features ###
    select_features(train, holdo, wt, k)
  }
}