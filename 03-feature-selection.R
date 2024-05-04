###### Feature Selection ######

## Analyse Current Model
#' @describe This function calculated the current rebuild method Macro AUC score for further comparision
#' @param RF The rebuilt random forest model
#' @param holdo_data current fold holdo set
#' @return current model Macro AUC score
analyse_curr_model <- function(RF, holdo_data) {
  outpredRF <- predict(RF, type="prob", newdata=holdo_data)
  curr_auc = calculateAUC(outpredRF, holdo_data)[[1]][[6]]
  
  return (curr_auc)
}

## Rebuild
#' @describe This function is to rebuild the model without the bottom features
#' @param train_data current fold train set
#' @param holdo_data current fold holdo set
#' @param wt weight to account for the class imbalanced
#' @return a vector that with the current rebuild model, current model auc score, and new feature ranking
rebuild <- function(train_data, holdo_data, wt) {
  # Build random forest model with updated data
  RF_model <- randomForest(period ~ ., data = train_data, importance = TRUE, proximity = TRUE, weight = wt)
  imp_scores <- round(importance(RF_model), 2)
  top_feats <- imp_scores[order(-imp_scores[,"MeanDecreaseGini"]), , drop = FALSE]
  top_feats_names <- rownames(top_feats)
  
  # Calculate AUC of new model
  curr_auc <- analyse_curr_model(RF_model, holdo_data)
  
  print(".................CURRENT FEATURES.................")
  print(top_feats_names)
  print(curr_auc)
  
  return(list(model = RF_model, auc = curr_auc, selected_feats = top_feats_names))
}

## Feature Selection
#' @describe This function applied backward elimintation under the greedy algorithm
#' @param train_data current fold train set
#' @param holdo_data current fold holdo set
#' @param wt weight to account for the class imbalanced
find_max_auc <- function(train_data, holdo_data, wt, k) {
  max_auc <- 0
  selected_features <- c()
  
  # Initial full model
  prev_model <- randomForest(period ~ ., data = train_data, importance = TRUE, proximity = TRUE, weights = wt)
  
  # Initial AUC calculation
  curr_auc <- analyse_curr_model(prev_model, holdo_data)
  prev_auc <- curr_auc
  max_auc <- curr_auc
  curr_features <- colnames(train_data)
  max_auc_features <- colnames(train_data)
  
  while (length(curr_features) > 1) {
    # Rebuild model without least important feature
    model_result <- rebuild(train_data, holdo_data, wt)
    curr_model <- model_result$model
    curr_auc <- model_result$auc
    model_features<- model_result$selected_feats
    curr_features <- model_features
    
    # Update selected features and max AUC if current AUC is better
    if ((max_auc - curr_auc) < 0.02) {
      if (curr_auc >= max_auc) {
        max_auc = curr_auc
      }
      print(".....AUC Better.....")
      max_auc_features <- curr_features
    } else {
      print(".....AUC Worse.....")
      curr_auc = prev_auc
      break # Stop if current AUC is worse
    }
    prev_auc = curr_auc
    prev_model = curr_model
    # Get least important feature
    bottom_feats_name <- tail(curr_features, 1)
    print(".........................ATTEMPT REMOVING BOTTOM FEATURE........................")
    print(bottom_feats_name)
    train_data <- train_data[, !colnames(train_data) %in% bottom_feats_name]
    holdo_data <- holdo_data[, !colnames(holdo_data) %in% bottom_feats_name]
  }
  
  # Save the list to a file
  selected_features=max_auc_features
  file_name <- paste0("select_features_fold", k, ".RData")
  folder_path <- "output/"
  save(selected_features, file = paste0(folder_path, file_name))
  
  # Plot the importance score ranking
  varImpPlot(prev_model)
}

## Build Random Forest Model Entry Point
#' @describe This function is the entry point of the feature selection and model building
#' @param train_data current fold train set
#' @param holdo_data current fold holdo set
#' @param wt weight to account for the class imbalanced
#' @param k current k-fold index
#' @return a vector that with the model that built with final selected features, the model auc object, and the outpred class result
select_features <- function(train_data, holdo_data, wt, k) {
  library(randomForest)
  
  # Assign Periods
  periods <- levels(train_data$period)
  train_data$period <- factor(train_data$period, levels = periods)
  if (!is.null(holdo_data)) {
    holdo_data$period <- factor(holdo_data$period, levels = periods)
  }
  
  # Find best set of features based on training and holdout datasets
  result <- find_max_auc(train_data, holdo_data, wt, k)
}