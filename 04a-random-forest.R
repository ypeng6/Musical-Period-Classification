##### Random Forest Model #####

## Analyse Final Model
#' @describe This function calculated the final RF model PI and save the result into RData file
#' @param RF The final random forest model
#' @param holdo_data current fold holdo set
#' @param k current k-fold index, use for separating the output in each fold
#' @return a vector that with current model auc object and the final outpred class result
analyse_final_model <- function(RF, holdo_data, k) {
  outpredRF <- predict(RF, type="prob", newdata=holdo_data)
  
  # Calculate PI
  predintRF <- CategoryPredInterval(outpredRF)
  pred50 = table(holdo_data$period, predintRF$pred50)
  pred80 = table(holdo_data$period, predintRF$pred80)
  rf_tables <- list(pred50, pred80)
  
  # Get outpred class result
  outpred_class <- max.col(outpredRF)
  outpred_class_factor <- factor(outpred_class, levels = c(1, 2, 3, 4, 5),
                                 labels = c("A", "B", "C", "R", "M"))
  
  # Save the list to a file
  file_name <- paste0("rf_tables_fold", k, ".RData")
  folder_path <- "output/"
  save(rf_tables, file = paste0(folder_path, file_name))
  
  model_auc_object = calculateAUC(outpredRF, holdo_data)
  
  return (list(model_auc_object=model_auc_object, outpred_class_factor=outpred_class_factor))
}

## Build Random Forest Model Entry Point
#' @describe This function is the entry point of the feature selection and model building
#' @param train_data current fold train set
#' @param holdo_data current fold holdo set
#' @param wt weight to account for the class imbalanced
#' @param k current k-fold index
#' @return a vector that with the model that built with final selected features, the model auc object, and the outpred class result
build_rf <- function(train_data, holdo_data, wt, k, seed) {
  library(randomForest)
  
  # Assign Periods
  periods <- levels(train_data$period)
  train_data$period <- factor(train_data$period, levels = periods)
  if (!is.null(holdo_data)) {
    holdo_data$period <- factor(holdo_data$period, levels = periods)
  }
  
  # Find best set of features based on training and holdout datasets
  set.seed(seed)
  RF_model <- randomForest(period ~ ., data = train_data, importance = TRUE, proximity = TRUE, weight = wt)

  analyse_result = analyse_final_model(RF_model, holdo_data, k)
  model_auc_object = analyse_result[[1]]
  model_outpred_class = analyse_result[[2]]
  
  return (list(model = RF_model, model_auc_object = model_auc_object, model_outpred_class=model_outpred_class))
}