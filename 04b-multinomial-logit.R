##### Weighted Multinomial Logit #####

## Build Multinomial Model Entry Point
#' @describe This function is the entry point of the model building
#' @param train_data current fold train set
#' @param holdo_data current fold holdo set
#' @param wt weight to account for the class imbalanced
#' @param k current k-fold index
#' @return logit model
build_multinom <- function(train, holdo, wt, k, seed) {
  library(VGAM)
  ## Fit full and selected models with weighted versions
  selected_weighted_model = analyse_logit(train, holdo, wt, k, seed)
  return (selected_weighted_model)
}

## Analyse Logit
#' @describe This function is the build the multi logit model and its PI,
#' saving the PI into RData file and calculate the outpred class result
#' @param train_data current fold train set
#' @param holdo_data current fold holdo set
#' @param wt weight to account for the class imbalanced
#' @param k current k-fold index
#' @return a vector with logit model, auc object and outpred class result
analyse_logit <- function(train, holdo, wt, k, seed) {
  # Fit multinomial logit model with weights
  set.seed(seed)
  mlogit = vglm(period~., multinomial(), data=train, weights = wt)
  outpred = predict(mlogit, type="response", newdata=holdo)
  predint = CategoryPredInterval(outpred)
  logit_summaries <- summary(mlogit)
  
  outpred_class <- max.col(outpred)
  outpred_class_factor <- factor(outpred_class, levels = c(1, 2, 3, 4, 5),
                                 labels = c("A", "B", "C", "R", "M"))
  
  # Save the list to a file
  pred50 = table(holdo$period, predint$pred50)
  pred80 = table(holdo$period, predint$pred80)
  logit_tables <- list(pred50, pred80)
  
  file_name <- paste0("logit_tables_fold", k, ".RData")
  folder_path <- "output/"
  save(logit_tables, file = paste0(folder_path, file_name))
  
  
  file_name <- paste0("logit_summary_fold", k, ".RData")
  folder_path <- "output/"
  save(logit_summaries, file = paste0(folder_path, file_name))
  
  # Calculate AUC score
  model_auc_object <- calculateAUC(outpred, holdo)
  
  return (list(logit_model = mlogit, model_auc_object = model_auc_object, outpred_class_factor=outpred_class_factor))
}