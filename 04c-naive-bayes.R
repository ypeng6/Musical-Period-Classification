##### Naive Bayes  #####

## Build Navie Bayes Model Entry Point
#' @describe This function is the entry point of the model building
#' @param train_data current fold train set
#' @param holdo_data current fold holdo set
#' @param k current k-fold index
#' @return navie bayes model
build_nb <- function(train, holdo, k, seed) {
  # df including all features, as well as period as the last column
  selected_df_train <- train
  selected_df_holdo <- holdo
  selected_nb_model <- analyse_full_nb(selected_df_train, selected_df_holdo, k, seed)
  
  return (selected_nb_model)
}

## Build Navie Bayes Model Entry Point
#' @describe This function is the build the Naive Bayes model and its PI,
#' saving the PI into RData file and calculate the outpred class result
#' @param train_data current fold train set
#' @param holdo_data current fold holdo set
#' @param k current k-fold index
#' @return a vector with navie bayes model, auc object and outpred class result
analyse_full_nb <- function(train, holdo, k, seed) {
  set.seed(seed)
  # Get training data for each period/class
  c1 = subset(train, period == "ancient_medieval_renaissance")
  c2 = subset(train, period == "baroque")
  c3 = subset(train, period == "classical")
  c4 = subset(train, period == "romantic")
  c5 = subset(train, period == "twentiethcentury_modern")
  
  # Get product of pdfs over all features for each class k
  c1pdfpmfprod <- prod_features(holdout = holdo, ck = c1) # or holdo[1:x,] if want to examine first x songs
  c2pdfpmfprod <- prod_features(holdout = holdo, ck = c2)
  c3pdfpmfprod <- prod_features(holdout = holdo, ck = c3)
  c4pdfpmfprod <- prod_features(holdout = holdo, ck = c4)
  c5pdfpmfprod <- prod_features(holdout = holdo, ck = c5)
  
  # Get posterior probabilities for class 1 to 5
  outpredPostc1 <- round(c1pdfpmfprod/(c1pdfpmfprod+c2pdfpmfprod+c3pdfpmfprod+c4pdfpmfprod+c5pdfpmfprod), 3)
  outpredPostc2 <-round(c2pdfpmfprod/(c1pdfpmfprod+c2pdfpmfprod+c3pdfpmfprod+c4pdfpmfprod+c5pdfpmfprod), 3)
  outpredPostc3 <-round(c3pdfpmfprod/(c1pdfpmfprod+c2pdfpmfprod+c3pdfpmfprod+c4pdfpmfprod+c5pdfpmfprod), 3)
  outpredPostc4 <-round(c4pdfpmfprod/(c1pdfpmfprod+c2pdfpmfprod+c3pdfpmfprod+c4pdfpmfprod+c5pdfpmfprod), 3)
  outpredPostc5 <-round(c5pdfpmfprod/(c1pdfpmfprod+c2pdfpmfprod+c3pdfpmfprod+c4pdfpmfprod+c5pdfpmfprod), 3)
  
  # Within each class see the summary (mean/median/quantiles) of predicted probabilities 
  summary(outpredPostc1[holdo$period == "ancient_medieval_renaissance"])
  summary(outpredPostc2[holdo$period == "baroque"])
  summary(outpredPostc3[holdo$period == "classical"])
  summary(outpredPostc4[holdo$period == "romantic"])
  summary(outpredPostc5[holdo$period == "twentiethcentury_modern"])
  
  # Format the out of sample prediction to be the same as the output of predict() function (ex. predict(mlogitw, type="response", newdata=holdo))
  outpredPost <- cbind(outpredPostc1, outpredPostc2, outpredPostc3, outpredPostc4, outpredPostc5)
  
  # Get outpred class result
  outpred_class <- max.col(outpredPost)
  outpred_class_factor <- factor(outpred_class, levels = c(1, 2, 3, 4, 5),
                                 labels = c("A", "B", "C", "R", "M"))
  
  # Prediction Interval #
  predintNaiveBayes = CategoryPredInterval(outpredPost)

  pred50 = table(holdo$period, predintNaiveBayes$pred50)
  pred80 = table(holdo$period, predintNaiveBayes$pred80)
  nb_tables <- list(pred50, pred80)
  
  # Save the list to a file
  file_name <- paste0("nb_tables_fold", k, ".RData")
  folder_path <- "output/"
  save(nb_tables, file = paste0(folder_path, file_name))
  
  model_auc_object <- calculateAUC(outpredPost, holdo)
  
  return (list(outpred = outpredPost, model_auc_object = model_auc_object, outpred_class_factor=outpred_class_factor))
}