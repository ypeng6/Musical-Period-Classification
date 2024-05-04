#### Method Comparison ####

## Compare Method
#' @describe print AUC matrix and load the PI into global var
#' @param cv_result the result from the cross validation
method_compare <- function(cv_result) {
  # Macro AUC Matrix
  print(cv_result[[1]])
  # Class AUC Matrix
  classAUCMatrix = cv_result[[4]]
  
  # Macro AUC Matrix
  print(cv_result[[5]])
  
  # Random Forest PI
  load("output/rf_tables_fold1.RData")
  rf_tables_fold1 <- rf_tables
  load("output/rf_tables_fold2.RData")
  rf_tables_fold2 <- rf_tables
  load("output/rf_tables_fold3.RData")
  rf_tables_fold3 <- rf_tables
  
  # Multi Logit PI
  load("output/logit_tables_fold1.RData")
  logit_tables_fold1 <- logit_tables
  load("output/logit_tables_fold2.RData")
  logit_tables_fold2 <- logit_tables
  load("output/logit_tables_fold3.RData")
  logit_tables_fold3 <- logit_tables
  
  # Navie Bayes PI
  load("output/nb_tables_fold1.RData")
  nb_tables_fold1 <- nb_tables
  load("output/nb_tables_fold2.RData")
  nb_tables_fold2 <- nb_tables
  load("output/nb_tables_fold3.RData")
  nb_tables_fold3 <- nb_tables
}