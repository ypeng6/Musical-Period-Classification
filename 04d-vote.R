##### Voting #####

##  Voting Classifier
#' @describe This function calculate the exact match classification accuracy of 
#' each method and aggregates the results from all input models and predicts the 
#' final response based on the majority vote.
#' @param acc_matrix matrix to record the accuacy accross different fold
#' @param rf_output random forest outpred class result
#' @param multi_output multinomial logit outpred class result
#' @param nb_output naive bayes outpred class result
#' @param holdo current fold holdo set
#' @param k current k-fold index
#' @param Kfold total number of folds
#' @return accuracy matrix of all methods
vote <- function(acc_matrix, rf_output, multi_output, nb_output, holdo, k, Kfold) {
  # Create a matrix with the classification results from each method
  all_outputs <- matrix(c(rf_output, multi_output, nb_output), nrow=length(rf_output), ncol=3)
  # Take the mode (most common element) of each row
  majority_vote <- apply(all_outputs, 1, function(x) names(which.max(table(x)))) 
  
  holdo$period <- gsub("ancient_medieval_renaissance", "A", holdo$period)
  holdo$period <- gsub("baroque", "B", holdo$period)
  holdo$period <- gsub("classical", "C", holdo$period)
  holdo$period <- gsub("romantic", "R", holdo$period)
  holdo$period <- gsub("twentiethcentury_modern", "M", holdo$period)
  
  holdo_class <- holdo$period
  total <- length(holdo_class)
  
  ### RF ###
  correct_rf <- sum(rf_output == holdo_class)
  accuracy_rf <- correct_rf / total
  
  ### Multi ###
  correct_multi <- sum(multi_output == holdo_class)
  accuracy_multi <- correct_multi / total
  
  ### NB ###
  correct_nb <- sum(nb_output == holdo_class)
  accuracy_nb <- correct_nb / total
  
  ### Vote ###
  correct_votes <- sum(majority_vote == holdo_class)
  accuracy_votes <- correct_votes / total
  
  acc_matrix[k,1] <- accuracy_rf
  acc_matrix[k,2] <- accuracy_multi
  acc_matrix[k,3] <- accuracy_nb
  acc_matrix[k,4] <- accuracy_votes
  
  return(acc_matrix)
}
