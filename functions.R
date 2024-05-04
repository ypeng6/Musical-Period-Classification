## Function to automate getting out-of-sample density values
# The bandwidth is obtained from bw.nrd0 for input to this function.
# Function could be more flexible to compute bw within, as an option.
#' @describe Gaussian kernel probability density function 
#' @param xstar vector of values for computing density values (e.g., holdout)
#' @param xtrain vector of values for a variable in training set
#' @param bw bandwidth for Gaussian kernel, if 0 apply  bw.nrd0()
#' @param iprint flag for intermediate calculations, default=F
#' @return vector density variables at xstar based on Gaussian kernel
PDF = function(xstar, xtrain, bw=0, iprint=F) { 
  nn = length(xstar); kpdf = rep(0,nn) 
  # modified function to compute bandwidth here
  if(bw<=0) bw = bw.nrd0(xtrain)
  if(iprint) { cat(length(xtrain), bw, "\n") }
  for(i in 1:nn) { kpdf[i] = mean( dnorm(xstar[i]-xtrain, 0, bw) ) }
  kpdf
}

## Function to get product of pdfs and pmfs for class k over all features in the input holdout data, numeric and categorical
#' @describe Product of PDFs and PMFs for a certain class k
#' @param holdout holdout dataframe 
#' @param ck training dataframe where period == class k
#' @return product of pdfs and pmfs for class k over all features in the input holdout data
# Generic function for all numeric and categorical features
prod_features <- function(holdout, ck) {
  nfeatures = ncol(holdout)-1 # assume last column is period variable
  classk_prob = NULL
  for(i in 1:nfeatures) {
    if(is.numeric(holdout[,i])){ # numeric features calls helper function PDF
      # get density value of holdout based on curve for c_k, for feature i
      ckpdfi = PDF(holdout[,i],ck[,i],bw=0)
      # append a list of ith feature's densities (array with length = nrow(holdout)) to classk_prob
      classk_prob = append(classk_prob, list(ckpdfi))
    } else if (is.factor(holdout[,i])) { # categorical features computes pmfs
      # get pmf of class ck for feature i
      ckpmfi = c(table(ck[,i]))/nrow(ck)
      cipmfi_holdo <- ckpmfi[holdout[,i]]
      # append a list of ith feature's densities (array with length = nrow(holdout)) to classk_prob
      classk_prob = append(classk_prob, list(cipmfi_holdo))
    } else{
      print("Feature is not numeric or factor, check input holdout set")
    }
  }
  # get product of pdf over all features for class k by first turning the nested list into matrix of size nfeatures*nrow(holdout)
  ckpdfpmfprod <- apply(matrix(unlist(classk_prob), nfeatures, nrow(holdout), byrow = TRUE), 2, prod)
  return(ckpdfpmfprod)
}

## Holdout set prediction intervals
#' @description
#' Prediction intervals for a categorical response
#' @param ProbMatrix of dimension nxJ, J = # categories,
#' each row is a probability mass function
#' @param labels vector of length J, with short names for categories
#' @details
#' A more general function can be written so the levels of prediction intervals
#' can be other than 0.50 and 0.80.
#' @return list with two string vectors of length n:
#' pred50 has 50% prediction intervals
#' pred80 has 80% prediction intervals
CategoryPredInterval <- function(ProbMatrix, labels=c("A","B","C","R","M")) {
  ncases <- nrow(ProbMatrix)
  pred50 <- rep(NA, ncases)
  pred80 <- rep(NA, ncases)
  for (i in 1:ncases) {
    p <- ProbMatrix[i, ]
    ip <- order(p, decreasing = TRUE)
    pOrdered <- p[ip]
    labelsOrdered <- labels[ip]
    G <- cumsum(pOrdered)
    k1 <- min(which(G >= 0.5))
    k2 <- min(which(G >= 0.8))
    pred1 <- labelsOrdered[1:k1]
    pred2 <- labelsOrdered[1:k2]
    pred50[i] <- paste(pred1, collapse = "")
    pred80[i] <- paste(pred2, collapse = "")
  }
  list(pred50 = pred50, pred80 = pred80)
}

##  Calculate AUC
#' @description Calculate the area under the ROC curve (AUC) for a multiclass classification model.
#' @param outpredMlogit a matrix of predicted probabilities for each class label for each observation in the holdout set.
#' @param holdo a data frame containing the true class labels for each observation in the holdout set.
#' @return the average AUC across all class labels.
calculateAUC <- function(outpred, holdo) {
  library(multiROC)
  # Convert for input to function multi_roc
  A = (holdo$period=="ancient_medieval_renaissance")
  B = (holdo$period=="baroque")
  C = (holdo$period=="classical")
  R = (holdo$period=="romantic")
  M = (holdo$period=="twentiethcentury_modern")
  
  ### Without weights
  inputROC = data.frame(as.numeric(A), as.numeric(B), as.numeric(C),
                        as.numeric(R),as.numeric(M),
                        outpred)
  names(inputROC) = c("A_true", "B_true", "C_true","R_true","M_true",
                      "A_pred_new", "B_pred_new", "C_pred_new", "R_pred_new", "M_pred_new")
  
  rocObj = multi_roc(inputROC)
  names(rocObj)
  # Calculate average AUC across all class labels
  return(rocObj$AUC)
}

## Calculate Weight
#' @description Calculate the weight for logit and rf to use
#' @param outpredMlogit a matrix of predicted probabilities for each class label for each observation in the holdout set.
#' @param train a data frame containing the training value
#' @return the weight
calculate_weight <- function(train) {
  ntrain = nrow(train)
  nvec = c(table(train$period))
  wt = rep(1,ntrain)
  wt[train$period=="ancient_medieval_renaissance"] = (ntrain/5)/nvec[1]
  wt[train$period=="baroque"] = (ntrain/5)/nvec[2]
  wt[train$period=="classical"] = (ntrain/5)/nvec[3]
  wt[train$period=="romantic"] = (ntrain/5)/nvec[4]
  wt[train$period=="twentiethcentury_modern"] = (ntrain/5)/nvec[5]
  return (wt)
} 