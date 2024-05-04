###### STAT 447 Project: Classical Music Historical Period Classification ######

### Source Import ###
source("./01-data-wrangling.R")
source("./02-data-transform.R")
source("./03-feature-selection.R")
source("./04a-random-forest.R") 
source("./04b-multinomial-logit.R")
source("./04c-naive-bayes.R")
source("./04d-vote.R")
source("./05-cross-validation.R")
source("./06-method-comparision.R")
source("./functions.R")

### Set Seed ###
seed = 447

### Data Loading ###
dat <- read.csv("data/spotifyclassical_composerbirthperiod.csv")

### Data Wrangling ###
data_wrangle(dat)

### Data Transformation ###
set.seed(seed)
data_transform(dat)
load('data/transformed_data.RData')

### Feature Selection ###
n = nrow(dat)
iperm = sample(n)
crossValidate_feature(Kfold=3, iperm, dat, seed)
load('output/select_features_fold1.RData')

### Method & Cross Validation ###
cv_result = crossValidate(Kfold=3, iperm, dat, nperfMeas=6, seed, selected_features)

### Method Comparison ###
method_compare(cv_result)


