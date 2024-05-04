# Musical-Period-Classification

This project involves classification of classical music historical periods using data from the website spotifyclassical.com. The code is written in R and includes scripts for data wrangling, data transformation, feature selection, and modeling using various methods, including random forest, multinomial logistic regression, naive Bayes, and a voting method. The project also includes cross-validation and method comparison steps.

#### Requirements

- R version 3.6 or later
- R packages: dplyr, ggplot2, ggcorrplot, gridExtra, magrittr, tibble, randomForest, VGAM, multiROC


#### Usage
**Note that the project may take up to 15 minutes to run, with feature selection taking up to 10 minutes of that time.**
- Open RStudio or your preferred R development environment.
- Set your working directory to the project directory.
- Run the following code in the console to install the required packages:
`install.packages(c("dplyr", "ggplot2", "ggcorrplot", "gridExtra", "magrittr", "tibble", "randomForest", "VGAM", "multiROC"))`
- Run the main.R script to run the project: `source("main.R")`
- The transformed data will be saved in the 'data' folder, and all output files will be saved in the 'output' folder.



#### Files

- 01-data-wrangling.R: Script for data wrangling.
- 02-data-transform.R: Script for data transformation.
- 03-feature-selection.R: Script for feature selection.
- 04a-random-forest.R: Script for random forest modeling.
- 04b-multinomial-logit.R: Script for multinomial logistic regression modeling.
- 04c-naive-bayes.R: Script for naive Bayes modeling.
- 04d-vote.R: Script for voting method modeling.
- 05-cross-validation.R: Script for cross-validation.
- 06-method-comparision.R: Script for method comparison.
- functions.R: Script containing helper functions used in the project.
- README.txt: This file.
- data/spotifyclassical_composerbirthperiod.csv: Raw data file.
- data/transformed_data.RData: Transformed data file (generated by 02-data-transform.R).
- output/select_features_fold1.RData: Selected features output file (generated by 03-feature-selection.R).
- output/cv_results.RData: Cross-validation results output file (generated by 05-cross-validation.R).
- output/method_comparison.RData: Method comparison output file (generated by 06-method-comparision.R).

#### Dependencies
This project requires the following R libraries:
- dplyr: used for data manipulation and transformation
- ggplot2: used for data visualization
- ggcorrplot: used for creating correlation plots
- gridExtra: used for combining plots
- magrittr: used for the pipe operator %>%
- tibble: used for creating data frames
- randomForest: used for creating random forest models
- VGAM: used for fitting generalized additive models
- multiROC: used for creating ROC curves

#### Credits
This project was developed as part of STAT 447 at the University of British Columbia.

- Contributors: Yufei Cai, Meiying Ding, Yulong Peng
