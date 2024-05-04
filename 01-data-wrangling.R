##### Data Wrangling #####

data_wrangle <- function(dat) {
  library(dplyr)
  library(ggplot2)
  library(ggcorrplot)
  library(gridExtra)
  library(magrittr)  # for pipe %>%
  library(tibble)  
  
  ## EDA and Plots ##
  # Factorize categorical features
  dat$period <- as.factor(dat$period)
  dat$time_signature <- as.factor(dat$time_signature)
  dat$mode <- as.factor(dat$mode)
  dat$key <- as.factor(dat$key)
  
  # Select out features to be used for classification
  dat = dat[,c(6:19,23)]
  names(dat)
  
  # Balanced dataset?
  print(table(dat$period))
  # Period and Key
  print(table(dat$period,dat$key))
  # Period and 
  print(table(dat$period,dat$mode))
  # Period and Signature
  print(table(dat$period,dat$time_signature))
  
  # Summary Statistics and plots for initial data analysis
  summary(dat)
  varn = names(dat)
  
  # Plot histograms of original distribution of features
  for (j in c(1:6,8,9,11,12,14)) 
  { print(ggplot(dat, aes(x = dat[,j])) +
            # geom_violin(trim = FALSE) +
            theme(text = element_text(size=15)) +
            geom_histogram(color = "darkblue",fill = "lightblue") +
            scale_fill_brewer() +
            ggtitle(paste(varn[j],"Original Distribution"))+
            theme(plot.title = element_text(hjust = 0.5)))
  }
}