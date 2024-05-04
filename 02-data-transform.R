##### Transformations #####

data_transform <- function(dat) {
  dato = dat
  varn = names(dat)
  # Factorize categorical features
  dat$period <- as.factor(dat$period)
  dat$time_signature <- as.factor(dat$time_signature)
  dat$mode <- as.factor(dat$mode)
  dat$key <- as.factor(dat$key)
  
  # Select out features to be used for classification
  dat = dat[,c(6:19,23)]
  names(dat)
  # Transform Acousticness, Duration_ms, energy, liveness, speechiness, valence
  for (j in c(1:6,8,9,11,12,14)) 
  { if(j == 4|j == 5|j == 8){
    print(ggplot(dat, aes(x = log(dat[,j]))) +
            theme(text = element_text(size=15)) +
            geom_histogram(color = "darkblue",fill = "lightblue") +
            scale_fill_brewer() +
            ggtitle(paste("log",varn[j],"Distribution"))+
            theme(plot.title = element_text(hjust = 0.5)))
  }
    else{ 
      if(j == 14){
        print(ggplot(dat, aes(x = log(dat[,j]+0.01))) +
                theme(text = element_text(size=15)) +
                geom_histogram(color = "darkblue",fill = "lightblue") +
                scale_fill_brewer() +
                ggtitle(paste("log",varn[j],"+0.01 Distribution"))+
                theme(plot.title = element_text(hjust = 0.5)))}
      else{
        print(ggplot(dat, aes(x = dat[,j])) +
                theme(text = element_text(size=15)) +
                geom_histogram(color = "darkblue",fill = "lightblue") +
                scale_fill_brewer() +
                ggtitle(paste(varn[j],"Distribution"))+
                theme(plot.title = element_text(hjust = 0.5)))
      }
    }
  }
  
  LogTransform = function(dataset)
  { dataTransform = as_tibble(dataset) %>%
    dplyr::mutate( logDura=log(duration_ms),
                   logEnergy=log(energy),
                   logLiveness=log(liveness),
                   logValence=log(valence+0.01)) %>%
    dplyr::select(logDura,logEnergy,logLiveness,logValence,
                  popularity,danceability,
                  key,loudness,mode,
                  tempo,time_signature,
                  period,Speechiness,Acousticness,Instrumentalness
                  # acousticness,instrumentalness,instrumentalness,
    )
  dataTransform
  }
  
  # Find Bins for speechiness: 0.042<0.063<1
  quantile(dat$speechiness,seq(0,1,0.1))
  summary(dat$speechiness>0.25)
  summary(dat$speechiness>0.2)
  summary(dat$speechiness>0.1)
  summary(dat$speechiness>0.063)
  
  # Find Bins for speechiness: 0.339<0.552<1
  quantile(dat$instrumentalness, seq(0,1,0.1))
  summary(dat$instrumentalness<0.339)
  summary(dat$instrumentalness<0.552)
  
  # transform re-loaded data and plot
  dato = dato %>% mutate(period = as.factor(period),
                         time_signature = as.factor(time_signature),
                         mode = as.factor(mode),
                         key = as.factor(key),
                         Speechiness = as.factor(case_when(speechiness < 0.042 ~ 'non-speech',
                                                           speechiness < 0.063 ~ 'music-and-speech',
                                                           speechiness < 1 ~ 'speech')),
                         Acousticness = as.factor(case_when(acousticness < 0.7 ~ 'low',
                                                            acousticness < 0.9 ~ 'medium',
                                                            acousticness < 1 ~ 'high')),
                         Instrumentalness = as.factor(case_when(instrumentalness < 0.339 ~ 'Vocal',
                                                                instrumentalness < 0.552 ~ 'Vocal-like',
                                                                instrumentalness < 1 ~ 'Instrumental')))
  
  dat = as.data.frame(LogTransform(dato))
  varn = names(dat)
  nfeature = length(varn)
  
  # Side-by-side boxplots
  for(j in 1:(nfeature-3))
  { print(ggplot(dat, aes(x = period, y = dat[,j], fill = period)) +
            # geom_violin(trim = FALSE) +
            theme(text = element_text(size=15)) +
            geom_boxplot(width = 0.07) +
            scale_fill_brewer() +
            ggtitle(paste(varn[j],"Boxplot by Different Periods of Classical Music"))+
            theme(plot.title = element_text(hjust = 0.5)))}
  
  # Correlation Matrix and its plot
  cordat = dat[,c(1:6,8,10)]
  corr = round(cor(cordat),2)
  ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE,
             outline.col = "white",
             ggtheme = ggplot2::theme_gray,
             colors = c("#6D9EC1", "white", "#E46726"))
  
  # Period and Speechiness
  print(table(dat$period,dat$Speechiness))
  
  # Period and Instrumentalness
  print(table(dat$period,dat$Instrumentalness))
  
  # Plot logEnergny against loudness by period
  ggplot(dat,aes(logEnergy,loudness, col = period))+
    geom_point(alpha=0.5)
  
  # Plot logValence against danceability by period
  ggplot(dat,aes(logValence,danceability, col = period))+
    geom_point(alpha=0.5)
  
  save(file="data/transformed_data.RData",dat) 
}
