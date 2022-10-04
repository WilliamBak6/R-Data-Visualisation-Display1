  library(tidyverse)
  library(ggplot2)
  library(ggrepel)
  library(ggdark)
  library(ggthemes)
  library(bbplot)
  library(zoo)
  library(ggalt)
  library(wordcloud)
  library(tm)
  
  SNCF = read.csv('Regularities_by_liaisons_Trains_France.csv')
  carte = map_data("france")
  station = read.csv("liste-des-gares.csv")

  
  SNCF = SNCF[,c(1:10,c(22:26))]
  names(SNCF) = stringr::str_replace(names(SNCF), "min", "")
  LesNa = is.na(SNCF)
  LesNa = apply(LesNa, 1, any)
  LesNa = which(LesNa == TRUE)
  SNCF = SNCF[-LesNa,]
  
  
  
