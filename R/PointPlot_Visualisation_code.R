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

  carte = carte %>% select(-subregion)
  colonne = c("Libellé.Gare","Commune","Département","coordonnees_geographiques")
  station = station[, names(station) %in% colonne]
  names(station)[c(1,3)] = c("Arrival.station","region")
  
  station = station %>% separate(coordonnees_geographiques, sep = ",", into = c("lat","long"))
  
  station = station %>% mutate(region = stringr::str_replace(region, "Côte-d'Or", "Cote-Dor")) %>%
    mutate(region = stringr::str_replace(region, "Côte-d'Armor", "Cotes-Darmor")) %>%
    mutate(region = stringr::str_replace(region,"ô", "o")
                           , region = stringr::str_replace(region, "è", "e")
                           , region = stringr::str_replace(region, "é", "e")
                           ) %>% 
    mutate(region = recode(region, "Pyrenées-Orientales" = "Pyrenees-Orientales")) %>%
    mutate(region = recode(region, "Pyrenées-Atlantiques" = "Pyrenees-Atlantiques"))


  station = station %>% mutate(Arrival.station = stringr::str_replace_all(Arrival.station ,c("é","â"), c("e","a"))) %>%
    mutate(Arrival.station = stringr::str_replace_all(Arrival.station ,c("è","ô"), c("e","o"))) %>% 
    mutate(Arrival.station = recode(Arrival.station, "Angers St Laud" = "Angers Saint Laud"
                                       , "Bordeaux-St-Jean" = "Bordeaux Saint Jean"
                                       , "Macon-Loché-TGV" = "Macon Loche"
                                       , "Marne-La-Valee-Chessy" = "Marne La Valee Chessy"
                                       , "Metz-Ville" = "Metz"
                                       , "Nîmes" = "Nimes"
                                       , "Paris-Gare-de-Lyon" = "Paris Lyon"
                                       , "St-Étienne-Chateaucreux" = "St Etienne Chateaucreux"
                                       , "Strasbourg-Ville" = "Strasbourg"
                                       , "Valence-TGV" = "Valence Alixan TGV"
                                       , "Angoulême" = "Angouleme"
                                       , "Lille-Flandres" = "Lille"
           )) %>%
    mutate(Arrival.station = stringr::str_replace_all(Arrival.station, "-", " ")
           , Arrival.station = toupper(Arrival.station)) %>% 
    mutate(Arrival.station = stringr::str_replace(Arrival.station, "MONTPELLIER ST ROCH", "MONTPELLIER"))
  
  carte1 = carte %>% select(-c(long,lat,order))
  
  Carte1 = inner_join(carte1, station, by = "region")
  
  SNCF = SNCF %>%  mutate(Arrival.station = recode(Arrival.station, "ANGERS SAINT LAUD" = "ANGERS ST LAUD"
                                                   , "BESANCON FRANCHE COMTE TGV" = "BESANÇON FRANCHE COMTE TGV"
                                                   , "BORDEAUX ST JEAN" = "BORDEAUX SAINT JEAN"
                                                   , "MARNE LA VALLEE" = "MARNE LA VALLEE CHESSY"
                                                   , "NANCY" = "NANCY VILLE"))

  SNCF2 = inner_join(SNCF, station, by = "Arrival.station")
  SNCF2 = unique.data.frame(SNCF2)
  SNCF2 = SNCF2 %>% mutate(region = recode(region, "Côte-d'Or" = "Cote-dor")) %>%
    mutate(region = stringr::str_replace(region,"ô", "o")
                           , region = stringr::str_replace(region, "è", "e")
                           , region = stringr::str_replace(region, "é", "e")
                           )
  CarteSNCF = left_join(SNCF, Carte1, by = "Arrival.station", copy = F)
  CarteSNCF = unique.data.frame(CarteSNCF)
  CarteSNCF = CarteSNCF %>% mutate(long = as.numeric(long), lat = as.numeric(lat))
  
  
  
