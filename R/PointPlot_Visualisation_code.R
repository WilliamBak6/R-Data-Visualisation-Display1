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
  
  SNCF2 = SNCF2 %>% group_by(region) %>%
    mutate(OccurenceRegion = n())
  q = summary(SNCF2$OccurenceRegion)
  Q = q["1st Qu."]
  SNCF2b = SNCF2 %>% filter(OccurenceRegion > Q +100)


  ggplot(SNCF2b, aes(y = Number.of.cancelled.trains, x = Number.of.expected.circulations, color = Number.of.cancelled.trains, size = Number.of.cancelled.trains)) +
    geom_point() +
    geom_text_repel(aes(label = region)
                    , max.overlaps = 20) +
    geom_smooth(method = "lm"
               , color = "grey"
               , size = .5) +
    scale_size(range = c(1,5), name = "Nombre de Trains Retardé au Démarage"
               ) +
    scale_color_distiller(palette = "Reds", name = "Nombre de Trains Supprimé"
                          , direction = 1) +
    labs(title = "Visualisation du lien entre le nombre de trains prévu et le retard"
         , subtitle = "(Pour les trains SNCF)"
         , x = "Trains prévus pour la circulation"
         , y = "Trains supprimer"
         , caption = "Graph réalisé par Bakenga"
         ) +
    theme_hc() +
    theme(plot.title = element_text(size = 16
                                    , family = "sans"
                                    , color = "black"
                                    , face = "bold"
                                    , hjust = 0)
          , text = element_text(color = "black"
                                , family = "A")
          , plot.subtitle = element_text(size = 10
                                         , color = "black"
                                         , family = "sans"
                                         )
          , plot.caption = element_text(size = 10)
          , axis.title.x = element_text(size = 12
                                        , hjust = 1)
          , axis.title.y = element_text(size = 12)
          , legend.position = "bottom"
          , legend.box.background = element_rect(fill = "white"
                                                 , color = "black"
                                                 , linetype = 1
                                                 , size = .5)
          , legend.margin = margin(t = .25, r = 1.25, b = .25, l = 1.25, unit = "cm")
          , legend.text = element_text(size = 10
                                       , face = "bold")
          , legend.key = element_blank()
          ) +
    guides(alpha = "none", size = "none")

    ggsave("Visualisation-Lien-Prevu-Retard.png"
         , width = 8.5
         , height = 11
         , units = "in"
         , dpi = 300) 
  
