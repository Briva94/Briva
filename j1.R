# BLOC 2: J1

# NOTIONS DE RESEAUX INFORMATIQUES

# Réseau informatiique: un ens d'appareils reliés entre eux.

# CREATION DE LA CLE PAR R STUDIO de la cle GIT
# Notre cle SSH (obligatoire pour pouvoir pousser des modifications),...

# Git est un gestionnaire de versions (décentralisé) créé en 205 par Linus Torvards

## ADD: ajout d'un elt dans le code

## commit: validation de la modifification dans la zone index

## push: On pousse la modif vers le dépôt distant
## PULL: Tirer les modif du dépot distant vers le dépot

# NB: un fichier peut être modifier, indexé et validé.

# En travaillant seul, c'est aussi intéressant por voir la tracabilité de ces préédentes versions de codes ou travail.

# Dans la barre d'outil R studio, il faut aller dans "More" et dire à Git qui on est. Donc s'identifier.
# Manipulation de données avec R

# EN GROS, IL FAUT ARRETER DE CREER DE NEW SCRIP MAIS DES NOVEAUX DOSSIERS


## ------------- tidyverse

# c'est un ensemble de packages: il y a tibble, dplyr, readr, ggplot2 et tidyr(permet de réorganiser les données), forcats, 
#string, lubridate, purr

#tidyverse, c'est une coquille vide

# L'objet centrale, c'est tibble.

# Le but ==> la stat c'est de resumé l'information d'une manière pertinente.

#%>% : raccourci crtl+shit+m

# -------------- EXERCICE - TIDYVERSE 1 -----------------


library(dplyr)

# 1.
iris %>% dplyr::select(Petal.Length,Species)

# 2.

iris_tbl = iris %>% dplyr::filter(Species %in% c("versicolor","virginica"))

class(iris_tbl)

iris_tbl = as_tibble(iris_tbl)

# 3.

iris %>% filter(Species=="setosa") %>% summarise(nombre_sesota=n())

# 4.

iris %>% filter(Species=="versicolor") %>% summarise(Moyenne_Versicolor=mean(Petal.Width))

# 5. 

iris = iris %>% mutate(Sum.Width= Petal.Width + Sepal.Width)

# 6.

iris %>% group_by(Species) %>%
  summarise(SD=sd(Sepal.Length),Moy=mean(Sepal.Length))

# Exo suite : Houston flights

library("hflights")
hflights <- as_tibble(hflights)

# 1.
hflights %>% select(ends_with("Time"))

# 2. 

hflights %>% select(contains("st")|starts_with("Taxi"))

# Ou mieux

hflights %>% select(matches("D.st")|starts_with("Taxi"))
# 3.

hflights <- hflights %>% mutate(ActualGroundTime = ActualElapsedTime - AirTime)

# 4.

hflights <- hflights %>% mutate(AverageSpeed = Distance/AirTime) %>%
  arrange(desc(AverageSpeed))

# 5. et 6.

hflights %>% filter(Dest=="JFK") %>% summarise(nbre_JFK=n())

# 7.

hflights %>%
  summarise(nbre_vols=n(),n_dest=n_distinct(Dest),n_carrier=n_distinct(UniqueCarrier))
# 9.
hflights %>% group_by(UniqueCarrier)%>% summarise(nbre_vol=n(),Moy=mean(AirTime,na.rm = T))

## ------------------ TIDYVERSE AVANCE -------------------------

# 1.
don <- read_csv("C:/Users/cepe-s4-02/Desktop/Bienv/R DATA/data/rolandgarros2013.csv")

# 2.
table(don$Round) # On bvoit bien qu'il ny a qu'un gagnant

don %>% filter(Round=="5") %>% select(Player1,Player2)

# 3.

don %>% mutate(ACE=ACE.1+ACE.2) %>% summarise(moy_ace=mean(ACE))

# 4.
don %>% summarise(moy_ace1=mean(ACE.1), moy_ace2=mean(ACE.2))

# 5.
rg_joueurs = bind_rows(
  don %>% select(Player=Player1),
  don %>% select(Player=Player2))%>%
  distinct()

# 6.
joueur <- "Rafael Nadal"

rg_victoire <- function(joueur){
  don %>% 
    filter((Player1 == joueur & Result == 1) | (Player2 == joueur & Result == 0))%>%
    nrow()
}

rg_joueurs %>% rowwise() %>% mutate(Victoires=rg_victoire(Player))

# rowwise(): La fonction rowwise() dans le package dplyr sert à effectuer des opérations 
# ligne par ligne sur un data frame.

# 7.

don2 <- read_csv("C:/Users/cepe-s4-02/Desktop/Bienv/R DATA/data/openaustralie2013.csv")

# 8.
tennis <- bind_rows(RG=don,OA=don2,.id ="Tournoi")

tennis %>% group_by(Tournoi) %>%summarise(n=n())
# 9.
aces <- tennis %>% mutate(aces=ACE.1 + ACE.2) %>% group_by(Tournoi, Round) %>% 
  summarise(mean_aces=mean(aces))

aces # est déjà long
# large
aces %>% tidyr::pivot_wider(names_from=Round, values_from=mean_aces)

# 10.


# BASES DE DONNEES RELATIONNELLES ET LANGAGE SQL

# La library DBI permet tidyverse à se connecter à tout type de bases de données
