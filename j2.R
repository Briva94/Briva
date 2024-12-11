
## Bases de données relationnelles et SQL - Exercices

# 1. 
setwd("C:/Users/cepe-s4-02/Desktop/Bienv/R DATA/data")

library(tidyverse)
library(DBI)
library(RSQLite)

con <- dbConnect(RSQLite::SQLite(), dbname="star.db")



# 2.

# 3.

dbGetQuery(con, "SELECT * from topologie")

# 4.
etat <- tbl(con, "Etat")
topologie <- tbl(con, "topologie")

# 5.

topologie %>% select(id,nom,id_proche_1) %>% collect()# Collect, c'est pour mettre la sortie à la 
# la forme d'un tibble ( mais ici, on voit que topologie est déjà chargée en tibble)

topologie %>% select(id,nom,id_proche_1) %>% show_query()
# Ici si topologie n'est pas chargée en tibble, il nous montrera bien que nous sommes dans l'envi
# ironnement du serveur (SQL) et surtout qu'est-ce qu'on a fait jusque là.
# 6.
topologie %>% select(id,nom,id_proche_1) %>%
  left_join(topologie, by=c("id_proche_1"="id")) %>% 
  select(id, nom=nom.x, nom_proche=nom.y) %>%
  collect()
  # On peut aussi utiliser la table Etat

# 7.

topologie %>% select(id,nom,id_proche_1,latitude,longitude) %>%
  left_join(topologie, by=c("id_proche_1"="id")) %>%
  mutate(distance=(latitude.x - latitude.y)^2 + (longitude.x - longitude.y)) 

# 8.
ma_latitude <- 48.1179151
ma_longitude <- -1.7028661
etat %>%
  mutate(distance=(latitude - ma_latitude)^2 + (longitude - ma_longitude)^2) %>%
  select(nom, distance, emplacements_disponibles) %>%
  arrange(distance) %>%
  head(3) %>%
  collect()

# 9.

# 10.
dbDisconnect(con)

## --------------- EXO 2: Musique -----------------

# 1.
con <- dbConnect(RSQLite::SQLite(), dbname="chinook.db")

Playlist <- tbl(con, "Playlist")
Track <- tbl(con, "Track")
PlaylistTrack <- tbl(con, "PlaylistTrack")

Playlist %>% collect()
Track %>% collect()
PlaylistTrack %>% collect()

# 2. (TrackId, cest le nombre de piste)

Piste=PlaylistTrack %>% group_by(PlaylistId) %>%
        summarise(Nbre_piste=n()) %>% arrange(desc(Nbre_piste))

# 3.
Piste %>% left_join(Playlist,by=c("PlaylistId"))%>%  left_join(Track, by=c("Name"))

# ------------------------------------------------------------------------------ #
# # #  -----------------  NO SQL ------------------
# ------------------------------------------------------------------------------ #


# Ils ont 4 grandes familles:

# - clé/valeur:point d'entrer (on se libère de la notion de table)
# - colonne
# - Document: genre d'orga qu'on trouve dans le web --> MongoDB
# - Graphe: ici on essaie de garder le monde du relationnel

# MONGODB fait appel Java script -> JASONl, mais permet de l'utiliser dans R

# ------------------------------------------------------------------------------ #
# --- Manipulation de données avec R -- Format JSON et API ------
# ------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------ #

library(jsonlite)

# Transformer un objet R en objet Json, "auto_unbox" permet à Json de ne pas considérer l'objet
# stocké comme un élément du type [1])

 json <- toJSON(
  list(nom="Gandalf", taille=1.68, residence=NA),
   auto_unbox=TRUE, # Option : évite les vecteurs de taille 1
   pretty=TRUE # Option : améliore l'affichage
   )
 json
 
 #  Exercice -- Format JSON et API
 
 library(jsonlite)

 don=iris 

 json <- toJSON(
   list(nom="don", valeur=42),
   auto_unbox=TRUE, # Option : évite les vecteurs de taille 1
   pretty=TRUE # Option : améliore l'affichage
 )
 json 

 class(json) 
 
 #2.
 obj <- fromJSON(json)
class(obj) 
obj

#3.

#4.

json2 <- toJSON(
  list(nom="don", valeur=42),
  auto_unbox=TRUE, 
  pretty=TRUE,
  dataframe = "columns"
)
json2
#  dataframe = c("rows", "columns", "values"): je dirais 3

#5. 
stream_out(don)

#6.
connexion <- file("test.json",open = "w") # On ouvre la connexion
stream_out(don,con = connexion) # open=r pour lecture
                                      # open=w pour l'écriture

close(connexion) # On ferme la connexion

# 7.
connexion <- file("test.json",open = "r") # On ouvre la connexion

stream_in(connexion)

close(connexion) # On ferme la connexion


### ------------------------- Exo: Star Wars API ------------------------------

             # Autre source (coquille du prof: "https://swapi-node.vercel.app")


url_next <- "https://swapi.dev/api/planets/?format=json"
planets <- NULL
while(!is.null(url_next)) { # Tant qu'il y a une URL à visiter
  obj <- fromJSON(url_next)
  planets <- rbind(planets, obj[["results"]])
  url_next <- obj[["next"]]
}
planets %>% summarise(n_planet=n())


# ------------------------------------------------------------------------------ #
# --- Manipulation de données avec R -- Web Scraping ------
# ------------------------------------------------------------------------------ #
# ------------------------------------------------------------------------------ #

library(rvest)

# Faire ctrl + u pour voir le format HTML du site web

# Faire "F12" pour avoir les différentes couches du HTML

 url_base <- "https://umizenbonsai.com/"
 url_next <- "shop/bonsai/coniferes/"

 data_html <- read_html(paste0(url_base, url_next))
 data_html
 
 #2.
 
  css_selector <- "li.entry"
  bonsai_nodes <- data_html %>% html_nodes(css_selector) # c'est mon arbre dome
  length(articles)
 
  # html_nodes renvoie une liste, on recupere plein de noeuds à la fois
  # html_node renvoie un élément
 #3.

  bonsais <- tibble()
  
  for( k in seq_along(bonsai_nodes)){
    bonsai_node <- bonsai_nodes[k]
    
    nom <- bonsai_node %>%
      html_node("li.title") %>% 
      html_text()
    
    prix_bloc <- bonsai_node %>% html_node("span.price")
    prix_nodes <- prix_bloc %>% 
      html_nodes("span.woocommerce-Price-amount")# span.Price n'est plus adapté car il y a des articles avec un price soldé
     if(length(prix_nodes)==1){
       prix <- prix_nodes[[1]] %>% html_text()
     }else{
       prix <- prix_bloc %>% 
         html_node("ins span.woocommerce-Price-amount")%>%
         html_text()
     }
    
    lien <- bonsai_node %>% 
      html_node("a.woocommerce-LoopProduct-link") %>% 
      html_attr("href") # Pour html_attr au singlier, il faut lui préciser un argument, ici c'est href pour avoir qu'un lien dans l'objet
    
    bonsais <- bonsais %>% bind_rows( tibble(nom=nom, prix=prix, lien=lien))
  }
  
  bonsais

  
  ## Exo 2
  
  url_wikipedia <- "https://fr.wikipedia.org/"
  url_blanchett <- "wiki/Cate_Blanchett"
  data_html <- paste0(url_wikipedia, url_blanchett) %>% read_html()
  film_selector <- "#mw-content-text div ul:nth-of-type(3) li i a"
  film_nodes <- data_html %>% html_nodes(film_selector) %>% html_attrs()
  films <- tibble()
  for(k in seq_along(film_nodes)) {
    film_node <- film_nodes[[k]]
    if("class" %in% names(film_node)) next # Absence de page dédiée
    if(film_node["title"] == "Galadriel") next # Mauvais lien
    films <- rbind(
      films,
      list(titre=film_node["title"], url=film_node["href"])
    )
  }
films  

# Lien IMDb
 
# Ici on prend l'exemple du 1er pour essayer
film_url <- films[1, "url"]# Quand on prend le 1er
film_html <- paste0(url_wikipedia, film_url) %>% 
  read_html()

film_nodes <- film_html %>%
  html_nodes("a.external.text")
film_imdb_url <- ""

for(k in seq_along(film_node)){
  if(film_nodes[[k]] %>% html_text()=="IMDb"){
    film_imdb_url <- film_nodes[[k]] %>% html_attr("href")
  }
  break
}

film_imdb_url <- film_imdb_url %>%
  str_extract("url_prefix=(.*)",group = 1) # (.*) : veut dire je veux tout ce qui est après le "=", peu importe le nombre de caractères.
film_imdb_url

# On peut mettre tout ça dans une fonction

get_url_imdb <- function(url_film) {
  # Les liens externes comme IMDb ont tous la classe "external text"
  external_selector <- "a[class='external text']"
  data_html <- paste0(url_wikipedia, url_film) %>% read_html()
  external_nodes <- data_html %>% html_nodes(external_selector)
  # Recherche du lien IMDb
  url <- NULL
  for(external_node in external_nodes) {
    if(external_node %>% html_text() == "IMDb") {
      external_attrs <- external_node %>% html_attrs()
      url <- external_attrs["href"]
      break
    }
  }
  # Extraction de l'URL IMDb par expression régulière
  # La regex "url_prefix=(.*)$" capture tout ce qui se trouve entre "url_prefix="
  # et la fin de la chaîne de caractères
  return(str_extract(url, "url_prefix=(.*)$", group=1))
}
# URL Casting (TODO)

film_imdb_id <- film_imdb_url %>% str_extract("id=(.*)", group = 1)
url_imdb_casting <- paste0( "https://www.imdb.com/title/", film_imdb_id, "/fullcredits")

