# À propos du document ----
# Date : 6 septembre 2024
# Autrice : Adèle Desaint 
# Objectif : Ce code a pour objectif de récupérer les données des SCOT et SUP
# soit les documents d'urbanisme non accessibles grâce à happign 

# Ouverture des packages ----
library(librarian)
shelf(sf, httr,happign,dplyr,tmap)
library(tmap);ttm()

# Identification des couches à importer ----
wfs_url <- "https://data.geopf.fr/wfs/ows?SERVICE=WFS&VERSION=1.1.0&REQUEST=GetCapabilities"

response <- GET(wfs_url)  # On teste la connexion à l'url grâce au package httr
content(response, "text")

# La commande "st_layers(wfs_url)" est trop lente, le nombre de couches étant
# Trop élevé. On lit le nom des couches voulues sur QGIS. 

SUP_surface <- st_read(wfs_url, layer = "wfs_sup:assiette_sup_s")   
head(SUP_surface)  # La colonne "typeass" donne le type de SUP
unique(SUP_surface$typeass)  # On a par exemple "Monument historique" 

# Exemple de recherche de monuments historiques dans les SUP ----

# On sélectionne que les SUP correspondant aux monuments historiques
SUP_MH = SUP_surface[SUP_surface$typeass == "Monument historique" | SUP_surface$typeass == "Périmètre des abords",]

# On sélectionne le point où on veut effectuer la recherche
point <- mapedit::drawFeatures()
point <- get_wfs(x = point,
                 layer = "ADMINEXPRESS-COG.LATEST:commune")
qtm(point)

# On vérifie que les géometries sont valides pour chercher les intersections 
st_is_valid(point)
st_is_valid(SUP_MH$the_geom)  # Certaines géometries sont invalides

# On sélectionne uniquement les géometries valides 
valid_SUP_MH <- SUP_MH[st_is_valid(SUP_MH$the_geom) == T, ]
st_transform(valid_SUP_MH,2154)
qtm(valid_SUP_MH)
  
# On cherche les intersections 
MH <- st_intersection(valid_SUP_MH$the_geom,point)
qtm(st_intersection(valid_SUP_MH$the_geom,point))
