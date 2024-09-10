# A propos du document ----
# Date : 6 septembre 2024
# Autrice : Adele Desaint 
# Objectif : Ce code a pour objectif de recuperer les données des SCOT et SUP
# soit les documents d'urbanisme non accessibles grâce à happign 

# Ouverture des packages ----
library(librarian)
shelf(sf, httr,happign,dplyr,tmap,lwgeom)
library(tmap);ttm()

# Identification des couches a importer ----
wfs_url <- "https://data.geopf.fr/wfs/ows?SERVICE=WFS&VERSION=1.1.0&REQUEST=GetCapabilities"

response <- GET(wfs_url)  # On teste la connexion à l'url grâce au package httr
content(response, "text")

# La commande "st_layers(wfs_url)" est trop lente, le nombre de couches étant
# Trop élevé. On lit le nom des couches voulues sur QGIS. 

SUP_surface <- st_read(wfs_url, layer = "wfs_sup:assiette_sup_s") 
SUP_ligne <- st_read (wfs_url, layer = "wfs_sup:assiette_sup_l")

# On reprojete en Lambert 93

st_transform(SUP_surface, 2154)
st_transform(SUP_ligne, 2154)

head(SUP_surface)  # La colonne "typeass" donne le type de SUP
head(SUP_ligne)

unique(SUP_surface$typeass)  # On a par exemple "Monument historique" 
unique(SUP_ligne$typeass)  # Rien ne semble intéressant


# Exemple de recherche de monuments historiques dans les SUP ----

# On selectionne que les SUP correspondant aux monuments historiques
SUP_MH = SUP_surface[SUP_surface$typeass == "Monument historique" | SUP_surface$typeass == "Périmètre des abords",]

# On selectionne le point ou on veut effectuer la recherche
point <- mapedit::drawFeatures()
point <- get_wfs(x = point,
                 layer = "ADMINEXPRESS-COG.LATEST:commune")
qtm(point)

# On verifie que les geometries sont valides pour chercher les intersections 
st_is_valid(point)
st_is_valid(SUP_MH$the_geom)  # Certaines geometries sont invalides

# On cherche à reparer les geometries
lwgeom::lwgeom_make_valid(SUP_MH$the_geom)
st_make_valid(SUP_MH)  # Le nombre de geometries invalides reste identique

# On selectionne uniquement les geometries valides 
valid_SUP_MH <- SUP_MH[st_is_valid(SUP_MH$the_geom) == T, ]
qtm(valid_SUP_MH)

tmap_options(check.and.fix = T)  # Pour afficher les geometries invalides
invalid_SUP_MH <- SUP_MH[!st_is_valid(SUP_MH$the_geom) == T,]
qtm(invalid_SUP_MH)

# On cherche les intersections 
MH <- st_intersection(valid_SUP_MH$the_geom,point)
qtm(st_intersection(valid_SUP_MH$the_geom,point))

# On affiche les geometries invalides sur la zone etudiee la plus precise 
# On a vu dans le data frame invalid_SUP_MH que le code geographique des 
# geometries invalides est soit un code de departement soit un code INSEE 
# de commune. 

# Au niveau de la commune 
data("cog_2023")  # On trouve le code INSEE

MH_commune_partition <-
  grep("_13055_", invalid_SUP_MH$partition) # Exemple a Marseille : 31 SUP
View(invalid_SUP_MH[MH_commune_partition,])
qtm(invalid_SUP_MH[MH_commune_partition,])

MH_commune_nomass <-
  grep("_Courcelles_", invalid_SUP_MH$nomass) 
View(SUP_MH[MH_commune_nomass,])

# Au niveau du département 
MH_departement <- grep("_13_", invalid_SUP_MH$partition)
View(SUP_MH[MH_departement,])  

# Au niveau de la region : si on veut faire des recherches sur les geometries
# valides
code_region <- c("R1","R2","R3","R4","R6","R11","R24","R27","R28","R32","R44",
                "R52","R53","R75","R76","R84","R93","R94")
noms_region <- c("Guadeloupe","Martinique","Guyane","La Reunion","Mayotte",
                "Ile De France","Centre Val De Loire","Bourgogne Franche Comte",
                "Normandie","Hauts De France","Grand Est","Pays De La Loire",
                "Bretagne","Nouvelle Aquitaine","Occitanie",
                "Auvergne Rhone Alpes","PACA","Corse")
table_regions <- data.frame(code_region,noms_region)
View (table_regions)
rm(code_region, noms_region)

MH_region = grep("_R93_", invalid_SUP_MH$partition)
View(SUP_MH[MH_region,])

# Fonction propre pour rechercher les SUP ----

get.assiettes.sup <- function(){
  # Ouverture des packages
  library(librarian)
  shelf(sf, httr,happign,dplyr,tmap)
  library(tmap);ttm()
  
  # Recuperation des SUP
  wfs_url <- "https://data.geopf.fr/wfs/ows?SERVICE=WFS&VERSION=1.1.0&REQUEST=GetCapabilities"
  SUP_ass_s <- st_read(wfs_url, layer = "wfs_sup:assiette_sup_s") 
  
  SUP_ass_s <- st_transform(SUP_s, 2154)
  
  # Selection des SUP utiles
  SUP_ass_s <- SUP_ass_s[
    SUP_s$suptype == "a7"|   # forets de protection
    SUP_s$suptype == "el9"|   # passage sur le littoral
    SUP_s$suptype == "a4"|   # passage dans le lit et berges d'un cours d'eau
    SUP_s$suptype == "a9"|   # zones agricoles protegees
    SUP_s$suptype == "a10"|   # zone de protection du parteau de Saclay
    SUP_s$suptype == "ac1"|   # monuments historiques
    SUP_s$suptype == "ac4"|   # patrimoine architectural
    SUP_s$suptype == "ac2",]   # sites inscrits et classes
  
  return(SUP_ass_s)
}

get.generateurs.sup <- function(){
  # Ouverture des packages
  library(librarian)
  shelf(sf, httr,happign,dplyr,tmap)
  library(tmap);ttm()
  
  # Recuperation des SUP
  wfs_url <- "https://data.geopf.fr/wfs/ows?SERVICE=WFS&VERSION=1.1.0&REQUEST=GetCapabilities"
  SUP_gen_s <- st_read(wfs_url, layer = "wfs_sup:generateur_sup_s")
  SUP_gen_l <- st_read(wfs_url, layer = "wfs_sup:generateur_sup_l")
  SUP_gen_p <- st_read(wfs_url, layer = "wfs_sup:generateur_sup_p")

  # Selection des SUP utiles
  SUP_gen_s <- SUP_gen_s[
    SUP_gen_s$suptype == "a7"|   # forêts de protection
      SUP_gen_s$suptype == "a4"|   # passage dans le lit et berges d'un cours d'eau
      SUP_gen_s$suptype == "a9"|   # zones agricoles protégées
      SUP_gen_s$suptype == "a10"|   # zone de protection du parteau de Saclay
      SUP_gen_s$suptype == "ac1"|   # monuments historiques
      SUP_gen_s$suptype == "ac4"|   # patrimoine architectural
      SUP_gen_s$suptype == "ac2",]   # sites inscrits et classes
  
  SUP_gen_p <- SUP_s[
      SUP_gen_s$suptype == "ac1"]  # monuments historiques

  SUP_gen_s <- SUP_gen_s[
      SUP_gen_s$suptype == "a4"|   # passage dans le lit et berges d'un cours d'eau
      SUP_s$suptype == "el9"]   # passage sur le littoral
  
  return(list(SUP_gen_s, SUP_gen_p, SUP_gen_s))
}
  
get.sup.point(x){
  
  # Separation des geometries valides et invalides
  # Toutes les geometries lineaires sont valides
  valid_SUP_s <- SUP_s[st_is_valid(SUP_s$the_geom) == T, ]
  invalid_SUP_s <- SUP_s[!st_is_valid(SUP_s$the_geom) == T,]
  
  # Recherche des SUP dans la commune consideree
  point <- get_apicarto_cadastre(x, type = "commune")
  point <- st_transform(point, 2154)

  SUP_s_point <- valid_SUP_s[st_intersection(valid_SUP_s$the_geom,point),]
  
  SUP_commune <- grep(x, invalid_SUP_s$partition) 
  
  departement <- substring(x,1,2)
  SUP_departement <- grep("_'departement'_", invalid_SUP_s$partition)

  return(list(SUP_s_point, SUP_commune, SUP_departement))
}


# code pour avoir plus de 5000 entites

url_wfs <- wfs_url
layer_name <- "wfs_sup:assiette_sup_s"
start_index <- 0
page_size <- 5000
all_data <- list()

repeat {
  # Request with pagination (startIndex controls the offset)
  data <- st_read(url_wfs, layer = layer_name, options = paste0("startIndex=", start_index, "&maxFeatures=", page_size))
  
  # Break the loop if no more data is returned (i.e., the last batch was smaller than the page size)
  if (nrow(data) == 0 || nrow(data) < page_size)) break
  
  # Store the data in a list
  all_data[[length(all_data) + 1]] <- data
  
  # Update startIndex for the next page
  start_index <- start_index + page_size
}

# Combine all pages into one sf object
final_data <- do.call(rbind, all_data)

# Check how many rows were retrieved
nrow(final_data)
