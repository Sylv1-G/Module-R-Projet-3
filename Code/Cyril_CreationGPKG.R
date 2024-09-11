# Installation des packages ----

library(librarian)
shelf(sf, httr,happign,dplyr,tmap,abind)
library(tmap);ttm()

# Exporter en GPKG un data frame ----

gpkg_path <- file.path("C:/Users/cyril/Downloads/FIF/ProjetRSIG/Module-R-Projet-3/Code/test.gpkg")

export_to_gpkg <- function(layer, gpkg_path, layer_name = NULL){
  
  # Si on ne précise pas de nom de couche, le nom de la variable
  # layer est utilisé (ex : pour routes, la couche s'appellera
  # "route")
  if (is.null(layer_name)){
    layer_name <- deparse(substitute(layer))
  }
  st_write(layer, gpkg_path, layer = layer_name)
}

# Exporter en GPKG une lidte de data frames ----

layer_names <- c("prescriptions", "infos", "generateur", "assiette")

export_list_to_gpkg <- function(tes_all_gpu, layer_names, gpkg_path) {
  
  # Vérifier que le nombre de noms de couches correspond au nombre de data frames
  if (length(tes_all_gpu) != length(layer_names)) {
    stop("Le nombre de noms de couches doit correspondre au nombre de data frames.")
  }
  
  # Boucle sur chaque élément de la liste avec les noms de couches
  for (i in seq_along(tes_all_gpu)) {
    df <- tes_all_gpu[[i]]
    layer_name <- layer_names[i]
    
    # Appel de la fonction export_to_gpkg pour chaque data frame avec le nom de la couche
    export_to_gpkg(layer = df, gpkg_path = gpkg_path, layer_name = layer_name)
  }
}