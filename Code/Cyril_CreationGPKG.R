# Installation des packages ----

library(librarian)
shelf(sf, httr,happign,dplyr,tmap,abind)
library(tmap);ttm()

# Exporter en GPKG un data frame ----

gpkg_path <- file.path(XXX)

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

dataframes_list <- list(XXX)

export_list_to_gpkg <- function(dataframes_list, gpkg_path) {
  
  # Boucle sur chaque élément de la liste
  for (df in dataframes_list) {
    
    # Appel de la fonction export_to_gpkg pour chaque data frame
    export_to_gpkg(layer = df, gpkg_path = gpkg_path)
  }
}
