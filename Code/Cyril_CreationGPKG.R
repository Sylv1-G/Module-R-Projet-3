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

p1 <- get_apicarto_gpu("DU_05023", ressource = "prescription-surf")
p2 <- get_apicarto_gpu("DU_05023", ressource = "prescription-lin")
p3 <- get_apicarto_gpu("DU_05023", ressource = "prescription-pct")

dataframes_list <- list(p1, p2, p3)

export_list_to_gpkg <- function(dataframes_list, gpkg_path) {
  
  # Boucle sur chaque élément de la liste
  for (df in dataframes_list) {
    
    # Appel de la fonction export_to_gpkg pour chaque data frame
    export_to_gpkg(layer = df, gpkg_path, layer_name = layer_name)
  }
}
