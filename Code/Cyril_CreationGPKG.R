# Installation des packages ----

library(librarian)
shelf(sf)

gpkg_path <- file.path("C:/Users/cyril/Downloads/FIF/ProjetRSIG/Module-R-Projet-3/Code/test.gpkg")

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
    st_write(df, gpkg_path, layer_name)
  }
}