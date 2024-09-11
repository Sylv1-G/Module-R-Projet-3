# Packages installation ----

library(librarian)
shelf(sf)

gpkg_path <- file.path("C:/Users/cyril/Downloads/FIF/ProjetRSIG/Module-R-Projet-3/Code/test.gpkg")

# Export a list of data frames to GPKG ----

layer_names <- c("prescriptions", "infos", "generateur", "assiette")

export_list_to_gpkg <- function(tes_all_gpu, layer_names, gpkg_path) {
  
  # Check that the number of layer names matches the number of data frames
  if (length(tes_all_gpu) != length(layer_names)) {
    stop("Le nombre de noms de couches doit correspondre au nombre de data frames.")
  }
  
  # Loop on each item in the list with layer names
  for (i in seq_along(tes_all_gpu)) {
    df <- tes_all_gpu[[i]]
    layer_name <- layer_names[i]
    st_write(df, gpkg_path, layer_name)
  }
}