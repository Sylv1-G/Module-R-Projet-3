


librarian::shelf(happign,dplyr,sf,tmaptools)

library(tmap); tmap_mode("view"); tmap_options(check.and.fix = TRUE)


# Répertoire de travail relatif à la source du fichier ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Nettoyage de l'environement R ----
rm(list=ls())


# Function to get insee code from a shape ----
get.code.insee <- function(shp){
  if (inherits(shp, c("sf", "sfc"))) {
    communes <- get_apicarto_gpu(shp,"municipality")
    code_insee <- communes$insee
  } else {
    stop("x must be of class sf or sfc.")
  }
  
  return(code_insee)
}

# Function to get partitions from insee codes ----
insee.to.partition <- function(code_insee){
  
  if(!inherits(code_insee,"character")){
    stop("x must be of class character")
  }
  
  # Is there rnu 
  is_rnu <- get_apicarto_gpu(code_insee, ressource = "municipality")
  
  is_rnu_TRUE <- filter(is_rnu, is_rnu == TRUE)
  is_rnu_FALSE <- filter(is_rnu, is_rnu == FALSE)
  
  cat("\nCommunes with rnu are:\n")
  print(is_rnu_TRUE$name)
  
  cat("\nCommunes without rnu are:\n")
  print(is_rnu_FALSE$name)
  
  doc_urbanisme <- data.frame(grid_title = character(),
                              partition = character(),
                              du_type = character(),
                              stringsAsFactors = FALSE)
  
  for (i in 1:nrow(is_rnu_FALSE)) {
    
    row <- is_rnu_FALSE[i,]
    doc <- get_apicarto_gpu(row, "document", dTolerance = 10)
    
    if (is.null(doc)) {
      next
    }
    
    doc_filtered <- doc |> 
      filter(grid_title == row$name)
    
    # Si aucune correspondance, chercher les lignes avec du_type == "PLUi"
    if (nrow(doc_filtered) == 0) {
      doc_filtered <- doc |>
        filter(du_type == "PLUi")
    }
    
    # Si toujours pas de résultats, afficher un avertissement
    if (nrow(doc_filtered) == 0) {
      warning(paste("No partitions found for", row$name))
      next
    }
    
    # Ajouter les lignes filtrées au data.frame
    doc_urbanisme <- rbind(doc_urbanisme, 
                           doc_filtered)
  }
  
  doc_urbanisme_unique <- doc_urbanisme[!duplicated(doc_urbanisme), ]
  
  cat("\nThe following town planning documents exist in the surrounding municipalities:\n")
  print (as.data.frame(doc_urbanisme_unique)[,c("grid_title", "partition", "du_type")])
  
  return(doc_urbanisme_unique)
}

# TESTS
res <- mapedit::drawFeatures()

resultats <- insee.to.partition(get.code.insee(res))

resultats <- insee.to.partition("56031")







# Affichage dans r ----

affichage <- function(shp, gpu_all, type = "Prescriptions"){
  
  types <- c("Prescriptions", "Informations", "SUP")
  
  if (!type %in% types){
    stop ("Type must be 'Prescriptions', 'Informations' or 'SUP'")
  }

  tmap_mode("view")
  
  # Definition de l'affichage
  x <- 1
  n <- 1
  if (type == "Informations"){
    x <- 2
  } else if (type == "SUP") {
    x <- 4
    n <- 2
  }
  
  
  # Creation de la carte interactive
  map <- tm_shape(shp) +
    tm_borders(col = "black", lwd = 2) +
    tm_view(view.legend.position = c("right", "bottom"))
  
  for (i in 1:n){
    # Separation des types de geometries
    geometry_type <- st_geometry_type(gpu_all[[x]])
    
    polygones <- st_make_valid(gpu_all[[x]][geometry_type == "MULTIPOLYGON", ])
    lignes <- st_make_valid(gpu_all[[x]][geometry_type == "MULTILINESTRING", ])
    points <- st_make_valid(gpu_all[[x]][geometry_type == "MULTIPOINT", ])
    
    if (nrow(polygones) > 0) {
      map <- map +
        tm_shape(polygones, group = "Polygones") +
        tm_fill(col = "libelle",
                alpha = ifelse(x == 4, 0.1, 0.9),
                palette = "Spectral",
                title = ifelse(x == 4, 
                               "Assiette SUP", 
                               paste(type, "surfaciques")), 
                legend.show = TRUE) +  
        tm_borders()
    }
    
    if (nrow(lignes) > 0) {
      map <- map +
        tm_shape(lignes, group = "Lignes") +
        tm_lines(col = "libelle", 
                 palette = "Accent", 
                 title.col = ifelse(x == 4, 
                                    "Assiette SUP", 
                                    paste(type, "lineaires")),
                 legend.show = TRUE)
    }
    
    if (nrow(points) > 0) {
      map <- map +
        tm_shape(points, group = "points") +
        tm_symbols(col = "libelle",
                   palette = "Paired",
                   shape = 21,
                   size = 0.2,
                   title.col = paste(type, "ponctuelles"))
    }
    x <- x - 1
  }
  
  map

}


affichage(shp, gpu_all_2154, type = "SUP")
