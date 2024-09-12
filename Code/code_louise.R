


librarian::shelf(happign,dplyr,sf,tmaptools)

library(tmap); tmap_mode("view"); tmap_options(check.and.fix = TRUE)


# Répertoire de travail relatif à la source du fichier ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Nettoyage de l'environement R ----
rm(list=ls())


# fonction pour obtenir les codes insee des communes associees a une geometrie ----
get.code.insee <- function(shp){
  if (inherits(shp, c("sf", "sfc"))) {
    communes <- get_apicarto_gpu(shp,"municipality")
    code_insee <- communes$insee
  } else {
    stop("x must be of class sf or sfc.")
  }
  return(code_insee)
}

# Fonction pour décrire quels sont les documents d'urbanisme present dans les communes ----
insee.to.documents <- function(code_insee){
  
  if(!inherits(code_insee,"character")){
    stop("x must be of class character")
  }
  
  # Y a-t-il un rnu associé à ces codes insee ?
  is_rnu <- get_apicarto_gpu(code_insee, ressource = "municipality")
  
  # Séparation des municipalités avec et sans rnu
  is_rnu_TRUE <- filter(is_rnu, is_rnu == TRUE)
  is_rnu_FALSE <- filter(is_rnu, is_rnu == FALSE)
  
  # Affichage des communes présentes avec ou sans rnu
  cat("\nCommunes with rnu are:\n")
  print(is_rnu_TRUE$name)
  
  cat("\nCommunes without rnu are:\n")
  print(is_rnu_FALSE$name)
  
  # Creation de dataframe conenant les documents d'urbanismes 
  doc_urbanisme <- data.frame(grid_title = character(),
                              partition = character(),
                              du_type = character(),
                              stringsAsFactors = FALSE)
  
  # Parcours de des comunnes non soumises au rnu
  for (i in 1:nrow(is_rnu_FALSE)) {
    
    row <- is_rnu_FALSE[i,]
    doc <- get_apicarto_gpu(row, "document", dTolerance = 10)  # chargement des documents de la commune
    
    if (is.null(doc)) {
      next
    }
    
    doc_filtered <- doc |> 
      filter(grid_title == row$name) # extraction des documents spécifiques à la commune (PLU, PSMV, CC)
    
    # Si aucuns documents associes a la commune, 
    # extraction du PLUi s'il existe
    if (nrow(doc_filtered) == 0) {
      doc_filtered <- doc |>
        filter(du_type == "PLUi")
    }
    
    # Si toujours pas de résultats, afficher un avertissement
    if (nrow(doc_filtered) == 0) {
      warning(paste("No document found for", row$name))
      next
    }
    
    # Ajout des documents filtrees au dataframe
    doc_urbanisme <- rbind(doc_urbanisme, 
                           doc_filtered)
  }
  
  # suppression des dupplicats (par exemple si plusieurs fois le meme PLUi)
  doc_urbanisme_unique <- doc_urbanisme[!duplicated(doc_urbanisme), ]
  
  # Affichage des partitions de documents d'urbanisme associes aux communes.
  cat("\nThe following town planning documents exist in the surrounding municipalities:\n")
  print (as.data.frame(doc_urbanisme_unique)[,c("grid_title", "partition", "du_type")])
  
  return(doc_urbanisme_unique)
}

# TESTS
res <- mapedit::drawFeatures()

resultats <- insee.to.documents(get.code.insee(res))

resultats <- insee.to.documents("56031")







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
  print(map)

}

affichage.interactif <- function (shp, gpu_all) {
  
  cat("\nLes trois carte vont s'afficher au fur et à mesure.\n")
  
  for (type in c("Prescriptions", "Informations", "SUP")){
    
    cat(paste("\nAffichage des", type))
    
    affichage(shp, gpu_all, type)
  }
}

affichage.interactif(shp, gpu_all_2154)
affichage(shp, gpu_all_2154, type = "Prescriptions")
