


librarian::shelf(happign,dplyr,sf,tmaptools,OpenStreetMap)

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





# Affichage dans R ----

prescriptions <- get_apicarto_gpu(
  "DU_05023", 
  ressource = c("prescription-surf","prescription-lin","prescription-pct")
)

shp <- get_apicarto_cadastre("05023", type = "commune")

#affichage_prescription <- function(shp, list_urba){
  
  # Forcer le mode "plot" pour des cartes statiques
  tmap_mode("plot")
  
  geometry_type <- st_geometry_type(list_urba[[1]])
  
  polygones <- list_urba[[1]][geometry_type == "MULTIPOLYGON", ]
  points <- list_urba[[1]][geometry_type == "MULTIPOINT", ]
  
  bbox <- st_bbox(st_union(st_geometry(shp), st_geometry(list_urba[[1]])))
  
  osm_basemap <- read_osm(bbox, type = "osm", zoom = NULL) 
  
  tm_shape(osm_basemap) +  # Ajouter le fond de carte OpenStreetMap
    tm_rgb() + 
  tm_shape(polygones) +
    tm_fill(col = "libelle", palette = "Set1", legend.show = TRUE) +  
    tm_borders() +
    tm_shape(points) +
    tm_dots(col = "libelle", palette = "Dark2", size = 0.1, legend.show = TRUE)  +
    tm_layout(
      legend.position = c("right", "bottom"),  # Position de la légende en mode statique
      legend.outside = TRUE,  # Légende en dehors du panneau principal
      legend.bg.color = "white",  # Couleur de fond de la légende
      legend.text.size = 0.7,  # Taille du texte dans la légende
      legend.title.size = 0.8  # Taille du titre de la légende
    ) +
    tm_shape(shp) +
    tm_borders(col = "black", lwd = 2) 
}

affichage_prescription <- function(shp, list_urba){
  
  tmap_mode("view")
  
  geometry_type <- st_geometry_type(list_urba[[1]])
  
  polygones <- list_urba[[1]][geometry_type == "MULTIPOLYGON", ]
  points <- list_urba[[1]][geometry_type == "MULTIPOINT", ]
  
  tm_shape(shp) +
    tm_borders(col = "black", lwd = 2) + 
    tm_shape(polygones,
             group = "Polygones") +
    tm_fill(col = "libelle", 
            palette = "Set1",
            title = "Prescriptions polygones", 
            legend.show = TRUE) +  
    tm_borders() +
    tm_shape(points, 
             group = "Points") +
    tm_symbols(col = "libelle", 
               palette = "Dark2", 
               shape = 16, 
               size = 0.2,
               title.col = "Prescirptions points")  +
    tm_view(
      view.legend.position = c("right", "bottom"),
    )
}


affichage_prescription(x,tes_all_gpu)
