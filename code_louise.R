


librarian::shelf(happign,dplyr,sf)

library(tmap); tmap_mode("view"); tmap_options(check.and.fix = TRUE)


#> tmap mode set to plotting

# Repertoir de travail -----

# Répertoire de travail relatif à la source du fichier 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Nettoyage de l'environement R ----
rm(list=ls())



# Function to get insee code from a shape
get_code_insee <- function(shp){
  if (inherits(shp, c("sf", "sfc"))) {
    communes <- get_apicarto_gpu(shp,"municipality")
    commune <- communes$insee
  } else {
    stop("x must be of class sf or sfc.")
  }
  
  return(commune)
}

# Function to get partitions from insee codes
commune.to.partition <- function(commune){
  
  if(!inherits(commune,"character")){
    stop("x must be of class character")
  }
  
  # Is there rnu 
  is_rnu <- get_apicarto_gpu(commune, ressource = "municipality")
  
  is_rnu_TRUE <- filter(is_rnu, is_rnu == TRUE)
  is_rnu_FALSE <- filter(is_rnu, is_rnu == FALSE)
  
  cat("Communes avec rnu :\n")
  print(is_rnu_TRUE$name)
  
  cat("\nCommunes sans rnu :\n")
  print(is_rnu_FALSE$name)
  
  # Téléchargements des documments d'urbanisme et de leur partition
  partitions <- c()
  
  for (i in 1:nrow(is_rnu_FALSE)) {
    
    row <- is_rnu_FALSE[i,]
    
    # Telechargement du cadastre des communes sans rnu
    com <- get_apicarto_cadastre(row$insee, "commune")
    
    if (is.null(com)) {
      next
    }
    
    doc <- get_apicarto_gpu(com, "document", dTolerance = 100)
    
    if (is.null(doc)) {
      next
    }
    
    partitions_to_add <- doc |>
      filter(grid_title == row$name) |>
      pull(partition)
    
    if (length(partitions_to_add) == 0) {
      partitions_to_add <- doc |>
        filter(du_type == "PLUi") |>
        pull(partition)
    }
    
    if (length(partitions_to_add) == 0) {
      warning(paste("No partitions found for",row$name))
    }
    
    partitions <- c(partitions, partitions_to_add)
  }
  
  return(unique(partitions))
}

# TESTS
res <- mapedit::drawFeatures()

resultats <- commune.to.partition(get_code_insee(res))

resultats <- commune.to.partition("56031")