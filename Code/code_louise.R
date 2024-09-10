


librarian::shelf(happign,dplyr,sf)

library(tmap); tmap_mode("view"); tmap_options(check.and.fix = TRUE)


# Répertoire de travail relatif à la source du fichier 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Nettoyage de l'environement R ----
rm(list=ls())


# Function to get insee code from a shape
get.code.insee <- function(shp){
  if (inherits(shp, c("sf", "sfc"))) {
    communes <- get_apicarto_gpu(shp,"municipality")
    code_insee <- communes$insee
  } else {
    stop("x must be of class sf or sfc.")
  }
  
  return(code_insee)
}

# Function to get partitions from insee codes
insee.to.partition <- function(code_insee){
  
  if(!inherits(code_insee,"character")){
    stop("x must be of class character")
  }
  
  # Is there rnu 
  is_rnu <- get_apicarto_gpu(code_insee, ressource = "municipality")
  
  is_rnu_TRUE <- filter(is_rnu, is_rnu == TRUE)
  is_rnu_FALSE <- filter(is_rnu, is_rnu == FALSE)
  
  cat("\nLes communes avec rnu sont:\n")
  print(is_rnu_TRUE$name)
  
  cat("\nLes communes sans rnu sont:\n")
  print(is_rnu_FALSE$name)
  
  # Téléchargements des documments d'urbanisme et de leur partition
  partitions <- c()
  
  for (i in 1:nrow(is_rnu_FALSE)) {
    
    row <- is_rnu_FALSE[i,]
    
    doc <- get_apicarto_gpu(row, "document", dTolerance = 10)
    
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

resultats <- insee.to.partition(get.code.insee(res))

resultats <- insee.to.partition("56031")
