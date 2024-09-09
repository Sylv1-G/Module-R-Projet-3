# Installation des packages ----

library(librarian)
shelf(sf, httr,happign,dplyr,tmap,abind)
library(tmap);ttm()

# Etude des documents d'urbanisme d'une commune ----

is_rnu <- get_apicarto_gpu("54395", ressource = "municipality")
is_rnu$is_rnu

nancy <- get_apicarto_cadastre("54395", "commune")
doc <- get_apicarto_gpu(nancy, "document",  dTolerance = 10)

partition <- doc |> 
  filter(grid_title == "NANCY") |> 
  pull(partition)

# Modifier Fusion pour ne garder que les colonnes communes à tous

Fusion <- function(a, b, c, d, e, f, g, h, i, j, k, l, m) {
  
  dfs <- list(a, b, c, d, e, f, g, h, i, j, k, l, m)
  for (j in seq_along(dfs)) {
    if (is.null(dfs[[j]])) {
      dfs[[j]] <- data.frame()
    } else if ("txt" %in% colnames(dfs[[j]])) {
      dfs[[j]] <- dfs[[j]][, !colnames(dfs[[j]]) %in% "txt"]
    }
  }
  result <- do.call(rbind, dfs)
  
  return(result)
}

A <- get_apicarto_gpu(partition, ressource = "prescription-surf", 
                      categorie = c("A1", "A7", "A8"), dTolerance = 0)
B <- get_apicarto_gpu(partition, ressource = "prescription-lin", 
                      categorie = c("A1", "A7", "A8"), dTolerance = 0)
C <- get_apicarto_gpu(partition, ressource = "prescription-pct", 
                      categorie = c("A1", "A7", "A8"), dTolerance = 0)
D <- get_apicarto_gpu(partition, ressource = "info-surf", 
                      categorie = c("A1", "A7", "A8"), dTolerance = 0)
E <- get_apicarto_gpu(partition, ressource = "info-lin", 
                      categorie = c("A1", "A7", "A8"), dTolerance = 0)
Test <- Fusion(A, B, C, D, E, A, B, C, D, E, A, B, C)

Forets <- Fusion(get_apicarto_gpu(partition, ressource = "prescription-surf", 
                                  categorie = c("A1", "A7", "A8"), dTolerance = 0), 
                 get_apicarto_gpu(partition, ressource = "prescription-lin", 
                                  categorie = c("A1", "A7", "A8"), dTolerance = 0), 
                 get_apicarto_gpu(partition, ressource = "prescription-pct", 
                                  categorie = c("A1", "A7", "A8"), dTolerance = 0), 
                 get_apicarto_gpu(partition, ressource = "info-surf", 
                                  categorie = c("A1", "A7", "A8"), dTolerance = 0), 
                 get_apicarto_gpu(partition, ressource = "info-lin", 
                                  categorie = c("A1", "A7", "A8"), dTolerance = 0), 
                 get_apicarto_gpu(partition, ressource = "info-pct", 
                                  categorie = c("A1", "A7", "A8"), dTolerance = 0), 
                 get_apicarto_gpu(partition, ressource = "acte-sup", 
                                  categorie = c("A1", "A7", "A8"), dTolerance = 0), 
                 get_apicarto_gpu(partition, ressource = "generateur-sup-s", 
                                  categorie = c("A1", "A7", "A8"), dTolerance = 0), 
                 get_apicarto_gpu(partition, ressource = "generateur-sup-l", 
                                  categorie = c("A1", "A7", "A8"), dTolerance = 0), 
                 get_apicarto_gpu(partition, ressource = "generateur-sup-p", 
                                  categorie = c("A1", "A7", "A8"), dTolerance = 0), 
                 get_apicarto_gpu(partition, ressource = "assiette-sup-s", 
                                  categorie = c("A1", "A7", "A8"), dTolerance = 0), 
                 get_apicarto_gpu(partition, ressource = "assiette-sup-l", 
                                  categorie = c("A1", "A7", "A8"), dTolerance = 0), 
                 get_apicarto_gpu(partition, ressource = "assiette-sup-p", 
                                  categorie = c("A1", "A7", "A8"), dTolerance = 0))

# Littoral <- get_apicarto_gpu(partition, ressource = "document", 
#                           categorie = c("EL9"), 
#                           dTolerance = 0)
# 
# Eaux <- get_apicarto_gpu(partition, ressource = "document", 
#                           categorie = c("A4", "AS1"), 
#                           dTolerance = 0)
# 
# RNPN <- get_apicarto_gpu(partition, ressource = "document", 
#                           categorie = c("AC3", "EL10"), 
#                           dTolerance = 0)
# 
# ZAP <- get_apicarto_gpu(partition, ressource = "document", 
#                           categorie = c("A9", "A10"), 
#                           dTolerance = 0)
# 
# MHSPR <- get_apicarto_gpu(partition, ressource = "document", 
#                           categorie = c("AC1", "AC4", "AC4bis"), 
#                           dTolerance = 0)
# 
# MNS <- get_apicarto_gpu(partition, ressource = "document", 
#                           categorie = c("AC2"), 
#                           dTolerance = 0)
# 
# Sportif <- get_apicarto_gpu(partition, ressource = "document", 
#                           categorie = c("JS1"), 
#                           dTolerance = 0)

# Forets <- st_transform(Forets, 2154)
# 
# st_write(Forets, "test.gpkg", layer = "Forets", append = FALSE)
# 
# Littoral <- st_transform(Littoral, 2154)
# 
# st_write(Littoral, "test.gpkg", layer = "Littoral", append = FALSE)
# 
# Eaux <- st_transform(Eaux, 2154)
# 
# st_write(Eaux, "test.gpkg", layer = "Eaux", append = FALSE)
# 
# RNPN <- st_transform(RNPN, 2154)
# 
# st_write(RNPN, "test.gpkg", layer = "RNPN", append = FALSE)
# 
# ZAP <- st_transform(ZAP, 2154)
# 
# st_write(ZAP, "test.gpkg", layer = "ZAP", append = FALSE)
# 
# MHSPR <- st_transform(MHSPR, 2154)
# 
# st_write(MHSPR, "test.gpkg", layer = "MHSPR", append = FALSE)
# 
# MNS <- st_transform(MNS, 2154)
# 
# st_write(MNS, "test.gpkg", layer = "MNS", append = FALSE)
# 
# Sportif <- st_transform(Sportif, 2154)
# 
# st_write(Sportif, "test.gpkg", layer = "Sportif", append = FALSE)

# tmap_options(check.and.fix = TRUE)
# 
# tm_shape(Forets)+
#   tm_polygons(col = "darkgreen")
# 
# tm_shape(Littoral)+
#   tm_polygons(col = "beige")
# 
# tm_shape(Eaux)+
#   tm_polygons(col = "blue")
# 
# tm_shape(RNPN)+
#   tm_polygons(col = "green")
# 
# tm_shape(ZAP)+
#   tm_polygons(col = "yellow")
# 
# tm_shape(MHSPR)+
#   tm_polygons(col = "brown")
# 
# tm_shape(MNS)+
#   tm_polygons(col = "lightgreen")
# 
# tm_shape(Sportif)+
#   tm_polygons(col = "grey")