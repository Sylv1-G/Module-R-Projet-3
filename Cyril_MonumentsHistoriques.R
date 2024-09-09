# Installation des packages ----

library(librarian)
shelf(sf, httr,happign,dplyr,tmap)
library(tmap);ttm()

# Etude des documents d'urbanisme d'une commune ----

is_rnu <- get_apicarto_gpu("54395", ressource = "municipality")
is_rnu$is_rnu

nancy <- get_apicarto_cadastre("54395", "commune")
doc <- get_apicarto_gpu(nancy, "document",  dTolerance = 10)

partition <- doc |> 
  filter(grid_title == "NANCY") |> 
  pull(partition)

Forets <- get_apicarto_gpu(partition, ressource = "document", 
                         categorie = c("A1", "A7", "A8"), 
                         dTolerance = 0)

Littoral <- get_apicarto_gpu(partition, ressource = "document", 
                          categorie = c("EL9"), 
                          dTolerance = 0)

Eaux <- get_apicarto_gpu(partition, ressource = "document", 
                          categorie = c("A4", "AS1"), 
                          dTolerance = 0)

RNPN <- get_apicarto_gpu(partition, ressource = "document", 
                          categorie = c("AC3", "EL10"), 
                          dTolerance = 0)

ZAP <- get_apicarto_gpu(partition, ressource = "document", 
                          categorie = c("A9", "A10"), 
                          dTolerance = 0)

MHSPR <- get_apicarto_gpu(partition, ressource = "document", 
                          categorie = c("AC1", "AC4", "AC4bis"), 
                          dTolerance = 0)

MNS <- get_apicarto_gpu(partition, ressource = "document", 
                          categorie = c("AC2"), 
                          dTolerance = 0)

Sportif <- get_apicarto_gpu(partition, ressource = "document", 
                          categorie = c("JS1"), 
                          dTolerance = 0)

tmap_options(check.and.fix = TRUE)

tm_shape(Forets)+
  tm_polygons(col = "darkgreen")

tm_shape(Littoral)+
  tm_polygons(col = "beige")

tm_shape(Eaux)+
  tm_polygons(col = "blue")

tm_shape(RNPN)+
  tm_polygons(col = "green")

tm_shape(ZAP)+
  tm_polygons(col = "yellow")

tm_shape(MHSPR)+
  tm_polygons(col = "brown")

tm_shape(MNS)+
  tm_polygons(col = "lightgreen")

tm_shape(Sportif)+
  tm_polygons(col = "grey")