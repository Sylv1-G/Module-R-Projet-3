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

MHSPR <- get_apicarto_gpu(partition, ressource = "document", 
                         categorie = c("AC1", "AC4", "AC4bis"), 
                         dTolerance = 0)

tm_shape(MHSPR)+
  tm_polygons()
