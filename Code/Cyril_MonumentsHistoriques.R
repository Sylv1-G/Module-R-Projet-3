# Installation des packages ----

library(librarian)
shelf(sf, httr,happign,dplyr,tmap,abind)
library(tmap);ttm()

# Etude des documents d'urbanisme d'une commune ----

is_rnu <- get_apicarto_gpu("INSEE", ressource = "municipality")
is_rnu$is_rnu

commune <- get_apicarto_cadastre("INSEE", "commune")
doc <- get_apicarto_gpu(commune, "document",  dTolerance = 10)

partition <- doc |> 
  filter(grid_title == "COMMUNE") |> 
  pull(partition)