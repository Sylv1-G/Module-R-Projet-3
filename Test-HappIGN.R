# A propos du code -----

# Titre du code : Test des principales fonction du package happign
# But du code : Se familiariser avec happign
# Auteur : Sylvain Giraudo
# Contact : Sylvain.giraudo13@gmail.com
# Dernière mise à jour :

# Installation des Packages ----

# permet de lancer la ligne de code suivante
#install.packages("rstudioapi","librarian")

# instalation des packages non instalé et chargement des packages

librarian::shelf(happign,dplyr,sf)

#> tmap mode set to plotting

# Repertoir de travail -----

# Répertoire de travail relatif à la source du fichier 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Nettoyage de l'environement R ----
rm(list=ls())


# fonction ----

#fonction sont séparé par des . et tout en minuscule


# Importation des données ----


# Exploration des données ----


# Analyse ----

is_rnu <- get_apicarto_gpu("56031", ressource = "municipality")
#> Features downloaded : 1
is_rnu$is_rnu 
#> [1] FALSE

# Penmarch is under the RNU and therefore has a document of urbanism

is_rnu <- get_apicarto_gpu("23004", ressource = "municipality")
#> Features downloaded : 1
is_rnu$is_rnu 
#> [1] TRUE

# Anzeme is under the RNU and therefore has a town planning document


# find out if documents are available
penmarch <- get_apicarto_cadastre("29158", "commune")
doc <- get_apicarto_gpu(penmarch, "document", dTolerance = 10) # complex geometry handle with dTolerance
# Because NULL is returned, it means that penmarch dont have available urban planning document

lanildut <- get_apicarto_cadastre("29112", "commune")
doc <- get_apicarto_gpu(lanildut, "document",  dTolerance = 10) # complex geometry handle with dTolerance
# Six documents are available inside the geometry.
# In reality, only one document exists for the commune of lanildut but because borders are not coherent with urban planning document, several communes are returned.

partition <- doc |> 
  filter(grid_title == "LANILDUT") |> 
  pull(partition)

partition <- doc |> 
  filter(grid_title == "PENMARCH") |> 
  pull(partition)

zone_urba <- get_apicarto_gpu(partition, ressource = "zone-urba")

# click on polygon for legend
tm_shape(zone_urba)+
  tm_polygons("libelong", legend.show = FALSE)

ressources <- c("prescription-lin",
                "prescription-pct")

prescriptions <- get_apicarto_gpu(partition,
                                  ressource = ressources,
                                  dTolerance = 10)
#> Warning: Resources have different attributes and cannot be joined. List is
#> returned.

tm_shape(prescriptions[[1]])+
   tm_lines("libelle", legend.col.show = FALSE, lwd = 2)+
tm_shape(prescriptions[[2]])+
   tm_dots("libelle",  legend.show = FALSE, size = 0.1)


#-------------------------------------------------------------------

prescriptions_2 <- get_apicarto_gpu("DU_93014", ressource = c("prescription-surf", "prescription-lin"))

ebc <- prescriptions[prescriptions$libelle == "EBC", ]

align_arbre <- prescriptions[prescriptions$libelle == "Alignement d'arbre", ]

com <- get_apicarto_cadastre("93014", "commune")

qtm(align_arbre)
