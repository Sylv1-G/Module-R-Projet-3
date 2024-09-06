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

library(tmap); tmap_mode("view"); tmap_options(check.and.fix = TRUE)


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


# idée amélio récupérerer les communes présente a partir d'un cadastre ou autre shp 
commune.to.partition <- function(commune){
  
  is_rnu <- get_apicarto_gpu(commune, ressource = "municipality")
  nom_com <- is_rnu$name
  print("présence de RNU :")
  print(is_rnu$is_rnu)
  
  com <- get_apicarto_cadastre(commune, "commune")
  doc <- get_apicarto_gpu(com, "document", dTolerance = 10)
  
  partition <- doc |> 
    filter(grid_title == nom_com) |> 
    pull(partition)
  
  return(partition)
  
  
}

commune.to.partition("29112")

# ressource = "prescription-surf", "prescription-lin","prescription-pct"

libelle.prescription <- function(commune){
  
  partition <- commune.to.partition(commune)
  
  prescription_surf <- get_apicarto_gpu(partition, ressource ="prescription-surf")
  libelle_surf <- unique(prescription_surf$libelle)
  
  prescription_lin <- get_apicarto_gpu(partition, ressource ="prescription-lin")
  libelle_lin <- unique(prescription_lin$libelle)
  
  prescription_pct <- get_apicarto_gpu(partition, ressource ="prescription-pct")
  libelle_pct <- unique(prescription_pct$libelle)
  
  print(libelle_surf)
  print(libelle_lin)
  print(libelle_pct)
  libelle <- c(libelle_surf, libelle_lin, libelle_pct)
  
  return(libelle)
  
}


libelle_sur <- libelle.prescription("29112")


libelle_lin <- test.function("29112","prescription-lin")
libelle_pct <- test.function("29112","prescription-pct")



# teste RNU 
is_rnu <- get_apicarto_gpu("56031", ressource = "municipality")
is_rnu$is_rnu 
# Penmarch is under the RNU and therefore has a document of urbanism

is_rnu <- get_apicarto_gpu("23004", ressource = "municipality")
is_rnu$is_rnu 

# Anzeme is under the RNU and therefore has a town planning document


# find out if documents are available
penmarch <- get_apicarto_cadastre("80784", "commune")
doc <- get_apicarto_gpu(penmarch, "document", dTolerance = 10) # complex geometry handle with dTolerance
# 2 document sont dispo, mais un seul est pour penmarch

briancon <- get_apicarto_cadastre("05023", "commune")
qtm(lanildut)

doc <- get_apicarto_gpu(lanildut, "document",  dTolerance = 10) # complex geometry handle with dTolerance
# Six documents are available inside the geometry.
# In reality, only one document exists for the commune of lanildut but because borders are not coherent with urban planning document, several communes are returned.

partition <- doc |> 
  filter(grid_title == "BRIANCON") |> 
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

prescriptions_2 <- get_apicarto_gpu("DU_05023", ressource = c("generateur-sup-s",
                                                              "generateur-sup-l",
                                                              "generateur-sup-p",
                                                              "assiette-sup-s",
                                                              "assiette-sup-l",
                                                              "assiette-sup-p"))


ebc <- prescriptions_2[prescriptions_2$libelle == "EBC",]

align_arbre <- prescriptions_2[prescriptions_2$libelle == "Alignement d'arbre", ]

com <- get_apicarto_cadastre("93014", "commune")

qtm(align_arbre)
