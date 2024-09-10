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


# X peut être un code INSEE ou une géométrie (par exemple des parcelles)
# Problèmes à régler : croiser avec les PLUi, dTolerance à mettre à 100 pour certaines communes

commune.to.partition <- function(x){

  if (inherits(x, c("sf", "sfc"))) {
    communes <- get_apicarto_gpu(x,"municipality")
    commune <- communes$insee
  } else if(inherits(x,"character")){
    commune <- x
  } else {
    stop("x must be of class character, sf or sfc.")
  }
  
  # is_rnu
  is_rnu <- get_apicarto_gpu(commune, ressource = "municipality")
  
  is_rnu_TRUE <- filter(is_rnu, is_rnu == TRUE)
  is_rnu_FALSE <- filter(is_rnu, is_rnu == FALSE)
  
  cat("Communes avec is_rnu == TRUE :\n")
  print(is_rnu_TRUE$name)
  
  cat("\nCommunes avec is_rnu == FALSE :\n")
  print(is_rnu_FALSE$name)
  
  nom_com <- is_rnu_FALSE$name

  
  # Téléchargements des documments d'urbanisme et de leur partition
  partitions <- c()
  
  for (i in 1:nrow(is_rnu_FALSE)) {
    
    row <- is_rnu_FALSE[i,]
    
    # Téléchargement du cadastre des communes sans rnu
    com <- get_apicarto_cadastre(is_rnu_FALSE$insee, "commune")
    
    if (is.null(com)) {
      next
    }
    
    doc <- get_apicarto_gpu(com, "document", dTolerance = 100)
    
    if (is.null(doc)) {
      next
    }
    
    partitions_to_add <- doc |>
      filter(grid_name == row$name) |>
      pull(partition)
    
    if (length(partitions_to_add) == 0) {
      partitions_to_add <- doc |>
        filter(du_type == "PLUi") |>
        pull(partition)
    }
    
    partitions <- c(partitions, partitions_to_add)
  }
  
  return(unique(partitions))
}


res <- mapedit::drawFeatures()
commune.to.partition(res)

résultats<- commune.to.partition("56031")

# ressource = "prescription-surf", "prescription-lin","prescription-pct"

commune <- "29112"

code_prescription <- c("01","05","07","18","19","25","31")
libelle_prescription <- c(
  "Espace boisé",
  "Emplacement réservé",
  "Patrimoine bâti,paysager ou éléments de paysages à protéger",
  "Périmètre comportant des orientations d’aménagement et deprogrammation (OAP)",
  "Secteur protégé en raison de la richesse du sol et du sous-sol",
  "Eléments de continuité écologique et trame verte et bleue",
  "Espaces remarquables du littoral")

libelle.prescription <- function(commune){
  
  partition <- commune.to.partition(commune)
  
  prescription_surf <- get_apicarto_gpu(partition, ressource ="prescription-surf")
  libelle_surf <- unique(prescription_surf$libelle)
  
  prescription_lin <- get_apicarto_gpu(partition, ressource ="prescription-lin")
  libelle_lin <- unique(prescription_lin$libelle)
  
  prescription_pct <- get_apicarto_gpu(partition, ressource ="prescription-pct")
  libelle_pct <- unique(prescription_pct$libelle)

  libelle <- c(libelle_surf, libelle_lin, libelle_pct)
  
  return(libelle)
  
}



prescription.geometrie <- function(commune, libelle){
  
  print(libelle)
  
  partition <- commune.to.partition(commune)
  
  prescriptions_2 <- get_apicarto_gpu(partition, ressource = c("prescription-surf","prescription-pct","prescription-lin"))
  
  # Chercher le libelle demander dans la fonction dans la colonne "libelle" de chaque DataFrame
  resultats <- lapply(
    prescriptions_2,
    function(df) df[grepl(libelle,df$libelle), ])
  
  # filtrer uniquement le Df non vide
  resultats_non_vides <- Filter(function(x) nrow(x) > 0, resultats)
  
  # transformation de la liste en  df
  resultats_df <- as.data.frame(resultats_non_vides)
  resultats_sf <- st_as_sf(resultats_df)  # transformation en sf, je pense que c'est du WGS84
  
  return(resultats_sf)
}



libelle <- libelle.prescription("29112")
libelle

EBC <- prescription.geometrie("29112",libelle[[3]])

qtm(EBC)








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

prescriptions_2 <- get_apicarto_gpu("DU_56031", ressource = c("assiette-sup-s"))

prescriptions_2 <- get_apicarto_gpu("130002926_SUP_95_A2", ressource = c("generateur-sup-s", "generateur-sup-l", "generateur-sup-p","acte-sup","assiette-sup-s", "assiette-sup-l", "assiette-sup-p"))

ebc <- prescriptions_2[prescriptions_2$libelle == "EBC",]

align_arbre <- prescriptions_2[prescriptions_2$libelle == "Alignement d'arbre", ]

com <- get_apicarto_cadastre("93014", "commune")

qtm(prescriptions_2)



