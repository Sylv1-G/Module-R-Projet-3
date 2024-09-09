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

code_prescription <- c("01","07","18","19","25","31", "34", "35", "43")
libelle_prescription <- c(
  "Espace boisé",
  "Patrimoine bâti, paysager ou éléments de paysages à protéger",
  "Périmètre comportant des orientations d’aménagement et deprogrammation (OAP)",
  "Secteur protégé en raison de la richesse du sol et du sous-sol",
  "Eléments de continuité écologique et trame verte et bleue",
  "Espaces remarquables du littoral",
  "Espaces, paysage et milieux caractéristiques du patrimoine naturel et culturel montagnard à préserver",
  "Terres nécessaires au maintien et au développement des activités agricoles, pastorales et forestières à préserver",
  "Réalisation d’espaces libres, plantations, aires de jeux et de loisir")

prescription_interet <- data.frame(code_prescription = code_prescription, libelle_prescription = libelle_prescription)

code_info <- c("03", "08", "16", "21", "22", "37", "40")
libelle_info <- c(
  "Zone de préemption dans un espace naturel et sensible",
  "Périmètre forestier : interdiction ou réglementation des plantations (code rural et de la pêche maritime), plantations à réaliser et semis d'essence forestière",
  "Site archéologique",
  "Projet de plan de prévention des risques",
  "Protection des rives des plans d'eau en zone de montagne",
  "Bois ou forêts relevant du régime forestier",
  "Périmètre d’un bien inscrit au patrimoine mondial ou Zone tampon d’un bien inscrit au patrimoine mondial")


info_interet <- data.frame(code_info = code_info, libelle_info = libelle_info)

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
  

  cat("\nCommunes avec is_rnu == FALSE :\n")
  print(is_rnu_FALSE$name)
  
  nom_com <- is_rnu_FALSE$name

  
  
  # Téléchargements des documments d'urbanisme et de leur partition
  partitions <- c()
  
  for (i in 1:nrow(is_rnu_FALSE)) {
    
    row <- is_rnu_FALSE[i,]
    
    # Téléchargement du cadastre des communes sans rnu
    com <- get_apicarto_cadastre(row$insee, "commune")
    
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

# ressource = "prescription-surf", "prescription-lin","prescription-pct"

commune <- "56031"

ressource <- "prescription-surf"

libelle.prescription <- function(commune){
  
  partition <- commune.to.partition(commune)
  
  prescription.libelle.filtre <- function(partition, ressource){
    # récupère toutes les préscriptions surfacique
    prescription_type <- get_apicarto_gpu(partition,  
                                          ressource = ressource)
    # filtre les préscriptions "jugé" interresante pour de la gf
    if (!is.null(prescription_type)){
      prescription_type <- filter(prescription_type,
                                  typepsc %in% code_prescription)
      prescription_libelle <- unique(prescription_type$libelle)
      
    }
    return(prescription_libelle)
  }
    
  
  libelle_surf <- prescription.libelle.filtre(partition, "prescription-surf")
  libelle_lin <- prescription.libelle.filtre(partition, "prescription-lin")
  libelle_pct <- prescription.libelle.filtre(partition, "prescription-pct")

  libelle <- c(libelle_surf, libelle_lin, libelle_pct)
  
  return(libelle)
  
}


ressource <- "info-surf" 

libelle.info <- function(commune){
  
  partition <- commune.to.partition(commune)
  
  info.libelle.filtre <- function(partition, ressource){
    
    # récupère toutes les préscriptions surfacique

    info_type <- get_apicarto_gpu(partition,  
                                          ressource = ressource)
    # filtre les préscriptions "jugé" interresante pour de la gf
    if (!is.null(info_type)){
      info_type <- filter(info_type,
                                  typeinf %in% code_info)
      info_libelle <- unique(info_type$libelle)
      return(info_libelle)
    }
    
  }

  
  libelle_surf <- info.libelle.filtre(partition,"info-surf")
  
  libelle_lin <- info.libelle.filtre(partition,"info-lin")
  
  libelle_pct <- info.libelle.filtre(partition,"info-pct")
  
  if (!is.null(info_surf)){
    
    
  }
  
  
  libelle <- c(info_surf, info_lin, info_pct)
  
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



prescription <- libelle.prescription("29112")
prescription

info <- libelle.info("56031")

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

prescriptions_2 <- get_apicarto_gpu("DU_56031", ressource = c("info-surf"))

prescriptions_2 <- get_apicarto_gpu("130002926_SUP_95_A2", ressource = c("generateur-sup-s", "generateur-sup-l", "generateur-sup-p","acte-sup","assiette-sup-s", "assiette-sup-l", "assiette-sup-p"))

prescriptions_2 <- get_apicarto_gpu(grepl("_95_", liste), ressource = c("generateur-sup-s", "generateur-sup-l", "generateur-sup-p","acte-sup","assiette-sup-s", "assiette-sup-l", "assiette-sup-p"))


ebc <- prescriptions_2[prescriptions_2$libelle == "EBC",]

align_arbre <- prescriptions_2[prescriptions_2$libelle == "Alignement d'arbre", ]

com <- get_apicarto_cadastre("93014", "commune")

qtm(prescriptions_2)



