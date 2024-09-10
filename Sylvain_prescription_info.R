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
libelle_prescription <- c("Espace boisé",
                          "Patrimoine bâti, paysager ou éléments de paysages à protéger",
                          "Périmètre comportant des orientations d’aménagement et deprogrammation (OAP)",
                          "Secteur protégé en raison de la richesse du sol et du sous-sol",
                          "Eléments de continuité écologique et trame verte et bleue",
                          "Espaces remarquables du littoral",
                          "Espaces, paysage et milieux caractéristiques du patrimoine naturel et culturel montagnard à préserver",
                          "Terres nécessaires au maintien et au développement des activités agricoles, pastorales et forestières à préserver",
                          "Réalisation d’espaces libres, plantations, aires de jeux et de loisir")

code_info <- c("03", "08", "16", "21", "22", "37", "40")
libelle_info <- c("Zone de préemption dans un espace naturel et sensible",
                  "Périmètre forestier : interdiction ou réglementation des plantations (code rural et de la pêche maritime), plantations à réaliser et semis d'essence forestière",
                  "Site archéologique",
                  "Projet de plan de prévention des risques",
                  "Protection des rives des plans d'eau en zone de montagne",
                  "Bois ou forêts relevant du régime forestier",
                  "Périmètre d’un bien inscrit au patrimoine mondial ou Zone tampon d’un bien inscrit au patrimoine mondial")


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
      filter(grid_title == row$name) |>
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




prescription.libelle.filtre <- function(partition, ressource){
  # récupère toutes les préscriptions surfacique
  prescription_type <- get_apicarto_gpu(partition,  
                                        ressource = ressource)
  # filtre les préscriptions "jugé" interresante pour de la gf
  if (!is.null(prescription_type)){
    prescription_type <- filter(prescription_type,
                                typepsc %in% code_prescription)
    prescription_libelle <- unique(prescription_type$libelle)
  }else {
    prescription_libelle <- c()
  }
  return(prescription_libelle)
}

libelle.prescription <- function(commune){
  
  partition <- commune.to.partition(commune)
  
  libelle_surf <- prescription.libelle.filtre(partition,"prescription-surf")
  libelle_lin <- prescription.libelle.filtre(partition,"prescription-lin")
  libelle_pct <- prescription.libelle.filtre(partition,"prescription-pct")
  
  
  libelle <- c(libelle_surf, libelle_lin, libelle_pct)
  
  return(libelle)
  
}

info.libelle.filtre <- function(partition, ressource){
  
  # récupère les informations 
  info_type <- get_apicarto_gpu(partition,  
                                ressource = ressource)
  # filtre les informations "jugé" interresante pour de la gf
  if (!is.null(info_type)){
    info_type <- filter(info_type,
                        typeinf %in% code_info)
    info_libelle <- unique(info_type$libelle)
  }else {
    info_libelle <- c()
  }
  return(info_libelle)
  
}

libelle.info <- function(commune){
  
  partition <- commune.to.partition(commune)
  
  info_surf <- info.libelle.filtre(partition,"info-surf")
  info_lin <- info.libelle.filtre(partition,"info-lin")
  info_pct <- info.libelle.filtre(partition,"info-pct")
  
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



prescription_libelle <- libelle.prescription("29112")
prescription_libelle


info_libelle <- libelle.info("56031")

EBC <- prescription.geometrie("29112",libelle[[3]])

qtm(EBC)


