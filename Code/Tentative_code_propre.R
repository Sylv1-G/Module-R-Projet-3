# A propos du code -----

# Titre du code : 
# But du code : 
# Auteur : 
# Contact :
# Dernière mise à jour :

# Installation des Packages ----

# permet de lancer la ligne de code suivante
#install.packages("rstudioapi","librarian")

# instalation des packages non instalé et chargement des packages
librarian::shelf(happign,dplyr,sf,tidyverse,tmap)
# parametrage de tmap
tmap_mode("view"); tmap_options(check.and.fix = TRUE)

# Repertoir de travail -----

# Répertoire de travail relatif a la source du fichier 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Nettoyage de l'environement R ----
rm(list=ls())

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

sup_utiles = list("a1","a7","a8","el9","a4","as1","ac3","el10","a9","a10",
                  "ac1","ac4","ac2")
col_utiles_gen = c("gid","suptype","partition","fichier","nomgen","typegen",
                   "nomsuplitt","geometry")
col_utiles_ass = c("gid","suptype","partition","fichier","nomass","typeass",
                   "nomsuplitt","geometry")
noms_def <- c("gid","suptype","partition","fichier","nom","type",
              "nomsuplitt","geometry")

# fonction ----


get.gpu.prescription <- function(x){
  
  prescription_surf <- get_apicarto_gpu(x,
                                        ressource = c("prescription-surf"))
  
  prescription_lin <- get_apicarto_gpu(x,
                                       ressource = c("prescription-lin"))
  
  prescription_pct <- get_apicarto_gpu(x,
                                       ressource = c("prescription-pct"))
  prescription_pct <- prescription_pct[ , !(names(prescription_pct) %in% "angle")]
  
  prescription <- rbind(prescription_surf, prescription_lin, prescription_pct)
  
  if (!is.null(prescription)){
    prescription <- filter(prescription, typepsc %in% code_prescription)
    
  }
  return(prescription)
  
}

get.gpu.info <- function(x){
  
  info_surf <- get_apicarto_gpu(x,
                                ressource = c("info-surf"))
  
  info_lin <- get_apicarto_gpu(x,
                               ressource = c("info-lin"))
  
  info_pct <- get_apicarto_gpu(x,
                               ressource = c("info-pct"))
  
  info <- rbind(info_surf, info_lin, info_pct)
  
  if (!is.null(info)){
    info <- filter(info, typeinf %in% code_info)
    
  }
  
  return(info)
  
}

get.sup.gen <- function(x){
  
  # Recuperation de toutes les informations utiles
  
  generateur_sup_s <- get_apicarto_gpu(x,
                                       ressource = "generateur-sup-s",
                                       dTolerance = 10,
                                       categorie = sup_utiles)
  
  
  generateur_sup_l <- get_apicarto_gpu(x,
                                       ressource = "generateur-sup-l",
                                       dTolerance = 10,
                                       categorie = sup_utiles)
  
  generateur_sup_p <- get_apicarto_gpu(x,
                                       ressource = "generateur-sup-p",
                                       dTolerance = 10,
                                       categorie = sup_utiles)  # aucune donnee
  
  # Rassemblement des donnees 
  generateur <- rbind(
    generateur_sup_s[ ,col_utiles_gen],
    generateur_sup_l[ ,col_utiles_gen],
    generateur_sup_p[ ,col_utiles_gen]
  )
  
  
  
  colnames(generateur) <- noms_def
  
  return(generateur)
  
}

get.sup.ass <- function(x){
  
  # Recuperation de toutes les informations utiles
  
  assiette_sup_s <- get_apicarto_gpu(x,
                                     ressource = "assiette-sup-s",
                                     dTolerance = 10,
                                     categorie = sup_utiles)
  
  
  assiette_sup_l <- get_apicarto_gpu(x,
                                     ressource = "assiette-sup-l",
                                     dTolerance = 10,
                                     categorie = sup_utiles)
  
  assiette_sup_p <- get_apicarto_gpu(x,
                                     ressource = "assiette-sup-p",
                                     dTolerance = 10,
                                     categorie = sup_utiles)
  
  assiette <- rbind(
    assiette_sup_s[ ,col_utiles_ass],
    assiette_sup_l[ ,col_utiles_ass],
    assiette_sup_p[ ,col_utiles_ass]
  )
  
  colnames(assiette) <- noms_def
  
  
  return(assiette)
  
}


get.gpu.all <- function(x){
  
  prescription <- get.gpu.prescription(x)
  print("1")
  info <- get.gpu.info(x)
  print("2")
  sup_gen <- get.sup.gen(x)
  print("3")
  sup_ass <- get.sup.ass(x)
  print("4")
  
  all_gpu <- list(prescription,info,sup_gen,sup_ass)
  
  return(all_gpu)
}

zonne <- mapedit::drawFeatures()
x <- zonne

tes_all_gpu <- get.gpu.all(zonne)
