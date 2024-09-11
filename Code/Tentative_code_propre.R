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

code_prescription <- c("01", "07", "18", "19", "25", "31", "34", "35",
                       "43", "46", "99")
libelle_prescription <- c(
  "Espace boisé classé",
  "Patrimoine bâti, paysager ou éléments de paysages à protéger",
  "Périmètre comportant des orientations d’aménagement et deprogrammation (OAP)",
  "Secteur protégé en raison de la richesse du sol et du sous-sol",
  "Eléments de continuité écologique et trame verte et bleue",
  "Espaces remarquables du littoral",
  "Espaces, paysage et milieux caractéristiques du patrimoine naturel et culturel montagnard à préserver",
  "Terres nécessaires au maintien et au développement des activités agricoles, pastorales et forestières à préserver",
  "Réalisation d’espaces libres, plantations, aires de jeux et de loisir",
  "Constructibilité espace boisé antérieur au 20ème siècle")

code_prescription_patrimonial <- c("01", "07", "18", "31", "34", "35", "43",
                                   "46", "99")
libelle_prescription_patrimonial <- c(
  "Espace boisé classé",
  "Patrimoine bâti, paysager ou éléments de paysages à protéger",
  "Périmètre comportant des orientations d’aménagement et deprogrammation (OAP)",
  "Espaces remarquables du littoral",
  "Espaces, paysage et milieux caractéristiques du patrimoine naturel et culturel montagnard à préserver",
  "Terres nécessaires au maintien et au développement des activités agricoles, pastorales et forestières à préserver",
  "Réalisation d’espaces libres, plantations, aires de jeux et de loisir",
  "Constructibilité espace boisé antérieur au 20ème siècle")


code_prescription_ecologique <- c("01","18", "25", "34", "43", "99")
libelle_prescription_ecologique <- c(
  "Espace boisé classé",
  "Périmètre comportant des orientations d’aménagement et deprogrammation (OAP)",
  "Eléments de continuité écologique et trame verte et bleue",
  "Espaces, paysage et milieux caractéristiques du patrimoine naturel et culturel montagnard à préserver",
  "Réalisation d’espaces libres, plantations, aires de jeux et de loisir")


code_info <- c("03", "08", "16", "21", "22","25", "37", "40")
libelle_info <- c(
  "Zone de préemption dans un espace naturel et sensible",
  "Périmètre forestier : interdiction ou réglementation des plantations (code rural et de la pêche maritime), plantations à réaliser et semis d'essence forestière",
  "Site archéologique",
  "Projet de plan de prévention des risques",
  "Protection des rives des plans d'eau en zone de montagne",
  "Périmètre de protection des espaces agricoles et naturels périurbain",
  "Bois ou forêts relevant du régime forestier",
  "Périmètre d’un bien inscrit au patrimoine mondial ou Zone tampon d’un bien inscrit au patrimoine mondial")

code_info_patrimonial <- c("16", "25", "40")
libelle_info_patrimonial <- c(
  "Site archéologique",
  "Périmètre de protection des espaces agricoles et naturels périurbain",
  "Périmètre d’un bien inscrit au patrimoine mondial ou Zone tampon d’un bien inscrit au patrimoine mondial")

code_info_ecologique <- c("03", "22")
libelle_info_ecologique <- c(
  "Zone de préemption dans un espace naturel et sensible",
  "Protection des rives des plans d'eau en zone de montagne")
                  

sup_utiles <- c("a1","a7","a8","el9","a4","as1","ac3","el10","a10",
                "ac1","ac4","ac2","pm1","el2","pm2","pm4","pm5",
                "pm6","pm7","pm8","pm9")
libelle_sup <- c(
  "Serviture de protection des bois et forêts relevant du régime forestier à Mayotte",
  "Servitude relative aux forêts dites de protection",
  "Servitures résultant de la mise en défens des terrains et pâturages en montagnes et dunes du Pas-de-Calais",
  "Servitudes de passage sur le littoral",
  "Servitudes de passage dans le lit ou sur les berges d'un cours d'eau",
  "Servitudes résultant de l'instauration de périmètres de protection autour des captaux d'eaux et des sources minérales naturelles",
  "Réserves naturelles et périmètres de protection autour des réserves naturelles",
  "Coeur de parc national",
  "Zones de protection naturelle, agricole et forestière du plateau de Saclay",
  "Servitudes relatives aux monuments historiques",
  "Sites patrimoniaux remarquables, zones de protection et de valorisation du patrimoine architectural, urbain et paysager",
  "Servitudes relatives aux sites inscrits et classés",
  "Plans de prévention des risques naturels prévisibles (PPRNP) et plans de prévention de risques miniers (PPRM) et documents valant PPRNP",
  "Servitude qui concerne la Loire et ses affluents",
  "Servitudes d'inondation pour la rétention des crues du Rhin",
  "Servitudes autour des installations classées pour la protection de l’environnement et sur des sites pollués, de stockage de déchets ou d’anciennes carrières",
  "Servitude relative aux zones de rétention d’eau et aux zones dites 'stratégiques pour la gestion de l’eau'",
  "Servitudes visant à ne pas aggraver les risques pour la sécurité publique en présence d’un ouvrages hydraulique",
  "Servitudes autour des installations nucléaires de base",
  "Servitudes relatives aux ouvrages ou infrastructures permettant de prévenir les inondations ou les submersions",
  "Servitudes relatives à la création, la continuité,la pérennité et l’entretien des équipements de défense des forêts contre les incendies (DFCI)",
  "Servitudes relatives aux zones de danger")

sup_patrimonial <- c("a10","ac1","ac4","ac2")

libelle_sup_patrimonial <- c(
  "Zones de protection naturelle, agricole et forestière du plateau de Saclay",
   "Servitudes relatives aux monuments historiques",
   "Sites patrimoniaux remarquables, zones de protection et de valorisation du patrimoine architectural, urbain et paysager",
   "Servitudes relatives aux sites inscrits et classés")

sup_ecologique <- c("a8","a4","as1","ac3","el10","a10")
libelle_ecologique <- c(
  "Servitures résultant de la mise en défens des terrains et pâturages en montagnes et dunes du Pas-de-Calais",
   "Servitudes de passage dans le lit ou sur les berges d'un cours d'eau",
   "Servitudes résultant de l'instauration de périmètres de protection autour des captaux d'eaux et des sources minérales naturelles",
   "Réserves naturelles et périmètres de protection autour des réserves naturelles",
   "Coeur de parc national",
   "Zones de protection naturelle, agricole et forestière du plateau de Saclay")



col_utiles_gen <- c("gid","suptype","partition","fichier","nomgen","typegen",
                   "nomsuplitt","geometry")
col_utiles_ass <- c("gid","suptype","partition","fichier","nomass","typeass",
                   "nomsuplitt","geometry")
noms_def <- c("gid","suptype","partition","fichier","nom","libelle",
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
  
  
  if(!is.null(generateur)){
    colnames(generateur) <- noms_def
  }
  
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
  
  if(!is.null(assiette)){
    colnames(assiette) <- noms_def
  }
  
  return(assiette)
  
}


get.gpu.all <- function(x){
  
  prescription <- get.gpu.prescription(x)
  print("prescription ok")
  info <- get.gpu.info(x)
  print("info ok")
  sup_gen <- get.sup.gen(x)
  print("SUP generateur ok")
  sup_ass <- get.sup.ass(x)
  print("SUP assiette ok")
  
  all_gpu <- list(prescription,info,sup_gen,sup_ass)
  
  return(all_gpu)
}

zonne <- mapedit::drawFeatures()
x <- zonne

tes_all_gpu <- get.gpu.all(zonne)
