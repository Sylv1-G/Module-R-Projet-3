library(librarian)
shelf(happign,mapedit,tmap)
tmap_options(check.and.fix = T)

x <- mapedit::drawFeatures()

sup_utiles = list("a1","a7","a8","el9","a4","as1","ac3","el10","a9","a10",
                  "ac1","ac4","ac2")
col_utiles_gen = c("gid","suptype","partition","fichier","nomgen","typegen",
                   "nomsuplitt","geometry")
col_utiles_ass = c("gid","suptype","partition","fichier","nomass","typeass",
                   "nomsuplitt","geometry")
noms_def <- c("gid","suptype","partition","fichier","nom","type",
              "nomsuplitt","geometry")

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
  
  colnames(generateur) <- c("gid","suptype","partition","fichier","nomass",
                            "typeass","nomsuplitt","geometry")
  
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
  
  colnames(assiette) <- c("gid","suptype","partition","fichier","nomass",
                          "typeass","nomsuplitt","geometry")

  
  return(assiette)
  
}

