
# fonction qui donne les differents libelles des document d'urbanisme
libelle.urba <- function(df){
  if (!is.null(df)){
    info_df <- unique(df$libelle)
  }else {
    info_df <- c()
  }
  return(info_df)
}

# fonction qui renvoie une liste des libelles voulus
select.libelle.urba <- function(df){
  info_df <- libelle.urba(df)
  cat("\nles libelles présents sont: \n\n")
  print (info_df)
  cat("\nVeuillez entrer une liste des numéros de ligne des libelles voulus séparés par des virgules \n(ex : 1,2,3) :")
  entree <- readline(prompt = "")
  #  Diviser la chaîne en éléments en utilisant la virgule comme séparateur
  elements <- strsplit(entree, split = ",")[[1]] 
  # Convertir les éléments en nombre si nécessaire
  numeros <- as.numeric(elements)
  liste_libelle <- info_df[numeros]
  print(liste_libelle)
  return(liste_libelle)
}


filtre.libelle.urba <- function(df){
  liste_libelle <- select.libelle.urba(df)
  
  df_filter <- dplyr::filter(df,
                             libelle %in% liste_libelle)
  
  return(df_filter)
}


exemple <- filtre.libelle.urba(prescription)
