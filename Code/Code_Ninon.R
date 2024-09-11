# fonction qui donne les differents types des assiettes ou generateurs des sup
type.sup <- function(sup){
  if (!is.null(sup)){
    info_sup <- unique(sup$type)
  }else {
    info_sup <- c()
  }
  return(info_sup)
}

# fonction qui renvoie une liste des types voulus
select.type.sup <- function(sup){
  info_sup <- type.sup(sup)
  print(info_sup)
  entree <- readline(prompt = "Veuillez entrer une liste des numéros de ligne des types voulus séparés par des virgules (ex : 1,2,3) : ")
}
  #  Diviser la chaîne en éléments en utilisant la virgule comme séparateur
  elements <- strsplit(entrée, split = ",")[[1]] 
  # Convertir les éléments en nombre si nécessaire
  numeros <- as.numeric(elements)
  

# fonction qui renvoie seulement les types voulus par l utilisateur
# numeros_lignes_types est une liste contenant les numéros des lignes 
info_sup <- 
filter.type.sup <-function(sup, numeros_lignes_types){ 
  sup <- filter(sup,
                sup$type %in% )
  