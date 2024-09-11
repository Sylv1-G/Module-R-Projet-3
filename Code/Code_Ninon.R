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
  print("les types présents sont: ")
  print (info_sup)
  entree <- readline(prompt = "Veuillez entrer une liste des numéros de ligne des types voulus séparés par des virgules (ex : 1,2,3) : ")
  #  Diviser la chaîne en éléments en utilisant la virgule comme séparateur
  elements <- strsplit(entree, split = ",")[[1]] 
  # Convertir les éléments en nombre si nécessaire
  numeros <- as.numeric(elements)
  list_type <- list()
  for (i in numeros)
    list_type <- append(list_type, info_sup[numeros[i]])
  return (list_type)
}


# fonction qui renvoie seulement les types voulus par l utilisateur
#filter.type.sup <-function(sup, list_type){ 
#sup <- filter(sup,
#sup$type %in% list_type)
