# fonction qui donne les differents type
filter.sup <- function(generateur, assiette){
  if (!is.null(generateur)){
    generateur <- filter(generateur,
                  typeinf %in% code_info)
    info_generateur <- unique(generateur$typeass)
  }else {
    info_generateur <- c()
  }
  return(info_generateur)
    
  }