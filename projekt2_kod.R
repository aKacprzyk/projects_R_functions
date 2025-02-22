#Aleksandra Kacprzyk 114125

flatten <- function(a){
  l <- list()
  for (i in 1:length(a)){
    if (inherits(a[[i]], "list")){
      nested <- flatten(a[[i]])
      l <- c(l, nested)
    }else{
      l[[names(a)[i]]] <- a[[i]]
    }
  }
  return(l)
}