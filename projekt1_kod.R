#Aleksandra Kacprzyk 114125

findComponents <- function(a) {
  n <- 0
  visited <- matrix(FALSE, nrow(a), ncol(a))
  l <- NULL

  research <- function(i, j, n) {
    if (i < 1 || i > nrow(a) || j < 1 || j > ncol(a) || a[i, j] == 0 || visited[i, j]) {
      return()
    }
    visited[i, j] <<- TRUE
    l <<- rbind(l, c(i, j, n))
    
    research(i - 1, j, n)
    research(i + 1, j, n)
    research(i, j - 1, n)
    research(i, j + 1, n)
  }
  
  for (j in 1:ncol(a)) {
    for (i in 1:nrow(a)) {
      if (a[i, j] == 1 && !visited[i, j]) {
        n <- n + 1
        research(i, j, n)
      }
    }
  }
  results <- as.data.frame(l)
  colnames(results) <- c("x", "y", "cluster")
  return(results)
}