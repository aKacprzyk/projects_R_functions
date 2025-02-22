#Aleksandra Kacprzyk 114125

duopolySim <- function(price, q0, cost1, cost2, learn1, learn2, n){
  results <- data.frame(q1 = numeric(n + 1), q2 = numeric(n + 1))  
  results$q1[1] <- q0[1]  
  results$q2[1] <- q0[2] 
  for (t in 1:n){
    q0 <- c(results$q1[t], results$q2[t])  
    q1 <- learn1(price, cost1, q0)  
    q2 <- learn2(price, cost2, q0)  
    results$q1[t + 1] <- q1  
    results$q2[t + 1] <- q2
  }
  return(results[-1, ]) 
}
