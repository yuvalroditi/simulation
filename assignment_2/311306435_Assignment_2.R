# Assignment 2
# Name: Yuval Roditi
# I.D. Number: 311306435

# Q1: Replace "return(NA)" by your code
a2q1 <- function(n, mu, Sigma){
  p = length(mu)
  eig <- eigen(Sigma, symmetric = TRUE)
  eigen_values = eig$values
  eigen_vectrors = eig$vectors
  X <- matrix(rnorm(p*n), n)
  X <- eigen_vectrors %*% diag(sqrt(eigen_values)) %*% t(X) + drop(mu)
  if (n==1){
    return (drop(X))
  }
  else {
    return (t(X))
  }
}


# Q2: Replace "return(NA)" by your code
a2q2 <- function(n, m, a, b, sd, sd.0){
  return(NA)
}

# Q3: Replace "return(NA)" by your code
a2q3 <- function(n, rN, rX){
  return(NA)
}