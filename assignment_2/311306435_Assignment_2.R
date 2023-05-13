# Assignment 2
# Name: Yuval Roditi
# I.D. Number: 311306435

# Q1
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


# Q2
a2q2 <- function(n, m, a, b, sd, sd.0){
  Y0 <- rnorm(n, 0, sd.0)
  Y <- cbind(Y0)
  eps <- matrix(rnorm(n * (m+1), 0, sd), nrow=n)
  for (i in 1:m){
    Y <- cbind(Y, a*Y[1:n,i] + b*eps[1:n,i] + eps[1:n,i+1])
  }
  return(Y[1:n, 1:m+1])
}

# Q3
a2q3 <- function(n, rN, rX){
  N <- rN(n)
  S <- numeric(n)
  for (i in 1:n){
    X <- rX(N[i])
    S[i] <- sum(X)
  }
  return(S)
}