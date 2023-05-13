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
set.seed(1)
a2q2 <- function(n, m, a, b, sd, sd.0){
  arma <-function(a, b){
    Y0 = rnorm(1, 0, sd.0)
    eps <- rnorm(m+1, 0, sd)
    eps <- filter(eps, c(1,b), sides=1L)
    eps <- eps[2:(m+1)]
    Y <- filter(eps, a, method = "recursive", init=Y0)
    return(Y)
  }
  Y_mat <- t(replicate(n, arma(a,b)))
  return(Y_mat)
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