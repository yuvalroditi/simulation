# Assignment 1
# Name: 
# I.D. Number:

# Q1: Replace "return(NA)" by your code
a1q1 <- function(n, j, k, m, seed){
  X_k <- numeric(length=k)
  X <- numeric(length=n)
  X_k[1] <- seed
  for (i in 1:(k-1)){
    X_k[i+1] <- (1103515245 * X_k[i] + 12345) %% 2^32
  }
  X[1:k] = X_k
  for (i in (k+1):(n+k+1)){
    X[i] <- (X[i-j] + X[i-k]) %% m
  }
  return (X[(k+2):(n+k+1)])
}


a1q2 <- function(n, shape){
  extra_factor <- 1.2
  r <- shape
  m <- floor(r)
  lambda <- m/r
  p <- function(X){return((X*(1-lambda)/(r-m))^(r-m)*exp(-(1-lambda)*X+(r-m)))}
  p <- function(x){return((x*(1-lambda)/(r-m))^(r-m)*exp(-(1-lambda)*x+(r-m)))}
  create_sums <- function(m, lambda){
    return (sum(rexp(m, lambda)))
  }
  X_prop <-replicate(n*extra_factor, sum(rexp(m, lambda)))
  U <- runif(n*extra_factor, 0, 1)
  X <- X_prop[U <= p(X_prop)]
  counter <- length(X)
  U <- U[U <= p(X_prop)]
  X <- X[1:min(n, counter)]
  U <- U[1:min(n, counter)]
  while (counter < n){
    x <- sum(rexp(m, lambda))
    u <- runif(1, 0, 1)
    if (u <= p(x)){
      counter <- counter + 1
      X <- append(X, x)
      U <- append(U, u)
    } 
  }
  return(data.frame(X, U))
}


# Q3: Replace "return(NA)" by your code
a1q3 <- function(n){
  m <- 1
  a <- -sqrt(2/exp(1))
  b <- sqrt(2/exp(1))
  factor <- 1.5
  U1_proposals <- runif(factor*n, 0, m)
  U2_proposals <- runif(factor*n, a, b)
  U2_U1 = U2_proposals/U1_proposals
  X <- U2_U1[U1_proposals^2 <= exp(-0.5*(U2_proposals^2)/(U1_proposals^2))]
  U1 <- U1_proposals[U1_proposals^2 <= exp(-0.5*(U2_proposals^2)/(U1_proposals^2))]
  U2 <- U2_proposals[U1_proposals^2 <= exp(-0.5*(U2_proposals^2)/(U1_proposals^2))]
  counter <- length(X)
  X <- X[1:min(n,counter)]
  U1 <- U1[1:min(n,counter)]
  U2 <- U2[1:min(n,counter)]
  print(counter)
  while (counter < n){
    u1 <- runif(1, 0, m)
    u2 <- runif(1, a, b)
    if (u1^2 <= exp(-0.5*(u2^2)/(u1^2))){
      counter <- counter + 1
      X <- append(X, u2/u1)
      U1 <- append(U1, u1)
      U2 <- append(U2, u2)
    }
  }
  return(data.frame(X, U1, U2))
}