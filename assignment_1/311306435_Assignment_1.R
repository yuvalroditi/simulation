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


# Q2: Replace "return(NA)" by your code
a1q2 <- function(n, shape){
  r <- shape
  m <- floor(r)
  lambda <- m/r
  reject <- 0
  p <- function(x){return((x*(1-lambda)/(r-m))^(r-m)*exp(-(1-lambda)*x+(r-m)))}
  counter <- 0
  X <- c()
  U <- c()
  while (counter < n){
    x <- 0
    for (i in 1:m){
      x <- x + rexp(1, lambda)
    }
    u <- runif(1, 0, 1)
    #cat("u:", u, "x:", x, "p", p)
    
    if (u <= p(x)){
        counter <- counter + 1
        X <- append(X, x)
        U <- append(U, u)
    } else{
      reject <- reject + 1
    }
  }
  cat("accept:", counter, "reject:", reject)
  return(data.frame(X,U))
}




# Q3: Replace "return(NA)" by your code
a1q3 <- function(n){
  counter <- 0
  reject <- 0
  X <- c()
  U1 <- c()
  U2 <- c()
  m <- 1
  a <- -sqrt(2/exp(1))
  b <- sqrt(2/exp(1))
  while (counter < n){
    u1 <- runif(1, 0, m)
    u2 <- runif(1, a, b)
    if (u1^2 <= exp(-0.5*(u2^2)/(u1^2))){
      counter <- counter + 1
      X <- append(X, u2/u1)
      U1 <- append(U1, u1)
      U2 <- append(U2, u2)
    } else{
      reject <- reject + 1
    }
  }
  cat("accept:", counter, "reject:", reject)
  return(data.frame(X, U1, U2))
}