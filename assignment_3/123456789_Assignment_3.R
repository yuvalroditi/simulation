# Assignment 3
# Name: 
# I.D. Number:

# Q1: Replace "return(NA)" by your code
a3q1 <- function(x, n, p0, p1, N){
  Y <- rbinom(N, n, p1)
  single_est <- function(a){
    indicator <- (Y>a)
    phi <- dbinom(Y, n, p0)
    psi <- dbinom(Y, n, p1)
    return (mean(indicator*phi/psi))
  }
  # vec_est <-numeric(length(x))
  # for (i in 1:length(x)){
  #  vec_est[i] <- single_est(x[i])
  # }
  return(sapply(x, single_est))
}


# Q2: Replace "return(NA)" by your code
a3q2 <- function(f, gen.X, g, m, N){
  return(NA)
}

# Q3: Replace "return(NA)" by your code
a3q3 <- function(X, m, B){
  return(NA)
}