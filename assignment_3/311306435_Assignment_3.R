# Assignment 3
# Name: Yuval Roditi  
# I.D. Number: 311306435

#Q1
a3q1 <- function(x, n, p0, p1, N){
  Y <- rbinom(N, n, p1)
  phi <- dbinom(Y, n, p0)
  psi <- dbinom(Y, n, p1)
  single_est <- function(a){
    indicator <- (Y>a)
    return (mean(indicator*phi/psi))
  }
  return(sapply(x, single_est))
}


# Q2
a3q2 <- function(f, gen.X, g, m, N){
  input <- gen.X(N)
  X <- f(input)
  Y <- g(input)
  c <- (-cov(X,Y)/var(Y))
  Z <- mean(X + c*(Y-m))
  return(Z)
}

# Q3
a3q3 <- function(X, m, B){
  mu_hat <- mean(X)
  sd_hat <- sd(X)
  n <- length(X)
  scan <- function(X, m){
    n <- length(X)
    sums <- numeric()
    for (i in 1:(n-m+1))  {
      sums <- append(sums, sum(X[i:(i+m-1)]))
    }
    return(max(sums))
  }
  M_hat <- scan(X, m)
  M_star<-replicate(B, scan(rnorm(n, mu_hat, sd_hat), m))
  p_val <- sum(M_star > M_hat)/B
  
  return(list(p.value=p_val,statistic=M_hat))
}