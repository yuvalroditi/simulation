# Assignment 4
# Name: 
# I.D. Number:

# Q1: Replace "return(NA)" by your code
a4q1 <- function(x, f.alpha, f.lambda, start, N){
  n <- length(x)
  alpha <- start[1] # alpha_0
  lambda <- start[2] # lambda_0
  theta <- matrix(nrow=(N+1), ncol=2)
  theta[1,1] <- alpha
  theta[1,2] <- lambda
  # likelihood <- function(al, lm, x){
  #   return ((al^n)*(lm^(n*al))*prod(x^(al-1))*exp(-lm*sum(x^al)))
  # }
  ll <- function(al, lm, x){
    return (n*log(al) + (n*al)*log(lm) + (al-1)*sum(log(x)) - (lm^al)*sum(x^al))
  }
  ratio_func <- function(al, lm, al_til, lm_til, x){
    ll_theta_tilde <- ll(al_til, lm_til, x)
    ll_theta <- ll(al, lm, x)
    ll_ratio <- ll_theta_tilde - ll_theta
    ratio <- (f.alpha (al_til) *f.lambda(lm_til) * exp(ll_ratio))/(f.alpha(al)*f.lambda(lm))
    return (ratio)
  }
  for (i in 2:(N+1)){
    alpha.tilde <- rexp(1, 1/alpha)
    lambda.tilde <- rexp(1, 1/lambda)
    ratio <- min(ratio_func(alpha, lambda, alpha.tilde, lambda.tilde, x), 1)
    U <- runif(1)
    if (U <= ratio){
      alpha <- alpha.tilde
      lambda <- lambda.tilde
    }
    theta[i, 1] <- alpha
    theta[i, 2] <- lambda
  }
  return(theta)
}

# Q2: Replace "return(NA)" by your code
a4q2 <- function(M, beta, Y, sigma, burn.in){
  return(NA)
}

# Q3: Replace "return(NA)" by your code
a4q3 <- function(y, x, theta, param, n.sweep){
  return(NA)
}