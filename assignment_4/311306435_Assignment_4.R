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
  # log likelihood:
  ll <- function(alpha, lambda){
    return (n*log(alpha) + (n*alpha)*log(lambda) + (alpha-1)*sum(log(x)) - (lambda)*sum(x^alpha))
  }
  ratio_func <- function(){
    ll_theta_tilde <- ll(alpha.tilde, lambda.tilde)
    ll_theta <- ll(alpha, lambda)
    ll_ratio <- ll_theta_tilde - ll_theta
    tran_al_al_til = dexp(alpha.tilde, rate = (1/alpha))
    tran_al_til_al = dexp(alpha, rate = (1/alpha.tilde))
    tran_lm_lm_til = dexp(lambda.tilde, rate = (1/lambda))
    tran_lm_til_lm = dexp(lambda, rate = (1/lambda.tilde))
    numerator <- f.alpha (alpha.tilde) *f.lambda(lambda.tilde) * exp(ll_ratio)* tran_al_til_al * tran_lm_til_lm
    denumerator <- f.alpha(alpha) * f.lambda(lambda) * tran_al_al_til * tran_lm_lm_til  
    if (is.nan(numerator)){
      ratio <- 1
    }
    else{
      ratio <- numerator/denumerator  
    }
    return (ratio)
  }
  for (i in 2:(N+1)){
    alpha.tilde <- rexp(1, 1/alpha)
    lambda.tilde <- rexp(1, 1/lambda)
    ratio <- min(ratio_func(), 1)
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