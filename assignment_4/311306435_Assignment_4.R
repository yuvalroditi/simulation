# Assignment 4
# Name: Yuval Roditi
# I.D. Number: 311306435

# Q1
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
    # compute the likelihood of the proposal
    ll_theta_tilde <- ll(alpha.tilde, lambda.tilde)
    # compute the likelihood of the current variables
    ll_theta <- ll(alpha, lambda)
    # compute the ratio of the likelihoods
    ll_ratio <- ll_theta_tilde - ll_theta
    # The transition from current variable to the proposal is not symmetric so
    # it is needed also to add the transition probability:
    tran_al_al_til = dexp(alpha.tilde, rate = (1/alpha))
    tran_al_til_al = dexp(alpha, rate = (1/alpha.tilde))
    tran_lm_lm_til = dexp(lambda.tilde, rate = (1/lambda))
    tran_lm_til_lm = dexp(lambda, rate = (1/lambda.tilde))
    # compute the full numerator and denumerator of the ratio:
    numerator <- f.alpha (alpha.tilde) *f.lambda(lambda.tilde) * exp(ll_ratio)* tran_al_til_al * tran_lm_til_lm
    denumerator <- f.alpha(alpha) * f.lambda(lambda) * tran_al_al_til * tran_lm_lm_til  
    # take care of zero (can happen because of rate 0 or very small for exp distribution)
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

# Q2
a4q2 <- function(M, beta, Y, sigma, burn.in=100){
  dim_x <- dim(Y)[1]
  dim_y <- dim(Y)[2]
  dim_z <- dim(Y)[3]
  if (is.na(dim_z)){
    dim_z <- 1
    Y <- array(Y,dim=c(dim_x, dim_y, dim_z))
  }
  X <- array(0, c(dim_x + 2, dim_y + 2, dim_z + 2))
  pixels <- sample(c(-1,1), size=dim_x*dim_y*dim_z, replace=TRUE)
  X[2:(dim_x+1), 2:(dim_y+1), 2:(dim_z+1)] <- array(pixels, dim=c(dim_x,dim_y,dim_z))
  # p.table <- numeric(length=7)
  # for (d in seq(-6, 6, by=2)) {
  #   p.table[(d+8)/2] <- 1 / (1 + exp(-2 * beta * d))
  # }
  #go over all possible pixels
  count <- array(0, dim=c(dim_x,dim_y,dim_z))
  count <- count[,,1:dim_z]
  for (sweep in 1:(M + burn.in)) {
    for(iz in 2:(dim_z+1)) {
      for (ix in 2:(dim_x+1)) {
        for (iy in 2:(dim_y+1)){
          # compute d according to its nearest neighboors
          d <- X[ix-1, iy, iz] + X[ix+1, iy, iz] +
               X[ix, iy-1, iz] + X[ix, iy+1, iz] +
               X[ix, iy, iz-1] + X[ix, iy, iz+1]
          # compute probability of obtaining 1
          # p <- p.table[(d+8)/2]
          p <- 1 / (1 + exp(-2 * (beta * d + Y[ix-1, iy-1, iz-1]/sigma^2)))
          # replace the pixel with a simulation with p as probability for 1
          X[ix, iy, iz] <- sample(c(-1, +1), size=1, prob=c(1-p, p))
        }
      }
    }
    if (sweep > burn.in){
      count <- count + (X[2:(dim_x+1),2:(dim_y+1), 2:(dim_z+1)] == 1)
    }
  }
  return(count / M)
}


# Q3: Replace "return(NA)" by your code
a4q3 <- function(y, x, theta, param, n.sweep){
  n < length(x)
  mu_0 <- param[1]
  sigma_0 <- param[2]
  mu_1 <- param[3]
  sigma_1 <- param[4]
  r <- param[5]
  lambda <- param[6]
  b0 <- theta[1]
  b1 <- theta[2]
  v <- theta[3]
  theta <- data.frame(matrix(nrow=(n.sweep+1), ncol=3))
  theta[1,1] <- b0
  theta[1,2] <- b1
  theta[1,3] <- v

  rand_b0 <- function(b1, v) {
    b_0_mean <- (mu_0/sigma_0 + sum(y-b1*x)/v)/(1/sigma_0 + n/v)
    b_0_var <- 1/(1/sigma_0+n/v)
    return(rnorm(1, b_0_mean, sqrt(b_0_var)))
  }

  rand_b1 <- function(b0, v){
    b_1_mean <- (mu_1/sigma_1+sum(x*(y-b0))/v)/(1/sigma_1+sum(x^2/v))
    b_1_var <- 1/(1/sigma_1 + sum(x^2/v))
    return(rnorm(1, b_1_mean, sqrt(b_1_var)))
  }

  rand_v <- function(b0, b1){
    return(1/rgamma(1, shape=(r+n/2), rate=(lambda+sum((y-b0-b1*x)^2/2))))
  }
  for (i in 2:(n.sweep+1)) {
    
    theta[i,1] <- rand_b0(theta[i-1,2], theta[i-1,3])
    
    theta[i,2] <- rand_b1(theta[i,1], theta[i-1,3])
    
    theta[i,3] <- rand_v(theta[i,1], theta[i,2])
    
  }
  return(data.frame(theta))
}
