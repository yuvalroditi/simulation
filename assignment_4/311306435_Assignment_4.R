# Assignment 4
# Name: Yuval
# I.D. Number: 311306435

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

# Q2: Replace "return(NA)" by your code
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


L <- 51
X <- matrix(-1,L,L)
ij <- expand.grid(seq(L)-25,seq(L)-25)
X[(rowSums(ij^2) <= 15^2) & 
    (rowSums(ij^2) >= 10^2)] <- 1
image(1:L, 1:L, X)

# Add noise
sigma <- 2
Y <- X + rnorm(prod(dim(X)), sd = sigma)
image(1:L, 1:L, Y)




# De-noise the image
beta <- 0.5
M <- 1e2
prob <- a4q2(M, beta, Y, sigma)
prob <- RB(M, beta, Y, sigma)
image(1:L, 1:L, Y)
image(1:L, 1:L, prob)


# Q3: Replace "return(NA)" by your code
a4q3 <- function(y, x, theta, param, n.sweep){
  return(NA)
}