# ------------------------------------------------------------------------------
# Functions come from:
#
# Dunn, Peter K., and Gordon K. Smyth. “Chapter 4: Beyond Linear Regression: 
# The Method of Maximum Likelihood.” In Generalized Linear Models With Examples 
# in R, edited by Peter K. Dunn and Gordon K. Smyth, 165–209. 
# Springer Texts in Statistics. New York, NY: Springer, 2018. 
# https://doi.org/10.1007/978-1-4419-0118-7_4.
# ------------------------------------------------------------------------------
data("quilpie", package = "GLMsData")

# Global ####
# For test purpose
X <- quilpie[, c("SOI"), drop = F]
# X$intercept <- 1
X <- X |> as.matrix()
Y <- quilpie[, "y", drop = F] |> as.matrix()

# Function to compute the information matrix ####
MakeExpInf <- function(x, mu){
  if(length(mu) == 1) mu <- rep(mu, dim(x)[1])
  mu <- as.vector(mu)
  return( t(x) %*% diag( mu * (1 - mu) ) %*% x )
}

# Function to compute mu ####
MakeMu <- function(x, beta){
  eta <- x %*% beta
  return( 1 / ( 1 + exp( -eta ) ) )
}

# Function for computing the score vector
MakeScore <- function(x, y, beta){
  mu <- MakeMu(x, beta)
  return( t(x) %*% (y - mu) )
}

FitModelMle <- function(y, x=NULL, maxits=8, add.constant=TRUE){
  if(is.null(x)){
    allx <- cbind( Constant = rep( 1, length(y) ) )
  } else{
    allx <- x
    if(add.constant){
      allx <- cbind( Constant = rep( 1, length(y) ), x)
    }
  }
  num.x.vars <- dim(allx)[2] - 1
  beta <- c(mean(y), rep(0, num.x.vars))
  
  beta.vec <- array(dim = c(maxits, length(beta)))
  beta.vec[1, ] <- beta
  mu <- MakeMu(allx, beta)
  score.vec <- MakeScore(allx, y, beta)
  inf.mat <- MakeExpInf(allx, mu)
  
  for(i in 2:maxits){
    beta <- beta + solve(inf.mat) %*% score.vec
    beta.vec[i, ] <- beta
    mu <- MakeMu(allx, beta)
    score.vec <- MakeScore(allx, y, beta)
    inf.mat <- MakeExpInf(allx, mu)
  }
  
  LLH <- sum(y*log(mu) + (1-y)*log(1-mu))
  
  return(list(
    coef.vec = beta.vec,
    coef = beta.vec[maxits, ],
    LLH = LLH,
    inf.mat = inf.mat,
    score.vec = score.vec,
    mu = mu
  ))
  
}


MakeMu(X, beta = c(0, 0))
MakeScore(X, Y, c(0, 0))
obj <- FitModelMle(Y, X)
obj$score.vec
obj$LLH
obj$coef.vec
obj$coef