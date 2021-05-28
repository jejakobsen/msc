setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Transformation functions for parameters 

transform.params <- function(params){
  beta.0.t <- - log(params[1]); alpha.1.t <- - log((1/params[2])-1)
  theta.t <- - log((1/params[3]) - 1)
  return(c(beta.0.t, alpha.1.t, theta.t))
}

inv.transform.params <- function(params){
  beta.0 <- exp(-params[1]); alpha.1 <- 1/(1+exp(-params[2]))
  theta <- 1/(1+exp(-params[3]))
  return(c(beta.0, alpha.1, theta))
}

# Negative log-likelihood function

neg.loglik.gp.inarch <- function(p, data){
  beta.0 <- exp(-p[1]); alpha.1 <- 1/(1+exp(-p[2]));
  theta <- 1/(1+exp(-p[3]))
  
  Y <- data$battle_deaths
  N <- length(Y)
  nll <- 0 
  for (i in 2:N){
    lambda.i <- (beta.0+alpha.1*Y[i-1])*(1-theta)
    nll <- nll - log(lambda.i) + lgamma(Y[i]+1) - (Y[i]-1)*log(lambda.i+theta*Y[i]) + (lambda.i+theta*Y[i])
  }
  return(nll)
} 

# Fit countries 

source("fit_country.R")
set.seed(198)
col.nlm.obj <- fit_country("colombia", neg.loglik.gp.inarch,
                           transform.params, inv.transform.params, 
                           params=c(5.4, 0.45, 0.85), N.iter=20, sigma=0.1)
uga.nlm.obj <- fit_country("uganda", neg.loglik.gp.inarch,
                           transform.params, inv.transform.params, 
                           params=c(2.2, 0.5, 0.92), N.iter=20, sigma=0.1)
con.nlm.obj <- fit_country("congo", neg.loglik.gp.inarch,
                           transform.params, inv.transform.params, 
                           params=c(8.1, 0.4, 0.96), N.iter=20, sigma=0.1)
eth.nlm.obj <- fit_country("ethiopia", neg.loglik.gp.inarch,
                           transform.params, inv.transform.params, 
                           params=c(44, 0.17, 0.99), N.iter=20, sigma=0.1)

# Extract estimates

col.est <- inv.transform.params(col.nlm.obj$estimate)
uga.est <- inv.transform.params(uga.nlm.obj$estimate)
con.est <- inv.transform.params(con.nlm.obj$estimate)
eth.est <- inv.transform.params(eth.nlm.obj$estimate)

# Show rounded estimates in matrix 

est.matrix <- matrix(c(col.est, uga.est, con.est, eth.est),
                  nrow=4, ncol=3, byrow=T)
rownames(est.matrix) <- c("Colombia", "Uganda", "Congo", "Ethiopia")
colnames(est.matrix) <- c("beta", "alpha", "theta")
est.matrix[, 2:3] <- round(est.matrix[, 2:3], 3)
est.matrix[, 1] <- round(est.matrix[, 1], 1)
est.matrix
##################################

#          beta alpha theta
# Colombia  5.5 0.457 0.861
# Uganda    2.2 0.517 0.927
# Congo     8.2 0.406 0.962
# Ethiopia 44.7 0.171 0.992

#################################

# Compute, round and show AIC and BIC

source("aic_bic.R")

IC.matrix <- matrix(nrow=4, ncol=2)
rownames(IC.matrix) <- c("Colombia", "Uganda", "Congo", "Ethiopia")
colnames(IC.matrix) <- c("AIC", "BIC")

IC.matrix[, 1] <- c(AIC.normalized(-col.nlm.obj$minimum, 3, 1617, 1),
                    AIC.normalized(-uga.nlm.obj$minimum, 3, 1617, 1),
                    AIC.normalized(-con.nlm.obj$minimum, 3, 1617, 1),
                    AIC.normalized(-eth.nlm.obj$minimum, 3, 1617, 1))

IC.matrix[, 2] <- c(BIC.normalized(-col.nlm.obj$minimum, 3, 1617, 1),
                    BIC.normalized(-uga.nlm.obj$minimum, 3, 1617, 1),
                    BIC.normalized(-con.nlm.obj$minimum, 3, 1617, 1),
                    BIC.normalized(-eth.nlm.obj$minimum, 3, 1617, 1))
IC.matrix[, 1] <- round(IC.matrix[, 1])
IC.matrix[, 2] <- round(IC.matrix[, 2])
IC.matrix

#############################

#           AIC  BIC
# Colombia 9643 9659
# Uganda   4623 4639
# Congo    6397 6413
# Ethiopia 6586 6602

############################

# PMF of the Generalized Poisson

dgpois <- function(x, lambda, theta) 
{
  ll <- log(lambda) - lgamma(x+1) + (x-1)*log(lambda+theta*x) - (lambda+theta*x)
  return(exp(ll))
}

pgpois <- function(x, lambda, theta)
  # dont know if this is entirely correct... 
{
  if (length(x) == 1){
    x.seq <- seq(0, x, by=1)
    return(sum(dgpois(x.seq, lambda, theta)))
  }
  else {
    p.x <- rep(NA, length(x))
    if (length(lambda) == 1){
      for (i in 1:length(x)){
        x.seq <- seq(0, x[i], by=1)
        p.x[i] <- sum(dgpois(x.seq, lambda, theta))
      }
      return(p.x)
    }
    else{
      for (i in 1:length(x)){
        x.seq <- seq(0, x[i], by=1)
        p.x[i] <- sum(dgpois(x.seq, lambda[i], theta))
      }
      return(p.x)
    }
  }
}

# Compute Scores

source("rps.R")

countries <- c("colombia", "uganda", "congo", "ethiopia")
param.matrix <- matrix(c(col.est, uga.est, con.est, eth.est),
                       nrow=4, ncol=3, byrow=T)

rps.vec <- rep(NA, 4)
qs.vec <- rep(NA, 4)
for (i in 1:4){
  beta.0 <- param.matrix[i, 1]
  alpha <- param.matrix[i, 2]
  theta <- param.matrix[i, 3]
  
  country <- read.csv(paste0("../../data/", countries[i], ".csv"))
  
  Y <- country$battle_deaths[2:1617]
  params.1 <- (1-theta)*(beta.0 + alpha*country$battle_deaths[1:1616])
  param.2 <- theta
  
  rps.vec[i] <- rps(Y, params.1, param.2, dgpois)
  qs.vec[i] <- qs(Y, params.1, param.2, dgpois)
}

score.matrix <- matrix(c(rps.vec, qs.vec), nrow=4, ncol=2)
rownames(score.matrix) <- c("Colombia", "Uganda", "Congo", "Ethiopia")
colnames(score.matrix) <- c("RPS", "QS")
score.matrix[, 1] <- round(score.matrix[, 1], 2)
score.matrix[, 2] <- round(score.matrix[, 2], 4)
score.matrix

#################################

#            RPS      QS
# Colombia  6.79 -0.1775
# Uganda    4.00 -0.6322
# Congo    12.02 -0.4864
# Ethiopia 50.55 -0.4790

#################################
