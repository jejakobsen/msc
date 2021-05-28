setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Transformation functions for parameters 

transform.params <- function(params){
  beta.0.t <- - log(params[1]); alpha.1.t <- - log((1/params[2])-1)
  beta.1.t <- - log((1/params[3])-1); m.1.t <- - log(params[4])
  theta.t <- - log((1/params[5]) - 1)
  return(c(beta.0.t, alpha.1.t, beta.1.t, m.1.t, theta.t))
}

inv.transform.params <- function(params){
  beta.0 <- exp(-params[1]); alpha.1 <- 1/(1+exp(-params[2]))
  beta.1 <- 1/(1+exp(-params[3])); m.1 <- exp(-params[4])
  theta <- 1/(1+exp(-params[5]))
  return(c(beta.0, alpha.1, beta.1, m.1, theta))
}

#transform.params <- function(params){
#  beta.0.t <- - log(params[1]); alpha.1.t <- - log((1/params[2])-1)
#  beta.1.t <- - log(((1-params[2])/params[3])-1); m.1.t <- - log(params[4])
#  theta.t <- - log((1/params[5]) - 1)
#  return(c(beta.0.t, alpha.1.t, beta.1.t, m.1.t, theta.t))
#}

#inv.transform.params <- function(params){
#  beta.0 <- exp(-params[1]); alpha.1 <- 1/(1+exp(-params[2]))
#  beta.1 <- (1-alpha.1)/(1+exp(-params[3])); m.1 <- exp(-params[4])
#  theta <- 1/(1+exp(-params[5]))
#  return(c(beta.0, alpha.1, beta.1, m.1, theta))
#}


# Negative log-likelihood function

neg.loglik.gp.ingarch <- function(p, data){
  beta.0 <- exp(-p[1]); alpha.1 <- 1/(1+exp(-p[2]));
  beta.1 <- 1/(1+exp(-p[3])); m.1 <- exp(-p[4]);
  theta <- 1/(1+exp(-p[5]))
  
  Y <- data$battle_deaths
  N <- length(Y)
  m <- rep(NA, N)
  m[1] <- m.1
  nll <- 0 
  for (i in 2:N){
    m[i] <- beta.0+alpha.1*Y[i-1]+beta.1*m[i-1]
    lambda.i <- m[i]*(1-theta)
    nll <- nll - log(lambda.i) + lgamma(Y[i]+1) - (Y[i]-1)*log(lambda.i+theta*Y[i]) + (lambda.i+theta*Y[i])
  }    
  return(nll)
}

# Fit countries 

source("fit_country.R")
set.seed(48835)
col.nlm.obj <- fit_country("colombia", neg.loglik.gp.ingarch,
                           transform.params, inv.transform.params, 
                           params=c(0.01, 0.1, 0.6, 3.4, 0.9), N.iter=20, sigma=0.1)
uga.nlm.obj <- fit_country("uganda", neg.loglik.gp.ingarch,
                           transform.params, inv.transform.params, 
                           params=c(0.01, 0.1, 0.6, 3.4, 0.9), N.iter=20, sigma=0.1)
con.nlm.obj <- fit_country("congo", neg.loglik.gp.ingarch,
                           transform.params, inv.transform.params, 
                           params=c(0.01, 0.1, 0.6, 3.4, 0.95), N.iter=20, sigma=0.1)
eth.nlm.obj <- fit_country("ethiopia", neg.loglik.gp.ingarch,
                           transform.params, inv.transform.params, 
                           params=c(0.01, 0.1, 0.1, 3.4, 0.98), N.iter=20, sigma=0.1)

# Extract estimates

col.est <- inv.transform.params(col.nlm.obj$estimate)
uga.est <- inv.transform.params(uga.nlm.obj$estimate)
con.est <- inv.transform.params(con.nlm.obj$estimate)
eth.est <- inv.transform.params(eth.nlm.obj$estimate)

# Show rounded estimates in matrix 

est.matrix <- matrix(c(col.est, uga.est, con.est, eth.est),
                     nrow=4, ncol=5, byrow=T)
rownames(est.matrix) <- c("Colombia", "Uganda", "Congo", "Ethiopia")
colnames(est.matrix) <- c("beta.0", "alpha.1", "beta.1", "m", "theta")
est.matrix[, 1] <- signif(est.matrix[, 1], 2)
est.matrix[, 2] <- signif(est.matrix[, 2], 3)
est.matrix[, 3] <- round(est.matrix[, 3], 4)
est.matrix[, 4] <- round(est.matrix[, 4], 1)
est.matrix[, 5] <- round(est.matrix[, 5], 3)
est.matrix
##################################

#           beta.0 alpha.1 beta.1      m theta
# Colombia 6.3e-02  0.0488 0.9448    5.1 0.843
# Uganda   1.7e-10  0.0434 0.9565    1.3 0.916
# Congo    5.5e-01  0.1340 0.8262    0.0 0.957
# Ethiopia 2.3e+01  0.1180 0.4501 1476.0 0.991

#################################

# Compute, round and show AIC and BIC

source("aic_bic.R")

IC.matrix <- matrix(nrow=4, ncol=2)
rownames(IC.matrix) <- c("Colombia", "Uganda", "Congo", "Ethiopia")
colnames(IC.matrix) <- c("AIC", "BIC")

IC.matrix[, 1] <- c(AIC.normalized(-col.nlm.obj$minimum, 5, 1617, 1),
                    AIC.normalized(-uga.nlm.obj$minimum, 5, 1617, 1),
                    AIC.normalized(-con.nlm.obj$minimum, 5, 1617, 1),
                    AIC.normalized(-eth.nlm.obj$minimum, 5, 1617, 1))

IC.matrix[, 2] <- c(BIC.normalized(-col.nlm.obj$minimum, 5, 1617, 1),
                    BIC.normalized(-uga.nlm.obj$minimum, 5, 1617, 1),
                    BIC.normalized(-con.nlm.obj$minimum, 5, 1617, 1),
                    BIC.normalized(-eth.nlm.obj$minimum, 5, 1617, 1))
IC.matrix[, 1] <- round(IC.matrix[, 1])
IC.matrix[, 2] <- round(IC.matrix[, 2])
IC.matrix

#############################

#           AIC  BIC
# Colombia 9364 9391
# Uganda   4160 4187
# Congo    6180 6207
# Ethiopia 6564 6591

############################

# PMF and CDF of the Generalized Poisson

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
                       nrow=4, ncol=5, byrow=T)

rps.vec <- rep(NA, 4)
qs.vec <- rep(NA, 4)
for (i in 1:4){
  beta.0 <- param.matrix[i, 1]
  alpha.1 <- param.matrix[i, 2]
  beta.1 <- param.matrix[i, 3]
  m.1 <- param.matrix[i, 4]
  theta <- param.matrix[i, 5]
  
  country <- read.csv(paste0("../../data/", countries[i], ".csv"))
  
  M <- rep(NA, 1617)
  M[1] <- m.1
  for (k in 2:1617){
    M[k] <- beta.0 + alpha.1*country$battle_deaths[k-1] + beta.1*M[k-1]
  }
  
  params.1 <- (1-theta)*M[2:1617]
  param.2 <- theta
  Y <- country$battle_deaths[2:1617]
  
  rps.vec[i] <- rps(Y, params.1, param.2, dgpois)
  qs.vec[i] <- qs(Y, params.1, param.2, dgpois)
  print(qs.vec[i])
}

score.matrix <- matrix(c(rps.vec, qs.vec), nrow=4, ncol=2)
rownames(score.matrix) <- c("Colombia", "Uganda", "Congo", "Ethiopia")
colnames(score.matrix) <- c("RPS", "QS")
score.matrix[, 1] <- round(score.matrix[, 1], 2)
score.matrix[, 2] <- round(score.matrix[, 2], 4)
score.matrix

#################################

#            RPS      QS
# Colombia  6.19 -0.1875
# Uganda    3.66 -0.6588
# Congo    11.85 -0.5077
# Ethiopia 49.55 -0.4814

#################################

# Plot of conditional means through time
par(mfrow=c(1,2))
for (i in c(3,4)){
  beta.0 <- param.matrix[i, 1]
  alpha.1 <- param.matrix[i, 2]
  beta.1 <- param.matrix[i, 3]
  m.1 <- param.matrix[i, 4]
  theta <- param.matrix[i, 5]
  
  country <- read.csv(paste0("../../data/", countries[i], ".csv"))
  
  M <- rep(NA, 1617)
  M[1] <- m.1
  for (k in 2:1617){
    M[k] <- beta.0 + alpha.1*country$battle_deaths[k-1] + beta.1*M[k-1]
  }
  
  plot(1:1617, M, type='l', main=paste0("GP-INGARCH(1,1), ", countries[i]))
}

