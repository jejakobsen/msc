setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Transformation functions for parameters 

transform.params <- function(params){
  lambda.t <- - log(params[1]); theta.t <- - log((1/params[2]) - 1)
  return(c(lambda.t, theta.t))
}

inv.transform.params <- function(params){
  lambda <- exp(-params[1]); theta <- 1/(1+exp(-params[2]))
  return(c(lambda, theta))
}

# Estimation

neg.loglik.gp <- function(p, data){
  lambda <- exp(-p[1]); theta <- 1/(1+exp(-p[2]))
  
  Y <- data$battle_deaths
  N <- length(Y)
  
  nll <- 0
  for (i in 1:N){
    nll <- nll - log(lambda) + lgamma(Y[i]+1) - (Y[i]-1)*log(lambda+theta*Y[i]) + (lambda+theta*Y[i])
  }
  return(nll)
}

# Fit countries

source("fit_country.R")
set.seed(563489)
col.nlm.obj <- fit_country("colombia", neg.loglik.gp,
                           transform.params, inv.transform.params, 
                           params=c(1, 0.9), N.iter=5, sigma=0.1)
uga.nlm.obj <- fit_country("uganda", neg.loglik.gp,
                           transform.params, inv.transform.params, 
                           params=c(1, 0.9), N.iter=5, sigma=0.1)
con.nlm.obj <- fit_country("congo", neg.loglik.gp,
                           transform.params, inv.transform.params, 
                           params=c(1, 0.9), N.iter=5, sigma=0.1)
eth.nlm.obj <- fit_country("ethiopia", neg.loglik.gp,
                           transform.params, inv.transform.params, 
                           params=c(1, 0.9), N.iter=5, sigma=0.1)

# Extract estimates

col.est <- inv.transform.params(col.nlm.obj$estimate)
uga.est <- inv.transform.params(uga.nlm.obj$estimate)
con.est <- inv.transform.params(con.nlm.obj$estimate)
eth.est <- inv.transform.params(eth.nlm.obj$estimate)

# Show rounded estimates in matrix 

est.matrix <- matrix(c(col.est, uga.est, con.est, eth.est),
                     nrow=4, ncol=2, byrow=T)
rownames(est.matrix) <- c("Colombia", "Uganda", "Congo", "Ethiopia")
colnames(est.matrix) <- c("lambda", "theta")
est.matrix[, 1] <- round(est.matrix[, 1], 2)
est.matrix[, 2] <- round(est.matrix[, 2], 3)
est.matrix
##################################

#          lambda theta
# Colombia   1.17 0.883
# Uganda     0.27 0.941
# Congo      0.42 0.969
# Ethiopia   0.40 0.993

#################################

# Compute, round and show AIC and BIC

source("aic_bic.R")

IC.matrix <- matrix(nrow=4, ncol=2)
rownames(IC.matrix) <- c("Colombia", "Uganda", "Congo", "Ethiopia")
colnames(IC.matrix) <- c("AIC", "BIC")

IC.matrix[, 1] <- c(AIC.normalized(-col.nlm.obj$minimum, 2, 1617, 0),
                    AIC.normalized(-uga.nlm.obj$minimum, 2, 1617, 0),
                    AIC.normalized(-con.nlm.obj$minimum, 2, 1617, 0),
                    AIC.normalized(-eth.nlm.obj$minimum, 2, 1617, 0))

IC.matrix[, 2] <- c(BIC.normalized(-col.nlm.obj$minimum, 2, 1617, 0),
                    BIC.normalized(-uga.nlm.obj$minimum, 2, 1617, 0),
                    BIC.normalized(-con.nlm.obj$minimum, 2, 1617, 0),
                    BIC.normalized(-eth.nlm.obj$minimum, 2, 1617, 0))
IC.matrix[, 1] <- round(IC.matrix[, 1])
IC.matrix[, 2] <- round(IC.matrix[, 2])
IC.matrix

#############################

#           AIC  BIC
# Colombia 9958 9969
# Uganda   4864 4875
# Congo    6562 6573
# Ethiopia 6664 6675

############################

# PMF and CDF of the Generalized Poisson

dgpois <- function(x, lambda, theta) 
{
  ll <- log(lambda) - lgamma(x+1) + (x-1)*log(lambda+theta*x) - (lambda+theta*x)
  return(exp(ll))
}

# Compute Scores

source("rps.R")

countries <- c("colombia", "uganda", "congo", "ethiopia")
param.matrix <- matrix(c(col.est, uga.est, con.est, eth.est),
                       nrow=4, ncol=2, byrow=T)

rps.vec <- rep(NA, 4)
qs.vec <- rep(NA, 4)
for (i in 1:4){
  lambda <- param.matrix[i, 1]
  theta <- param.matrix[i, 2]
  
  country <- read.csv(paste0("../../data/", countries[i], ".csv"))
  
  Y <- country$battle_deaths[2:1617]
  params.1 <- rep(lambda, 1616)
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

#                RPS         QS
# Colombia  7.369718 -0.1452674
# Uganda    4.232776 -0.6037249
# Congo    12.861300 -0.4607304
# Ethiopia 52.635999 -0.4732559

#################################

dgpois <- function(x, lambda, theta, log=FALSE){
  ll <- log(lambda) - lgamma(x+1) + (x-1)*log(lambda+theta*x) - (lambda+theta*x)
  if (log){
    return(ll)
  }
  else{
    return(exp(ll)) 
  }
}

ZI.matrix <- matrix(NA, nrow=4, ncol=1)
rownames(ZI.matrix) <- c("Colombia", "Uganda", "Congo", "Ethiopia")
colnames(ZI.matrix) <- "Zero Index"
ZI.matrix[1] <- 1 + dgpois(0, col.est[1], col.est[2], log=TRUE)/(col.est[1]/(1-col.est[2]))
ZI.matrix[2] <- 1 + dgpois(0, uga.est[1], uga.est[2], log=TRUE)/(uga.est[1]/(1-uga.est[2]))
ZI.matrix[3] <- 1 + dgpois(0, con.est[1], con.est[2], log=TRUE)/(con.est[1]/(1-con.est[2]))
ZI.matrix[4] <- 1 + dgpois(0, eth.est[1], eth.est[2], log=TRUE)/(eth.est[1]/(1-eth.est[2]))
ZI.matrix

#          Zero Index
# Colombia  0.8834800
# Uganda    0.9409363
# Congo     0.9693473
# Ethiopia  0.9925271

