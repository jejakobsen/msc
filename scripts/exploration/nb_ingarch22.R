setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Transformation functions for parameters 

transform.params <- function(params){
  beta.0.t <- - log(params[1]); alpha.1.t <- - log((1/params[2])-1)
  beta.1.t <- - log((1/params[3])-1); alpha.2.t <- - log((1/params[4])-1)
  beta.2.t <- - log((1/params[5])-1)
  m.1.t <- - log(params[6]); m.2.t <- log(params[7])
  pi.t <- - log((1/params[8]) - 1)
  return(c(beta.0.t, alpha.1.t, beta.1.t, alpha.2.t, beta.2.t, m.1.t, m.2.t, pi.t))
}

inv.transform.params <- function(params){
  beta.0 <- exp(-params[1]); alpha.1 <- 1/(1+exp(-params[2]))
  beta.1 <- 1/(1+exp(-params[3])); alpha.2 <- 1/(1+exp(-params[4]))
  beta.2 <- 1/(1+exp(-params[5]))
  m.1 <- exp(-params[6]); m.2 <- exp(-params[7]); 
  pi <- 1/(1+exp(-params[8]))
  return(c(beta.0, alpha.1, beta.1, alpha.2, beta.2, m.1, m.2, pi))
}

# Negative log-likelihood function

neg.loglik.nb.ingarch <- function(p, data){
  beta.0 <- exp(-p[1]); alpha.1 <- 1/(1+exp(-p[2]))
  beta.1 <- 1/(1+exp(-p[3])); alpha.2 <- 1/(1+exp(-p[4]))
  beta.2 <- 1/(1+exp(-p[5]))
  m.1 <- exp(-p[6]); m.2 <- exp(-p[7])
  pi <- 1/(1+exp(-p[8]))
  
  Y <- data$battle_deaths
  N <- length(Y)
  m <- rep(NA, N)
  m[1] <- m.1
  m[2] <- m.2
  nll <- 0 
  for (i in 3:N){
    m[i] <- beta.0 + alpha.1*Y[i-1] + beta.1*m[i-1] + alpha.2*Y[i-2] + beta.2*m[i-2]
    r.i <- m[i]*(pi/(1-pi))
    nll <- nll - lgamma(Y[i]+r.i) + lgamma(Y[i]+1) + lgamma(r.i) - Y[i]*log(1-pi) - r.i*log(pi)
  }
  return(nll)
} 

# Fit countries 

source("fit_country.R")
set.seed(5655)
col.nlm.obj <- fit_country("colombia", neg.loglik.nb.ingarch,
                           transform.params, inv.transform.params, 
                           params=c(0.01, 0.1, 0.2, 0.1, 0.2, 1, 1, 0.02), N.iter=20, sigma=0.1)
uga.nlm.obj <- fit_country("uganda", neg.loglik.nb.ingarch,
                           transform.params, inv.transform.params, 
                           params=c(0.01, 0.1, 0.2, 0.1, 0.2, 1, 1, 0.02), N.iter=20, sigma=0.1)
con.nlm.obj <- fit_country("congo", neg.loglik.nb.ingarch,
                           transform.params, inv.transform.params, 
                           params=c(0.01, 0.1, 0.2, 0.1, 0.2, 1, 1, 0.02), N.iter=20, sigma=0.1)
eth.nlm.obj <- fit_country("ethiopia", neg.loglik.nb.ingarch,
                           transform.params, inv.transform.params, 
                           params=c(0.01, 0.1, 0.2, 0.1, 0.2, 1, 1, 0.02), N.iter=20, sigma=0.1)

# Extract estimates

col.est <- inv.transform.params(col.nlm.obj$estimate)
uga.est <- inv.transform.params(uga.nlm.obj$estimate)
con.est <- inv.transform.params(con.nlm.obj$estimate)
eth.est <- inv.transform.params(eth.nlm.obj$estimate)

# Show rounded estimates in matrix 

est.matrix <- matrix(c(col.est, uga.est, con.est, eth.est),
                     nrow=4, ncol=8, byrow=T)
rownames(est.matrix) <- c("Colombia", "Uganda", "Congo", "Ethiopia")
colnames(est.matrix) <- c("beta.0", "alpha.1", "beta.1", "alpha.2", "beta.2", "m.1", "m.2", "pi")
est.matrix[, 1] <- signif(est.matrix[, 1], 2)
est.matrix[, 2] <- round(est.matrix[, 2], 4)
est.matrix[, 3] <- round(est.matrix[, 3], 3)
est.matrix[, 4] <- signif(est.matrix[, 4], 2)
est.matrix[, 5] <- round(est.matrix[, 5], 3)
est.matrix[, 6] <- signif(est.matrix[, 6], 2)
est.matrix[, 7] <- signif(est.matrix[, 7], 2)
est.matrix[, 8] <- round(est.matrix[, 8], 3)
est.matrix
##################################

#           beta.0 alpha.1 beta.1 alpha.2 beta.2     m.1     m.2    pi
# Colombia 1.5e-01  0.0807  0.268 4.0e-23  0.636 6.4e-05 7.6e+00 0.040
# Uganda   1.1e-45  0.0545  0.609 1.2e-09  0.336 1.3e-05 2.4e+00 0.018
# Congo    9.0e-01  0.1576  0.359 2.7e-09  0.419 4.6e-08 1.6e-11 0.007
# Ethiopia 2.5e+01  0.0866  0.083 3.5e-02  0.331 1.6e+00 1.8e+00 0.001

#################################

sum(col.est[2:5]) # 0.984888
sum(uga.est[2:5]) # 0.9996998
sum(con.est[2:5]) # 0.9358454
sum(eth.est[2:5]) # 0.5353412

# Compute, round and show AIC and BIC

source("aic_bic.R")

IC.matrix <- matrix(nrow=4, ncol=2)
rownames(IC.matrix) <- c("Colombia", "Uganda", "Congo", "Ethiopia")
colnames(IC.matrix) <- c("AIC", "BIC")

IC.matrix[, 1] <- c(AIC.normalized(-col.nlm.obj$minimum, 8, 1617, 2),
                    AIC.normalized(-uga.nlm.obj$minimum, 8, 1617, 2),
                    AIC.normalized(-con.nlm.obj$minimum, 8, 1617, 2),
                    AIC.normalized(-eth.nlm.obj$minimum, 8, 1617, 2))

IC.matrix[, 2] <- c(BIC.normalized(-col.nlm.obj$minimum, 8, 1617, 2),
                    BIC.normalized(-uga.nlm.obj$minimum, 8, 1617, 2),
                    BIC.normalized(-con.nlm.obj$minimum, 8, 1617, 2),
                    BIC.normalized(-eth.nlm.obj$minimum, 8, 1617, 2))
IC.matrix[, 1] <- round(IC.matrix[, 1])
IC.matrix[, 2] <- round(IC.matrix[, 2])
IC.matrix

#############################

#           AIC  BIC
# Colombia 9171 9214
# Uganda   4013 4056
# Congo    6124 6167
# Ethiopia 6604 6647

############################

# Compute Scores

source("rps.R")

countries <- c("colombia", "uganda", "congo", "ethiopia")
param.matrix <- matrix(c(col.est, uga.est, con.est, eth.est),
                       nrow=4, ncol=8, byrow=T)

rps.vec <- rep(NA, 4)
qs.vec <- rep(NA, 4)
for (i in 1:4){
  beta.0 <- param.matrix[i, 1]
  alpha.1 <- param.matrix[i, 2]
  beta.1 <- param.matrix[i, 3]
  alpha.2 <- param.matrix[i, 4]
  beta.2 <- param.matrix[i, 5]
  m.1 <- param.matrix[i, 6]
  m.2 <- param.matrix[i, 7]
  pi <- param.matrix[i, 8]
  
  country <- read.csv(paste0("../../data/", countries[i], ".csv"))
  
  M <- rep(NA, 1617)
  M[1] <- m.1
  M[2] <- m.2
  for (k in 3:1617){
    M[k] <- beta.0 + alpha.1*country$battle_deaths[k-1] + beta.1*M[k-1] + alpha.1*country$battle_deaths[k-2] + beta.2*M[k-2]
  }
  
  params.1 <- (pi/(1-pi))*M[3:1617]
  param.2 <- pi
  Y <- country$battle_deaths[3:1617]
  
  rps.vec[i] <- rps(Y, params.1, param.2, dnbinom)
  qs.vec[i] <- qs(Y, params.1, param.2, dnbinom)
}

score.matrix <- matrix(c(rps.vec, qs.vec), nrow=4, ncol=2)
rownames(score.matrix) <- c("Colombia", "Uganda", "Congo", "Ethiopia")
colnames(score.matrix) <- c("RPS", "QS")
score.matrix[, 1] <- round(score.matrix[, 1], 2)
score.matrix[, 2] <- round(score.matrix[, 2], 4)
score.matrix

#################################

#            RPS      QS
# Colombia  7.58 -0.1677
# Uganda    4.15 -0.6376
# Congo    14.64 -0.5137
# Ethiopia 48.60 -0.4858

#################################