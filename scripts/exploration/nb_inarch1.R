setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Transformation functions for parameters 

transform.params <- function(params){
  beta.0.t <- - log(params[1]); alpha.1.t <- - log((1/params[2])-1)
  pi.t <- - log((1/params[3]) - 1)
  return(c(beta.0.t, alpha.1.t, pi.t))
}

inv.transform.params <- function(params){
  beta.0 <- exp(-params[1]); alpha.1 <- 1/(1+exp(-params[2]))
  pi <- 1/(1+exp(-params[3]))
  return(c(beta.0, alpha.1, pi))
}

# Negative log-likelihood function

neg.loglik.nb.inarch <- function(p, data){
  beta.0 <- exp(-p[1]); alpha.1 <- 1/(1+exp(-p[2]));
  pi <- 1/(1+exp(-p[3]))
  
  Y <- data$battle_deaths
  N <- length(Y)
  m <- rep(NA, N)
  nll <- 0 
  for (i in 2:N){
    r.i <- (beta.0+alpha.1*Y[i-1])*(pi/(1-pi))
    nll <- nll - lgamma(Y[i]+r.i) + lgamma(Y[i]+1) + lgamma(r.i) - Y[i]*log(1-pi) - r.i*log(pi)
  }
  return(nll)
} 

# Fit countries 

source("fit_country.R")
set.seed(234)
col.nlm.obj <- fit_country("colombia", neg.loglik.nb.inarch,
                            transform.params, inv.transform.params, 
                            params=c(5, 0.4, 0.03), N.iter=20, sigma=0.1)
uga.nlm.obj <- fit_country("uganda", neg.loglik.nb.inarch,
                           transform.params, inv.transform.params, 
                           params=c(9, 0.33, 0.006), N.iter=20, sigma=0.1)
con.nlm.obj <- fit_country("congo", neg.loglik.nb.inarch,
                           transform.params, inv.transform.params, 
                           params=c(9, 0.33, 0.006), N.iter=20, sigma=0.1)
eth.nlm.obj <- fit_country("ethiopia", neg.loglik.nb.inarch,
                           transform.params, inv.transform.params, 
                           params=c(9, 0.33, 0.006), N.iter=20, sigma=0.1)

# Extract estimates

col.est <- inv.transform.params(col.nlm.obj$estimate)
uga.est <- inv.transform.params(uga.nlm.obj$estimate)
con.est <- inv.transform.params(con.nlm.obj$estimate)
eth.est <- inv.transform.params(eth.nlm.obj$estimate)

# Show rounded estimates in matrix 

est.matrix <- matrix(c(col.est, uga.est, con.est, eth.est),
                     nrow=4, ncol=3, byrow=T)
rownames(est.matrix) <- c("Colombia", "Uganda", "Congo", "Ethiopia")
colnames(est.matrix) <- c("beta", "alpha", "pi")
est.matrix[, 2:3] <- round(est.matrix[, 2:3], 3)
est.matrix[, 1] <- round(est.matrix[, 1], 1)
est.matrix
##################################

#          beta alpha    pi
# Colombia  5.9 0.412 0.033
# Uganda    2.4 0.471 0.015
# Congo     9.0 0.344 0.006
# Ethiopia 46.6 0.135 0.001

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
# Colombia 9409 9425
# Uganda   4430 4447
# Congo    6286 6303
# Ethiopia 6634 6650

############################

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
  pi <- param.matrix[i, 3]
  
  country <- read.csv(paste0("../../data/", countries[i], ".csv"))
  
  Y <- country$battle_deaths[2:1617]
  params.1 <- (pi/(1-pi))*(beta.0 + alpha*country$battle_deaths[1:1616])
  param.2 <- pi
  
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
# Colombia  6.68 -0.1884
# Uganda    3.95 -0.6345
# Congo    11.98 -0.4871
# Ethiopia 50.94 -0.4828

#################################

