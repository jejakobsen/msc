setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Transformation functions for parameters 

transform.params <- function(params){
  n.t <- - log(params[1]); pi.t <- - log((1/params[2]) - 1)
  return(c(n.t, pi.t))
}

inv.transform.params <- function(params){
  n <- exp(-params[1]); pi <- 1/(1+exp(-params[2]))
  return(c(n, pi))
}

# Estimation

neg.loglik.nb <- function(p, data){
  n <- exp(-p[1]); pi <- 1/(1+exp(-p[2]))
  
  Y <- data$battle_deaths
  N <- length(Y)
  
  nll <- 0
  for (i in 1:N){
    nll <- nll - lgamma(Y[i]+n) + lgamma(Y[i]+1) + lgamma(n) - Y[i]*log(1-pi) - n*log(pi)
  }
  return(nll)
}

# Fit countries

source("fit_country.R")
set.seed(33247)
col.nlm.obj <- fit_country("colombia", neg.loglik.nb,
                           transform.params, inv.transform.params, 
                           params=c(1, 0.03), N.iter=5, sigma=0.1)
uga.nlm.obj <- fit_country("uganda", neg.loglik.nb,
                           transform.params, inv.transform.params, 
                           params=c(1, 0.006), N.iter=5, sigma=0.1)
con.nlm.obj <- fit_country("congo", neg.loglik.nb,
                           transform.params, inv.transform.params, 
                           params=c(1, 0.006), N.iter=5, sigma=0.1)
eth.nlm.obj <- fit_country("ethiopia", neg.loglik.nb,
                           transform.params, inv.transform.params, 
                           params=c(1, 0.006), N.iter=5, sigma=0.1)

# Extract estimates

col.est <- inv.transform.params(col.nlm.obj$estimate)
uga.est <- inv.transform.params(uga.nlm.obj$estimate)
con.est <- inv.transform.params(con.nlm.obj$estimate)
eth.est <- inv.transform.params(eth.nlm.obj$estimate)

# Show rounded estimates in matrix 

est.matrix <- matrix(c(col.est, uga.est, con.est, eth.est),
                     nrow=4, ncol=2, byrow=T)
rownames(est.matrix) <- c("Colombia", "Uganda", "Congo", "Ethiopia")
colnames(est.matrix) <- c("n", "pi")
est.matrix[, 1] <- round(est.matrix[, 1], 3)
est.matrix[, 2] <- round(est.matrix[, 2], 4)
est.matrix
##################################

#              n     pi
# Colombia 0.282 0.0273
# Uganda   0.059 0.0126
# Congo    0.073 0.0053
# Ethiopia 0.051 0.0009

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

dnbinom(0, size=col.est[1], prob=col.est[2])
dnbinom(0, size=uga.est[1], prob=uga.est[2])
dnbinom(0, size=con.est[1], prob=con.est[2])
dnbinom(0, size=eth.est[1], prob=eth.est[2])
#############################

#           AIC  BIC
# Colombia 9658 9669
# Uganda   4637 4648
# Congo    6424 6435
# Ethiopia 6700 6711

############################

# Compute Scores

source("rps.R")

countries <- c("colombia", "uganda", "congo", "ethiopia")
param.matrix <- matrix(c(col.est, uga.est, con.est, eth.est),
                       nrow=4, ncol=2, byrow=T)

rps.vec <- rep(NA, 4)
qs.vec <- rep(NA, 4)
for (i in 1:4){
  n <- param.matrix[i, 1]
  pi <- param.matrix[i, 2]
  
  country <- read.csv(paste0("../../data/", countries[i], ".csv"))
  
  Y <- country$battle_deaths[2:1617]
  params.1 <- rep(n, 1616)
  param.2 <- pi
  
  rps.vec[i] <- rps(Y, params.1, param.2, dnbinom)
  qs.vec[i] <- qs(Y, params.1, param.2, dnbinom)
}

score.matrix <- matrix(c(rps.vec, qs.vec), nrow=4, ncol=2)
rownames(score.matrix) <- c("Colombia", "Uganda", "Congo", "Ethiopia")
colnames(score.matrix) <- c("RPS", "QS")
score.matrix

#################################

#                RPS         QS
# Colombia  7.235025 -0.1623232
# Uganda    4.166105 -0.6080658
# Congo    12.812839 -0.4666822
# Ethiopia 53.342701 -0.4782705

#################################

# Unrounded parameter estimates used in model_adequacy script
#####################################################################

col.est
#[1] 0.28201175 0.02732244
uga.est
#[1] 0.05866680 0.01262854
con.est
#[1] 0.073065624 0.005291858
eth.est
#[1] 0.0513704648 0.0009487691

######################################################################
