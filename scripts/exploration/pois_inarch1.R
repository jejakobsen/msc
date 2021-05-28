setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Transformation functions for parameters 

transform.params <- function(params){
  beta.0.t <- - log(params[1]); alpha.1.t <- - log((1/params[2])-1)
  return(c(beta.0.t, alpha.1.t))
}

inv.transform.params <- function(params){
  beta.0 <- exp(-params[1]); alpha.1 <- 1/(1+exp(-params[2]))
  return(c(beta.0, alpha.1))
}

# Negative log-likelihood function

neg.loglik.pois.inarch <- function(p, data){
  beta.0 <- exp(-p[1]); alpha.1 <- 1/(1+exp(-p[2]));
  
  Y <- data$battle_deaths
  N <- length(Y)
  m <- rep(NA, N)
  nll <- 0 
  for (i in 2:N){
    lambda.i <- beta.0+alpha.1*Y[i-1]
    nll <- nll - log(lambda.i) + lgamma(Y[i]+1) - (Y[i]-1)*log(lambda.i) + (lambda.i)
  }
  return(nll)
} 

# Fit countries 

source("fit_country.R")
set.seed(2347587)
col.nlm.obj <- fit_country("colombia", neg.loglik.pois.inarch,
                           transform.params, inv.transform.params, 
                           params=c(5, 0.4), N.iter=20, sigma=0.1)
uga.nlm.obj <- fit_country("uganda", neg.loglik.pois.inarch,
                           transform.params, inv.transform.params, 
                           params=c(9, 0.33), N.iter=20, sigma=0.1)
con.nlm.obj <- fit_country("congo", neg.loglik.pois.inarch,
                           transform.params, inv.transform.params, 
                           params=c(9, 0.33), N.iter=20, sigma=0.1)
eth.nlm.obj <- fit_country("ethiopia", neg.loglik.pois.inarch,
                           transform.params, inv.transform.params, 
                           params=c(9, 0.33), N.iter=20, sigma=0.1)

# Extract estimates

col.est <- inv.transform.params(col.nlm.obj$estimate)
uga.est <- inv.transform.params(uga.nlm.obj$estimate)
con.est <- inv.transform.params(con.nlm.obj$estimate)
eth.est <- inv.transform.params(eth.nlm.obj$estimate)

# Show rounded estimates in matrix 

est.matrix <- matrix(c(col.est, uga.est, con.est, eth.est),
                     nrow=4, ncol=2, byrow=T)
rownames(est.matrix) <- c("Colombia", "Uganda", "Congo", "Ethiopia")
colnames(est.matrix) <- c("beta", "alpha")
est.matrix[, 2] <- round(est.matrix[, 2], 3)
est.matrix[, 1] <- round(est.matrix[, 1], 1)
est.matrix
##################################

#          beta alpha
# Colombia  5.9 0.410
# Uganda    2.8 0.383
# Congo     6.1 0.554
# Ethiopia 29.7 0.447

#################################

# Unrounded parameter estimates used in model_adequacy script
#####################################################################

col.est
#[1] 5.9283033 0.4098723
uga.est
#[1] 2.8333501 0.3826761
con.est
#[1] 6.1275273 0.5543957
eth.est
#[1] 29.7393530  0.4471952

######################################################################

# Compute, round and show AIC and BIC

source("aic_bic.R")

IC.matrix <- matrix(nrow=4, ncol=2)
rownames(IC.matrix) <- c("Colombia", "Uganda", "Congo", "Ethiopia")
colnames(IC.matrix) <- c("AIC", "BIC")

IC.matrix[, 1] <- c(AIC.normalized(-col.nlm.obj$minimum, 2, 1617, 1),
                    AIC.normalized(-uga.nlm.obj$minimum, 2, 1617, 1),
                    AIC.normalized(-con.nlm.obj$minimum, 2, 1617, 1),
                    AIC.normalized(-eth.nlm.obj$minimum, 2, 1617, 1))

IC.matrix[, 2] <- c(BIC.normalized(-col.nlm.obj$minimum, 2, 1617, 1),
                    BIC.normalized(-uga.nlm.obj$minimum, 2, 1617, 1),
                    BIC.normalized(-con.nlm.obj$minimum, 2, 1617, 1),
                    BIC.normalized(-eth.nlm.obj$minimum, 2, 1617, 1))
IC.matrix[, 1] <- round(IC.matrix[, 1])
IC.matrix[, 2] <- round(IC.matrix[, 2])
IC.matrix

#############################

#             AIC    BIC
# Colombia  32484  32494
# Uganda    28581  28592
# Congo     90315  90325
# Ethiopia 485314 485324

############################

# Compute Scores

source("rps.R")

countries <- c("colombia", "uganda", "congo", "ethiopia")
param.matrix <- matrix(c(col.est, uga.est, con.est, eth.est),
                       nrow=4, ncol=2, byrow=T)

rps.vec <- rep(NA, 4)
qs.vec <- rep(NA, 4)
for (i in 1:4){
  beta.0 <- param.matrix[i, 1]
  alpha <- param.matrix[i, 2]
  
  country <- read.csv(paste0("../../data/", countries[i], ".csv"))
  
  Y <- country$battle_deaths[2:1617]
  params.1 <- beta.0 + alpha*country$battle_deaths[1:1616]
  param.2 <- FALSE 
  # There is no second parameter for this model, but for the code to work
  # we must provide a second parameter. The second parameter in dpois is 
  # log, which has default FALSE. Hence, we set param.2 to FALSE, which makes
  # code run and does not make any difference. 
  
  rps.vec[i] <- rps(Y, params.1, param.2, dpois)
  qs.vec[i] <- qs(Y, params.1, param.2, dpois)
}

score.matrix <- matrix(c(rps.vec, qs.vec), nrow=4, ncol=2)
rownames(score.matrix) <- c("Colombia", "Uganda", "Congo", "Ethiopia")
colnames(score.matrix) <- c("RPS", "QS")
score.matrix[, 1] <- round(score.matrix[, 1], 2)
score.matrix[, 2] <- round(score.matrix[, 2], 4)
score.matrix
#################################

#            RPS     QS
# Colombia  8.57 0.0480
# Uganda    5.44 0.0520
# Congo    17.50 0.0812
# Ethiopia 82.66 0.0459

#################################