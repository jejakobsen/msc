setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Transformation functions for parameters 

transform.params <- function(params){
  alpha.t <- - log((1/params[1])-1); lambda.t <- - log(params[2])
  theta.t <- - log((1/params[3]) - 1)
  return(c(alpha.t, lambda.t, theta.t))
}

inv.transform.params <- function(params){
  alpha <- 1/(1+exp(-params[1])); lambda <- exp(-params[2])
  theta <- 1/(1+exp(-params[3]))
  return(c(alpha, lambda, theta))
}

# Estimation

dgpois <- function(x, lambda, theta, log=FALSE){
  ll <- log(lambda) - lgamma(x+1) + (x-1)*log(lambda+theta*x) - (lambda+theta*x)
  if (log){
    return(ll)
  }
  else{
    return(exp(ll)) 
  }
}

dgpinar1 <- function(alpha, k, l, lambda, theta){
  K <- min(k, l)
  p <- 0
  for (j in 0:K){
    p <- p + exp(dbinom(j, size=l, prob=alpha, log=TRUE) + dgpois(k-j, lambda, theta, log=TRUE))
  }
  return(p)
}

# Estimation

neg.loglik.gp.inar <- function(p, data){
  alpha <- 1/(1+exp(-p[1])); lambda <- exp(-p[2]); 
  theta <- 1/(1+exp(-p[3]))
  
  Y <- data$battle_deaths
  N <- length(Y)
  
  ll <- 0
  for (i in 2:N){
    y.t <- Y[i]; y.t.min1 <- Y[i-1]
    ll <- ll + log(dgpinar1(alpha, y.t, y.t.min1, lambda, theta))
  }
  return(-ll)
}

# Fit countries

source("fit_country.R")
set.seed(22457)
col.nlm.obj <- fit_country("colombia", neg.loglik.gp.inar,
                           transform.params, inv.transform.params, 
                           params=c(0.1, 1, 0.9), N.iter=5, sigma=0.1)
uga.nlm.obj <- fit_country("uganda", neg.loglik.gp.inar,
                           transform.params, inv.transform.params, 
                           params=c(0.1, 1, 0.9), N.iter=5, sigma=0.1)
con.nlm.obj <- fit_country("congo", neg.loglik.gp.inar,
                           transform.params, inv.transform.params, 
                           params=c(0.1, 1, 0.9), N.iter=5, sigma=0.1)
eth.nlm.obj <- fit_country("ethiopia", neg.loglik.gp.inar,
                           transform.params, inv.transform.params, 
                           params=c(0.1, 1, 0.9), N.iter=5, sigma=0.1)

# Extract estimates

col.est <- inv.transform.params(col.nlm.obj$estimate)
uga.est <- inv.transform.params(uga.nlm.obj$estimate)
con.est <- inv.transform.params(con.nlm.obj$estimate)
eth.est <- inv.transform.params(eth.nlm.obj$estimate)

# Show rounded estimates in matrix 

est.matrix <- matrix(c(col.est, uga.est, con.est, eth.est),
                     nrow=4, ncol=3, byrow=T)
rownames(est.matrix) <- c("Colombia", "Uganda", "Congo", "Ethiopia")
colnames(est.matrix) <- c("alpha", "lambda", "theta")
est.matrix[, 1] <- signif(est.matrix[, 1], 2)
est.matrix[, 2] <- round(est.matrix[, 2], 2)
est.matrix[, 3] <- round(est.matrix[, 3], 3)
est.matrix
##################################

#            alpha lambda theta
# Colombia 8.5e-09   1.17 0.883
# Uganda   7.2e-10   0.27 0.941
# Congo    4.4e-10   0.42 0.969
# Ethiopia 1.2e-22   0.40 0.993

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
# Colombia 9964 9980
# Uganda   4869 4885
# Congo    6567 6583
# Ethiopia 6650 6666

############################