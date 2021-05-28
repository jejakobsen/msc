setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Transformation functions for parameters 

transform.params <- function(params){
  alpha.t <- - log((1/params[1])-1); n.t <- - log(params[2])
  pi.t <- - log((1/params[3]) - 1)
  return(c(alpha.t, n.t, pi.t))
}

inv.transform.params <- function(params){
  alpha <- 1/(1+exp(-params[1])); n <- exp(-params[2])
  pi <- 1/(1+exp(-params[3]))
  return(c(alpha, n, pi))
}

# Estimation

dnbinar1 <- function(alpha, k, l, n, pi){
  K <- min(k, l)
  p <- 0
  for (j in 0:K){
    p <- p + exp(dbinom(j, size=l, prob=alpha, log=TRUE) + dnbinom(k-j, size=n, prob=pi, log=TRUE))
  }
  return(p)
}

neg.loglik.nb.inar <- function(p, data){
  alpha <- 1/(1+exp(-p[1])); n <- exp(-p[2]); 
  pi <- 1/(1+exp(-p[3]))
  
  Y <- data$battle_deaths
  N <- length(Y)
  
  ll <- 0
  for (i in 2:N){
    y.t <- Y[i]; y.t.min1 <- Y[i-1]
    ll <- ll + log(dnbinar1(alpha, y.t, y.t.min1, n, pi))
  }
  return(-ll)
}

# Fit countries

source("fit_country.R")
set.seed(6547)
col.nlm.obj <- fit_country("colombia", neg.loglik.nb.inar,
                           transform.params, inv.transform.params, 
                           params=c(0.1, 1, 0.03), N.iter=5, sigma=0.1)
uga.nlm.obj <- fit_country("uganda", neg.loglik.nb.inar,
                           transform.params, inv.transform.params, 
                           params=c(0.1, 1, 0.006), N.iter=5, sigma=0.1)
con.nlm.obj <- fit_country("congo", neg.loglik.nb.inar,
                           transform.params, inv.transform.params, 
                           params=c(0.1, 1, 0.006), N.iter=5, sigma=0.1)
eth.nlm.obj <- fit_country("ethiopia", neg.loglik.nb.inar,
                           transform.params, inv.transform.params, 
                           params=c(0.1, 1, 0.006), N.iter=5, sigma=0.1)

# Extract estimates

col.est <- inv.transform.params(col.nlm.obj$estimate)
uga.est <- inv.transform.params(uga.nlm.obj$estimate)
con.est <- inv.transform.params(con.nlm.obj$estimate)
eth.est <- inv.transform.params(eth.nlm.obj$estimate)

# Show rounded estimates in matrix 

est.matrix <- matrix(c(col.est, uga.est, con.est, eth.est),
                     nrow=4, ncol=3, byrow=T)
rownames(est.matrix) <- c("Colombia", "Uganda", "Congo", "Ethiopia")
colnames(est.matrix) <- c("alpha", "n", "pi")
est.matrix[, 1] <- signif(est.matrix[, 1], 2)
est.matrix[, 2] <- round(est.matrix[, 2], 3)
est.matrix[, 3] <- round(est.matrix[, 3], 4)
est.matrix
##################################

#            alpha     n     pi
# Colombia 9.6e-08 0.282 0.0273
# Uganda   9.0e-04 0.058 0.0125
# Congo    4.0e-10 0.073 0.0053
# Ethiopia 6.7e-12 0.051 0.0009

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
# Colombia 9664 9680
# Uganda   4641 4657
# Congo    6429 6446
# Ethiopia 6688 6704

############################
