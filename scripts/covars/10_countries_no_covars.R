setwd(dirname(rstudioapi::getSourceEditorContext()$path))

### Define transformation function for parameters ###
### and negative log-likelihood function          ###

transform.est.params <- function(N.countries, p){
  param_matrix <- matrix(NA, nrow=5, ncol=N.countries)
  for (i in 1:N.countries){
    param_matrix[1, i] <- exp(-p[i])
    param_matrix[2, i] <- 1/(1+exp(-p[N.countries + i]))
    param_matrix[3, i] <- (1-param_matrix[2, i])/(1+exp(-p[2*N.countries + i]))
    param_matrix[4, i] <- exp(-p[3*N.countries + i])
    param_matrix[5, i] <- 1/(1+exp(-p[4*N.countries + i]))
  }
  return(param_matrix)
}

neg.loglik.nb.ingarch <- function(p, data){
  Y <- data[[1]]
  X <- data[[2]]
  inds.start <- data[[3]]
  inds.stop <- data[[4]]
  N.countries <- length(inds.start)
  
  nll <- 0
  for (i in 1:N.countries){
    Y.i <- Y[inds.start[i]:inds.stop[i]]
    X.1.i <- X[inds.start[i]:inds.stop[i], 1]
    X.2.i <- X[inds.start[i]:inds.stop[i], 2]
    X.3.i <- X[inds.start[i]:inds.stop[i], 3]
    X.4.i <- X[inds.start[i]:inds.stop[i], 4]
    N.i <- length(Y.i)
    
    beta.0.i <- exp(-p[i])
    alpha.1.i <- 1/(1+exp(-p[N.countries + i]))
    beta.1.i <- (1-alpha.1.i)/(1+exp(-p[N.countries*2 + i]))
    m.1.i <- exp(-p[N.countries*3 + i])
    pi.i <- 1/(1+exp(-p[N.countries*4 + i]))
    
    M.i <- rep(NA, N.i)
    M.i[1] <- m.1.i
    for (t in 2:N.i){
      M.i[t] <- beta.0.i + alpha.1.i * Y.i[t-1] + beta.1.i * M.i[t-1]
      r.i.t <- M.i[t]*(pi.i/(1-pi.i))
      nll <- nll - lgamma(Y.i[t]+r.i.t) + lgamma(Y.i[t]+1) + lgamma(r.i.t) - Y.i[t]*log(1-pi.i) - r.i.t*log(pi.i)
    }   
  }
  return(nll)
} 

###################################################################

### Prepare data, parameters and sigmas ###

source("prep_data.R")
countries <- c("colombia", "congo", "ethiopia",
               "iraq", "mali", "myanmar",
               "nigeria", "pakistan", "sleone",
               "uganda")
data <- prep_data(countries)

source("prep_params_alt.R")
params <- prep_params_alt(10, beta.0=0.1, alpha.1=0.1, beta.1=0.6, 
                          m.1=1, pi=0.01, gamma.0=FALSE, 
                          gammas=FALSE)

sigmas <- prep_sigmas_alt(10, sig.beta.0=0.1, sig.alpha.1=0.001,
                          sig.beta.1=0.001, sig.m.1=0.1, 
                          sig.pi=0.001, sig.gamma.0=FALSE, 
                          sig.gammas=FALSE)

####################################################################

### Fit Model and extract parameters ###

source("fit_countries.R")
set.seed(33942341)
best.fit <- fit_countries(neg.loglik.nb.ingarch, data, params, 
                          N.iter=10, sigma=sigmas, hess=FALSE)

# Negative log-likelihood of 10 iterations
#[1] 29390.40 29390.24 29390.19 29388.89 29387.97 
#[6] 29387.97 29387.96 29387.96 29387.96 29387.96

est.params <- transform.est.params(10, best.fit$estimate) 

##########################################################

### Compute country-specific AIC ###

source("../exploration/aic_bic.R")

ll <- function(p, Y, X){
  beta.0 <- p[1]
  alpha.1 <- p[2]
  beta.1 <- p[3]; m.1 <- p[4]
  pi <- p[5]
  
  N <- length(Y)
  
  m <- rep(NA, N)
  m[1] <- m.1
  
  nll <- 0 
  for (i in 2:N){
    m[i] <- beta.0 + alpha.1 * Y[i-1] + beta.1 * m[i-1]
    r.i <- m[i]*(pi/(1-pi))
    nll <- nll - lgamma(Y[i]+r.i) + lgamma(Y[i]+1) + lgamma(r.i) - Y[i]*log(1-pi) - r.i*log(pi)
  }
  return(-nll)
}

AIC.normalized.matrix <- function(ll.func, param.matrix, data, N.excluded, country.names){
  inds.start <- data[[3]]
  inds.stop <- data[[4]]
  N.countries <- length(param.matrix[1, ])
  N.params <- length(param.matrix[, 1])
  AIC.matrix <- matrix(NA, nrow=N.countries, ncol=1)
  colnames(AIC.matrix) <- "AIC"
  rownames(AIC.matrix) <- country.names
  for (i in 1:N.countries){
    Y <- data[[1]][inds.start[i]:inds.stop[i]]
    X <- data[[2]][inds.start[i]:inds.stop[i], ]
    params <- param.matrix[, i]
    log.lik <- ll(params, Y, X)
    AIC.matrix[i] <- round(AIC.normalized(log.lik, N.params, length(Y), N.excluded))
  }
  return(AIC.matrix)
}

aic.matrix <- AIC.normalized.matrix(ll, est.params, data, 1, countries)
aic.matrix

#           AIC
# colombia 9176
# congo    6120
# ethiopia 6606
# iraq     9966
# mali     2818
# myanmar  5653
# nigeria  4670
# pakistan 7080
# sleone   2818
# uganda   4006

########################################################

### NB-INGARCH(1,1) specific parameters ###

est.matrix <- t(est.params[1:5, ])
rownames(est.matrix) <- countries
colnames(est.matrix) <- c("beta.0", "alpha.1", "beta.1", "m.1", "pi")
est.matrix[, 1] <- signif(est.matrix[, 1], 2)
est.matrix[, 2] <- round(est.matrix[, 2], 3)
est.matrix[, 3] <- round(est.matrix[, 3], 3)
est.matrix[, 4] <- round(est.matrix[, 4], 1)
est.matrix[, 5] <- round(est.matrix[, 5], 3)
est.matrix

###############################################

#           beta.0 alpha.1 beta.1   m.1    pi
# colombia 8.7e-02   0.046  0.945   5.4 0.039
# congo    6.5e-01   0.110  0.844   0.0 0.007
# ethiopia 1.8e+01   0.085  0.579 610.8 0.001
# iraq     1.0e-01   0.096  0.904   0.0 0.013
# mali     4.0e-03   0.060  0.940   0.0 0.032
# myanmar  1.6e+00   0.134  0.527   0.1 0.022
# nigeria  1.3e-03   0.018  0.982   0.0 0.016
# pakistan 1.8e-02   0.082  0.918   0.0 0.029
# sleone   5.2e-03   0.101  0.899   0.0 0.009
# uganda   1.4e-06   0.041  0.959   1.6 0.018

###############################################


save.image(file="10_countries_no_covars.RData")