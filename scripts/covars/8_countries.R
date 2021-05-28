setwd(dirname(rstudioapi::getSourceEditorContext()$path))

### Define transformation function for parameters ###
### and negative log-likelihood function          ###

transform.est.params <- function(N.countries, p){
  param_matrix <- matrix(NA, nrow=11, ncol=N.countries)
  for (i in 1:N.countries){
    param_matrix[1, i] <- exp(-p[i])
    param_matrix[2, i] <- 1/(1+exp(-p[N.countries + i]))
    param_matrix[3, i] <- (1-param_matrix[2, i])/(1+exp(-p[2*N.countries + i]))
    param_matrix[4, i] <- exp(-p[3*N.countries + i])
    param_matrix[5, i] <- 1/(1+exp(-p[4*N.countries + i]))
    param_matrix[6, i] <- p[5*N.countries + i]
    param_matrix[7, i] <- p[6*N.countries + 1]
    param_matrix[8, i] <- p[6*N.countries + 2]
    param_matrix[9, i] <- p[6*N.countries + 3]
    param_matrix[10, i] <- p[6*N.countries + 4]
    param_matrix[11, i] <- p[6*N.countries + 5]
  }
  return(param_matrix)
}

neg.loglik.nb.ingarch <- function(p, data){
  Y <- data[[1]]
  X <- data[[2]]
  inds.start <- data[[3]]
  inds.stop <- data[[4]]
  N.countries <- length(inds.start)
  
  gamma.1 <- p[N.countries*6+1] 
  gamma.2 <- p[N.countries*6+2]
  gamma.3 <- p[N.countries*6+3]
  gamma.4 <- p[N.countries*6+4]
  gamma.5 <- p[N.countries*6+5]
  
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
    gamma.0.i <- p[N.countries*5 + i]
    
    XGamma.i.t <- gamma.0.i + gamma.1*X.1.i + gamma.2*(X.1.i)^2 + gamma.3*X.2.i + gamma.4*X.3.i + gamma.5*X.4.i
    h.i <- exp(-XGamma.i.t)
    
    M.i <- rep(NA, N.i)
    M.i[1] <- m.1.i
    for (t in 2:N.i){
      M.i[t] <- beta.0.i + alpha.1.i * Y.i[t-1] + beta.1.i * M.i[t-1] + h.i[t]
      r.i.t <- M.i[t]*(pi.i/(1-pi.i))
      nll <- nll - lgamma(Y.i[t]+r.i.t) + lgamma(Y.i[t]+1) + lgamma(r.i.t) - Y.i[t]*log(1-pi.i) - r.i.t*log(pi.i)
    }   
  }
  return(nll)
} 

###############################################################################

### Prepare data, parameters and sigmas ###

source("prep_data.R")
countries <- c("colombia", "congo", "mali",
               "myanmar", "nigeria", "pakistan",
               "sleone", "uganda")
data <- prep_data(countries)

source("prep_params_alt.R")
params <- prep_params_alt(8, beta.0=0.01, alpha.1=0.05, beta.1=0.85, 
                          m.1=0.5, pi=0.05, gamma.0=2, 
                          gammas=c(-1, 1, 0.5, 1, -1))
sigmas <- prep_sigmas_alt(8, sig.beta.0=0.1, sig.alpha.1=0.001,
                          sig.beta.1=0.001, sig.m.1=0.1, 
                          sig.pi=0.001, sig.gamma.0=0.1, 
                          sig.gammas=c(0.1, 0.1, 0.1, 0.1, 0.1)) 

##############################################################################

### Fit Model and extract parameters ###

source("fit_countries.R")
set.seed(2199211)
best.fit <- fit_countries(neg.loglik.nb.ingarch, data, params, 
                          N.iter=10, sigma=sigmas, hess=TRUE)

# Negative log-likelihood of 10 iterations
#[1] 20814.13 20777.23 20775.63 20774.82 20774.61
#[5] 20774.49 20774.17 20773.87 20773.73 20772.55

est.params <- transform.est.params(8, best.fit$estimate) 

##############################################################################

### Compute country-specific AIC ###

source("../exploration/aic_bic.R")

ll <- function(p, Y, X){
  beta.0 <- p[1]
  alpha.1 <- p[2]
  beta.1 <- p[3]; m.1 <- p[4]
  pi <- p[5]
  g.0 <- p[6]; g.1 <- p[7]; g.2 <- p[8]; g.3 <- p[9]; g.4 <- p[10]; g.5 <- p[11]
  
  x.1 <- X[, 1]
  x.2 <- X[, 2]
  x.3 <- X[, 3]
  x.4 <- X[, 4]
  
  h <- exp(-(g.0 + g.1*x.1 + g.2*x.1^2 + g.3*x.2 + g.4*x.3 + g.5*x.4))
  
  N <- length(Y)
  
  m <- rep(NA, N)
  m[1] <- m.1
  
  nll <- 0 
  for (i in 2:N){
    m[i] <- beta.0 + alpha.1 * Y[i-1] + beta.1 * m[i-1] + h[i]
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
# colombia 9175
# congo    5740
# mali     2701
# myanmar  5601
# nigeria  4616
# pakistan 7065
# sleone   2830
# uganda   4019

##############################################################################

### gamma.0i parameters ###

gamma.0.matrix <- matrix(est.params[6, ], nrow=8, ncol=1)
colnames(gamma.0.matrix) <- "gamma.0"
rownames(gamma.0.matrix) <- countries
gamma.0.matrix <- round(gamma.0.matrix, 2)
gamma.0.matrix

#          gamma.0
# colombia    4.39
# congo      -0.34
# mali        9.63
# myanmar     3.08
# nigeria    -3.80
# pakistan   -2.90
# sleone     22.94
# uganda     15.89

##############################################################################

### Compute 95% CI of global effects ###

gammas <- best.fit$estimate[49:53]
gammas
sigma.gammas <- sqrt(diag(solve(best.fit$hessian)))[49:53]
gammas.low <- gammas - qnorm(0.975)*sigma.gammas
gammas.high <- gammas + qnorm(0.975)*sigma.gammas

gammas.matrix <- matrix(c(gammas, gammas.low, gammas.high), nrow=3, ncol=5, byrow=TRUE)
colnames(gammas.matrix) <- c("g.1", "g.2", "g.3", "g.4", "g.5")
rownames(gammas.matrix) <- c("est", "low", "high")
gammas.matrix[, 1] <- round(gammas.matrix[, 1], 2)
gammas.matrix[, 2] <- round(gammas.matrix[, 2], 2)
gammas.matrix[, 3] <- round(gammas.matrix[, 3], 2)
gammas.matrix[, 4] <- round(gammas.matrix[, 4], 2)
gammas.matrix[, 5] <- round(gammas.matrix[, 5], 2)
gammas.matrix

#        g.1  g.2   g.3   g.4   g.5
# est  -1.39 1.44  0.31  6.47 -4.11
# low  -2.04 0.88 -3.34 -2.68 -6.32
# high -0.74 2.00  3.96 15.63 -1.90

##############################################################################

### Plots of bd, covars and M.i along with decomposed covars ###

M.i <- function(p, Y, X){
  beta.0 <- p[1]
  alpha.1 <- p[2]
  beta.1 <- p[3]; m.1 <- p[4]
  pi <- p[5]
  g.0 <- p[6]; g.1 <- p[7]; g.2 <- p[8]; g.3 <- p[9]; g.4 <- p[10]; g.5 <- p[11]
  
  x.1 <- X[, 1]
  x.2 <- X[, 2]
  x.3 <- X[, 3]
  x.4 <- X[, 4]
  
  h <- exp(-(g.0 + g.1*x.1 + g.2*x.1^2 + g.3*x.2 + g.4*x.3 + g.5*x.4))
  
  N <- length(Y)
  
  m <- rep(NA, N)
  m[1] <- m.1
  
  nll <- 0 
  for (i in 2:N){
    m[i] <- beta.0 + alpha.1 * Y[i-1] + beta.1 * m[i-1] + h[i]
    r.i <- m[i]*(pi/(1-pi))
  }
  mat <- matrix(c(beta.0 + h, m), nrow=2, ncol=1617, byrow=TRUE)
  return(mat)
}

extract.effects <- function(p, Y, X){
  g.0 <- p[6]; g.1 <- p[7]; g.2 <- p[8]; g.3 <- p[9]; g.4 <- p[10]; g.5 <- p[11]
  
  x.1 <- X[, 1]
  x.2 <- X[, 2]
  x.3 <- X[, 3]
  x.4 <- X[, 4]
  
  h.0 <- exp(-g.0*rep(1, 1617))
  h.pa <- exp(-(g.1*x.1 +g.2*x.1^2))
  h.gdp <- exp(-g.3*x.2)
  h.pop <- exp(-g.4*x.3)
  h.time <- exp(-g.5*x.4)
  
  h.mat <- matrix(c(h.0, h.pa, h.gdp, h.pop, h.time), nrow=5, ncol=1617, byrow=TRUE)
  return(h.mat)
}

plot.M.i <- function(param.matrix, data, country){
  inds.start <- data[[3]]
  inds.stop <- data[[4]]
  i <- which(countries == country)
  Y <- data[[1]][inds.start[i]:inds.stop[i]]
  X <- data[[2]][inds.start[i]:inds.stop[i], ]
  params <- param.matrix[, i]
  mat <- M.i(params, Y, X)
  par(mfrow=c(1,3))
  plot(1:1617, Y, type='l', xlab="t", ylab="", cex.lab=1.6, cex.axis=1.6)
  title(ylab="battle_deaths", line=2.1, cex.lab=1.6)  
  plot(1:1617, mat[1, ], type='l', xlab="t", cex.lab=1.6, cex.axis=1.6, ylab="", 
       main=paste0("i=", country), cex.main=2)
  title(ylab=expression(beta["0,i"] + exp(-gamma[i]^T*Z["i,t"])), line=2, cex.lab=1.6)
  plot(1:1617, mat[2, ], type='l', xlab="t", cex.lab=1.6, cex.axis=1.6, ylab="")
  title(ylab=expression(M["i,t"]), line=2.1, cex.lab=1.6)
  return(0)
}

plot.h.i <- function(param.matrix, data, country){
  inds.start <- data[[3]]
  inds.stop <- data[[4]]
  i <- which(countries == country)
  Y <- data[[1]][inds.start[i]:inds.stop[i]]
  X <- data[[2]][inds.start[i]:inds.stop[i], ]
  params <- param.matrix[, i]
  h.mat <- extract.effects(params, Y, X)
  M.mat <- M.i(params, Y, X)
  par(mfrow=c(1,2))
  plot(1:1617, M.mat[1, ], type='l', main=paste0("i=", country), 
       xlab="t", ylab="", cex.lab=1.3)
  title(ylab=expression(exp(-gamma[i]^T*Z["i,t"])), line=2, cex.lab=1.3)
  plot(1:1617, h.mat[1, ]*h.mat[4, ]*h.mat[5, ],
       type='l', col="firebrick",
       xlab="t", ylab="", cex.lab=1.3)
  lines(1:1617, h.mat[2, ], col="blue", lty=2)
  lines(1:1617, h.mat[3, ], col="forestgreen", lty=3, lwd=1.2)
  legend("topleft", 
         legend=c(expression(h["ic+pop+time"]), expression(h["pa"]), expression(h["gdp"])),
         lty=c(1,2,3), col=c("firebrick", "blue", "forestgreen"), cex=1.3)
  return(0)
}

pdf("./figs/nga_bd_covars.pdf", width=10, height=6)
plot.M.i(est.params, data, "nigeria")
dev.off()
pdf("./figs/nga_decomp.pdf", width=10, height=6)
plot.h.i(est.params, data, "nigeria")
dev.off()
pdf("./figs/mli_bd_covars.pdf", width=10, height=6)
plot.M.i(est.params, data, "mali")
dev.off()
pdf("./figs/mli_decomp.pdf", width=10, height=6)
plot.h.i(est.params, data, "mali")
dev.off()

##############################################################################

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

#           beta.0 alpha.1 beta.1  m.1   pi
# colombia 1.4e-03   0.033  0.965 5.7 0.039
# congo    9.0e-02   0.050  0.871 0.0 0.007
# mali     3.1e-02   0.091  0.702 0.0 0.036
# myanmar  1.5e+00   0.132  0.478 0.1 0.023
# nigeria  2.2e-06   0.020  0.972 0.0 0.014
# pakistan 2.2e-02   0.096  0.901 0.0 0.028
# sleone   8.7e-03   0.103  0.896 0.0 0.009
# uganda   6.4e-06   0.041  0.959 1.8 0.017

###############################################

##############################################################################

# Only for saving
save.image(file="8_countries_new.RData")
