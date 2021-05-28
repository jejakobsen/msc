setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Transformation functions for parameters 

transform.params <- function(params){
  beta.0.t <- - log(params[1]); alpha.1.t <- - log((1/params[2])-1)
  beta.1.t <- - log((1/params[3])-1); m.1.t <- - log(params[4])
  pi.t <- - log((1/params[5]) - 1)
  g.0 <- params[6]; g.1 <- params[7]; g.2 <- params[8]; g.3 <- params[9]
  g.4 <- params[10]; g.5 <- params[11]
  return(c(beta.0.t, alpha.1.t, beta.1.t, m.1.t, pi.t, g.0, g.1, g.2, g.3, g.4, g.5))
}

inv.transform.params <- function(params){
  beta.0 <- exp(-params[1]); alpha.1 <- 1/(1+exp(-params[2]))
  beta.1 <- 1/(1+exp(-params[3])); m.1 <- exp(-params[4])
  pi <- 1/(1+exp(-params[5]))
  g.0 <- params[6]; g.1 <- params[7]; g.2 <- params[8]; g.3 <- params[9]
  g.4 <- params[10]; g.5 <- params[11]
  return(c(beta.0, alpha.1, beta.1, m.1, pi, g.0, g.1, g.2, g.3, g.4, g.5))
}

# Negative log-likelihood function

neg.loglik.nb.ingarch <- function(p, data){
  beta.0 <- exp(-p[1]); 
  alpha.1 <- 1/(1+exp(-p[2]));
  beta.1 <- 1/(1+exp(-p[3])); m.1 <- exp(-p[4]);
  pi <- 1/(1+exp(-p[5]))
  g.0 <- p[6]; g.1 <- p[7]; g.2 <- p[8]; g.3 <- p[9]; g.4 <- p[10]; g.5 <- p[11]
  
  Y <- data$battle_deaths
  x.1 <- (data$v2x_polyarchy - mean(data$v2x_polyarchy))/sd(data$v2x_polyarchy)
  x.2 <- (log(data$gdp_pc) - mean(log(data$gdp_pc)))/sd(log(data$gdp_pc))
  x.3 <- (log(data$pop_tot) - mean(log(data$pop_tot)))/sd(log(data$pop_tot))
  x.4 <- (data$year - mean(data$year))/sd(data$year)
  N <- length(Y)
  
  h <- exp(-(g.0 + g.1*x.1 + g.2*x.1^2 + g.3*x.2 + g.4*x.3 + g.5*x.4))
  
  m <- rep(NA, N)
  m[1] <- m.1
  nll <- 0 
  for (i in 2:N){
    m[i] <- beta.0+alpha.1*Y[i-1]+beta.1*m[i-1] + h[i]
    r.i <- m[i]*(pi/(1-pi))
    nll <- nll - lgamma(Y[i]+r.i) + lgamma(Y[i]+1) + lgamma(r.i) - Y[i]*log(1-pi) - r.i*log(pi)
  }
  return(nll) 
} 

# Fit Congo with covariates using fit_country
setwd("../exploration/")
source("fit_country.R")
source("aic_bic.R")
setwd("../covars/")
set.seed(123335)
con.nlm.obj <- fit_country("congo", neg.loglik.nb.ingarch,
                           transform.params, inv.transform.params, 
                           params=c(0.01, 0.05, 0.8, 3.4, 0.02, 3, 0, 0, 0, 0, 0),
                           N.iter=20, sigma=0.01)

# Compute AIC
AIC.normalized(-con.nlm.obj$minimum, 11, 1617, 1)
# 5707.615

# Extract estimates
con.est <- inv.transform.params(con.nlm.obj$estimate)


# NB-INGARCH(1,1) specific parameters 
params.matrix <- matrix(con.est[1:5], ncol=5, nrow=1)
colnames(params.matrix) <- c("beta.0", "alpha.1", "beta.1", "m.1", "pi")
rownames(params.matrix) <- c("est")
params.matrix[1, 1] <- signif(params.matrix[1, 1], 2)
params.matrix[1, 2:5] <- round(params.matrix[1, 2:5], 4)
params.matrix

#      beta.0 alpha.1 beta.1 m.1     pi
# est 2.4e-19  0.0641 0.8184   0 0.0074

# Estimates of gamma
covar.matrix <- matrix(con.est[6:11], ncol=6, nrow=1)
colnames(covar.matrix) <- c("g.0", "g.1", "g.2", "g.3", "g.4", "g.5")
rownames(covar.matrix) <- c("est")
covar.matrix[1, ] <- round(covar.matrix[1, ], 2)
covar.matrix

#      g.0   g.1   g.2  g.3    g.4   g.5
# est 1.43 -0.14 -0.29 1.47 -24.03 22.45

# Standardize data (same used in negative log-likelihood function)
x.1 <- (data$v2x_polyarchy - mean(data$v2x_polyarchy))/sd(data$v2x_polyarchy)
x.2 <- (log(data$gdp_pc) - mean(log(data$gdp_pc)))/sd(log(data$gdp_pc))
x.3 <- (log(data$pop_tot) - mean(log(data$pop_tot)))/sd(log(data$pop_tot))
x.4 <- (data$year - mean(data$year))/sd(data$year)
Y <- data$battle_deaths

# Correlation between standardized log(pop_tot) and time
cor(x.3, x.4) 
# 0.9992401

### Plots of battle_deaths, time-specific intercept and M.t ###
Mh.t <- function(p, Y, x.1, x.2, x.3, x.4){
  beta.0 <- p[1]; alpha.1 <- p[2]; beta.1 <- p[3]
  m.1 <- p[4]; pi <- p[5]
  g.0 <- p[6]; g.1 <- p[7]; g.2 <- p[8]; g.3 <- p[9]
  g.4 <- p[10]; g.5 <- p[11]
  
  h <- exp(-(g.0 + g.1*x.1 + g.2*x.1^2 + g.3*x.2 + g.4*x.3 + g.5*x.4))

  N <- length(Y)
  m <- rep(NA, N)
  m[1] <- m.1
  for (i in 2:N){
    m[i] <- beta.0+alpha.1*Y[i-1]+beta.1*m[i-1] + h[i]
    #r.i <- m[i]*(pi/(1-pi))
  }
  Mh.mat <- matrix(c(m, h), nrow=2, ncol=1617, byrow=TRUE)
  return(Mh.mat)
}

Mh.con <- Mh.t(con.est, Y, x.1, x.2, x.3, x.4)

pdf("./figs/con_covars.pdf", width=10, height=5)
par(mfrow=c(1,3))
plot(1:1617, data$battle_deaths, type='l', xlab="t", ylab="", cex.lab=1.6, cex.axis=1.2)
title(ylab="battle_deaths", line=2.1, cex.lab=1.6)
plot(1:1617, Mh.con[2, ], type='l', xlab="t", ylab="", cex.lab=1.6, cex.axis=1.2)
title(ylab=expression(beta[0] + exp(-gamma^T* Z[t])), line=2.1, cex.lab=1.6)
plot(1:1617, Mh.con[1, ], type='l', xlab="t", ylab="", cex.lab=1.6, cex.axis=1.2)
title(ylab=expression(M[t]), line=2.1, cex.lab=1.6)
dev.off()



