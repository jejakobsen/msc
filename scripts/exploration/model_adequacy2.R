setwd(dirname(rstudioapi::getSourceEditorContext()$path))

countries <- c("colombia", "uganda", "congo", "ethiopia")
plot.names <- c("Colombia", "Uganda", "Congo", "Ethiopia")

col.color <- "darkgoldenrod1"
uga.color <- "firebrick"
con.color <- "cornflowerblue"
eth.color <- "darkgreen"

cols <- c(col.color, uga.color, con.color, eth.color)

# Poisson INARCH(1) estimates (beta.0, alpha)
col.est.1 <- c(5.9283033, 0.4098723)
uga.est.1 <- c(2.8333501, 0.3826761)
con.est.1 <- c(6.1275273, 0.5543957)
eth.est.1 <- c(29.7393530, 0.4471952)

# NB-INGARCH(1,1) estimates (beta.0, alpha.1, beta.1, m.1, pi)
col.est.2 <- c(8.714199e-02, 0.04627278, 0.9449050, 5.420183e+00, 0.039387728)
uga.est.2 <- c(5.530259e-10, 0.04057004, 0.9592238, 1.595643e+00, 0.017735621)
con.est.2 <- c(6.451809e-01, 0.11014131, 0.8436932, 1.601460e-06, 0.007232661)
eth.est.2 <- c(1.787846e+01, 0.08500927, 0.5795386, 6.066816e+02, 0.001079615)

est.1.mat <- matrix(c(col.est.1, uga.est.1, con.est.1, eth.est.1),
                             nrow=4, ncol=2, byrow=T)
est.2.mat <- matrix(c(col.est.2, uga.est.2, con.est.2, eth.est.2),
                       nrow=4, ncol=5, byrow=T)

# Compute conditional means for the two models for all countries
M.est.1.mat <- matrix(nrow=4, ncol=1616)
M.est.2.mat <- matrix(nrow=4, ncol=1616)

for (i in 1:4){
  beta.0.1 <- est.1.mat[i, 1]
  alpha.1.1 <- est.1.mat[i, 2]
  
  beta.0.2 <- est.2.mat[i, 1]
  alpha.1.2 <- est.2.mat[i, 2]
  beta.1.2 <- est.2.mat[i, 3]
  m.1.2 <- est.2.mat[i, 4]
  pi.2 <- est.2.mat[i, 5]
  
  country <- read.csv(paste0("../../data/", countries[i], ".csv"))
  
  M.est.1 <- rep(NA, 1617)
  M.est.2 <- rep(NA, 1617)
  M.est.2[1] <- m.1.2
  for (k in 2:1617){
    M.est.1[k] <- beta.0.1 + alpha.1.1*country$battle_deaths[k-1]
    M.est.2[k] <- beta.0.2 + alpha.1.2*country$battle_deaths[k-1] + beta.1.2*M.est.2[k-1]
  }
  
  M.est.1.mat[i, ] <- M.est.1[2:1617]
  M.est.2.mat[i, ] <- M.est.2[2:1617]
}


# Standardized Pearson Residuals

pdf("./figs/res_2models.pdf", width=10, height=15)
par(mfrow=c(4,2))
for (i in 1:4){
  country <- read.csv(paste0("../../data/", countries[i], ".csv"))
  Y <- country$battle_deaths[2:1617]
  
  E.1 <- M.est.1.mat[i, ]
  E.2 <- M.est.2.mat[i, ]
  pi.2 <- est.2.mat[i, 5]
  
  V.2 <- (1/pi.2)*E.2
  
  eps.1 <- (Y - E.1)/(sqrt(E.1))
  eps.2 <- (Y - E.2)/(sqrt(V.2))
  
  y.min <- min(eps.1, eps.2)
  y.max <- max(eps.2)
  
  
  plot(2:1617, eps.1, col=cols[i], xlab="t", ylab="std.Pearson residual",
       main=paste0("INARCH(1), ", plot.names[i]), ylim=c(y.min, y.max))
  legend("bottomright", legend=c(paste0("mean=", round(mean(eps.1),2)), 
                              paste0("var=", round(sd(eps.1)^2, 3))),
         cex=1.3)
  plot(2:1617, eps.2, col=cols[i], xlab="t", ylab="std.Pearson residual",
       main=paste0("NB-INGARCH(1,1), ", plot.names[i]), ylim=c(y.min, y.max))
  legend("bottomright", legend=c(paste0("mean=", round(mean(eps.2),2)), 
                              paste0("var=", round(sd(eps.2)^2, 3))),
         cex=1.3)
}
dev.off()

pdf("./figs/res_acf_2models.pdf", width=10, height=15)
par(mfrow=c(4,2))
for (i in 1:4){
  country <- read.csv(paste0("../../data/", countries[i], ".csv"))
  Y <- country$battle_deaths[2:1617]
  
  E.1 <- M.est.1.mat[i, ]
  E.2 <- M.est.2.mat[i, ]
  pi.2 <- est.2.mat[i, 5]
  
  V.2 <- (1/pi.2)*E.2
  
  eps.1 <- (Y - E.1)/(sqrt(E.1))
  eps.2 <- (Y - E.2)/(sqrt(V.2))
  
  acf(eps.1, main=paste0("INARCH(1), ", plot.names[i]), 
      lag.max=20)
  acf(eps.2, main=paste0("NB-INGARCH(1,1), ", plot.names[i]),
      lag.max=20)
}
dev.off()

# PIT histograms

PIT.nb <- function(Y, M, pi){
  U <- seq(0,1, by=0.1)
  F.vec <- rep(0, length(U))
  for (i in 1:length(U)){
    u <- U[i]
    for (j in 1:length(Y)){
      Y.j <- Y[j]
      M.j <- M[j]
      n.j <- (pi/(1-pi))*M.j
      f.t.min1 <- ifelse(Y.j-1<0, 0, pnbinom(Y.j-1, n.j, pi))
      f.t <- pnbinom(Y.j, n.j, pi)
      if (u <= f.t.min1){
        F.t.u <- 0 
      }
      else if (u >= f.t){
        F.t.u <- 1
      }
      else{
        F.t.u <- (u -f.t.min1)/(f.t - f.t.min1)
      }
      F.vec[i] <- F.vec[i] + F.t.u
    }
  }
  F.vec.std <- F.vec/(length(Y))
  hist.height <- F.vec.std[2:length(U)] - F.vec.std[1:(length(U)-1)]
  return(hist.height)
}

PIT.pois <- function(Y, M){
  U <- seq(0,1, by=0.1)
  F.vec <- rep(0, length(U))
  for (i in 1:length(U)){
    u <- U[i]
    for (j in 1:length(Y)){
      Y.j <- Y[j]
      M.j <- M[j]
      f.t.min1 <- ifelse(Y.j-1<0, 0, ppois(Y.j-1, M.j))
      f.t <- ppois(Y.j, M.j)
      if (u <= f.t.min1){
        F.t.u <- 0 
      }
      else if (u >= f.t){
        F.t.u <- 1
      }
      else{
        F.t.u <- (u -f.t.min1)/(f.t - f.t.min1)
      }
      F.vec[i] <- F.vec[i] + F.t.u
    }
  }
  F.vec.std <- F.vec/(length(Y))
  hist.height <- F.vec.std[2:length(U)] - F.vec.std[1:(length(U)-1)]
  return(hist.height)
}

pdf("./figs/pit_2models.pdf", width=10, height=15)
par(mfrow=c(4, 2))
for (i in 1:4){
  country <- read.csv(paste0("../../data/", countries[i], ".csv"))
  Y <- country$battle_deaths[2:1617]
  
  E.1 <- M.est.1.mat[i, ]
  E.2 <- M.est.2.mat[i, ]
  pi.2 <- est.2.mat[i, 5]
  
  h.1 <- PIT.pois(Y, E.1)
  h.2 <- PIT.nb(Y, E.2, pi.2)
  
  barplot(height=h.1, names=seq(0.05, 0.95, by=0.1), ylab="prob",
          main=paste0("INARCH(1), ", plot.names[i]), col=cols[i], yaxt="n")
  axis(2, cex.axis=1.5)
  
  barplot(height=h.2, names=seq(0.05, 0.95, by=0.1), ylab="prob",
          main=paste0("NB-INGARCH(1,1), ", plot.names[i]), col=cols[i], yaxt="n")
  axis(2, cex.axis=1.5)
}
dev.off()

# Plot of sample-paths from NB-INGARCH(1,1)
  
library(scales)
par(mfrow=c(2,2))
for (i in 1:4){
  country <- read.csv(paste0("../../data/", countries[i], ".csv"))
  Y <- country$battle_deaths[2:1617]
  
  M <- M.est.2.mat[i, ]
  pi.2 <- est.2.mat[i, 5]
  M <- c(est.2.mat[i, 4], M)
  
  plot(1:1617, country$battle_deaths, type='l', lty=2, lwd=0.7, col=alpha("black", 0.5))
  lines(1:1617, M, col=cols[i])
}

pdf("./figs/sim_ingarch11.pdf", width=10, height=15)
set.seed(133411)
par(mfrow=c(4,2))
for (i in 1:4){
  country <- read.csv(paste0("../../data/", countries[i], ".csv"))
  Y <- country$battle_deaths
  
  M <- M.est.2.mat[i, ]
  pi.2 <- est.2.mat[i, 5]
  M <- c(est.2.mat[i, 4], M)
  
  Y.sim <- rnbinom(1617, size=(pi.2/(1-pi.2))*M, prob=pi.2)
  y.lim <- c(min(Y, Y.sim), max(Y, Y.sim))
  
  plot(1:1617, country$battle_deaths, type='l', col=cols[i], ylim=y.lim, lwd=0.7,
       main=paste0(plot.names[i], " - Actual Data"), xlab="t", ylab="y_t")
  plot(1:1617, Y.sim, type='l', col=alpha(cols[i], 0.7), ylim=y.lim, lwd=0.7,
       main=paste0(plot.names[i], " - 1 Simulation from (M_t)"), xlab="t", ylab="y_t")
  lines(1:1617, M, lwd=1)
  legend("topright", legend=c("(M_t)", "Simulation from (M_t)"),
         lty=c(1,1), lwd=c(1, 0.7), col=c("black", alpha(cols[i], 0.7)))
}
dev.off()

