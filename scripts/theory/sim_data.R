setwd(dirname(rstudioapi::getSourceEditorContext()$path))

alpha <- 0.4
n <- 1
pi <- 0.4
e.nb <- (n*(1-pi))/pi
var.nb <- (n*(1-pi))/(pi^2)
mu <- e.nb/(1-alpha)
sigma.sq <- (var.nb + alpha*e.nb)/(1-alpha^2)

N.1 <- 100
N.2 <- 1000
r <- 1000

set.seed(149) # 149
y.nb <- rep(NA, r+1+N.2)
y.nb[1] <- round(mu) 
for (i in 2:(r+N.2+1)){
  y.nb[i] <- rbinom(1, size=y.nb[i-1], prob=alpha) + rnbinom(1, size=n, prob=pi)
}

y.sim.1 <- y.nb[(r+1+1):(r+1+N.1)]
y.sim.2 <- y.nb[(r+1+1):(r+1+N.2)]

pdf("./figs/sim_nb_ts.pdf", width=10, height=6)
par(mfrow=c(1,1))
plot(1:N.1, y.sim.1, type='l', ylab=expression(y[t]), xlab="t", col="forestgreen", lty=2,
     cex.lab=1.4, cex.axis=1.2)
points(1:N.1, y.sim.1, col="forestgreen", pch=16)
dev.off()

pdf("./figs/sim_nb_acf.pdf", width=10, height=6)
par(mfrow=c(1,1))
acf(y.sim.1, main="", cex.axis=1.2, cex.lab=1.4, ylab="SACF")
dev.off()

y.mean <- mean(y.sim.1)
y.s.sq <- round(sd(y.sim.1)^2, 2)
p.0 <- sum(y.sim.1 == 0)/N.1

c(y.mean, y.s.sq, p.0)
# 2.56 6.47 0.21

I <- round(y.s.sq/y.mean, 2) 
I.zero <- round(1 + log(p.0)/mean(y.sim.1), 2)

c(I, I.zero)
# 2.53 0.39

####

E.cond.nb <- alpha*y.sim.1[1:(N.1-1)] + (n*(1-pi))/(pi)
V.cond.nb <- alpha*(1-alpha)*y.sim.1[1:(N.1-1)] + (n*(1-pi))/(pi^2)
res.nb <- (y.sim.1[2:N.1] - E.cond.nb)/sqrt(V.cond.nb)

lambda <- mu*(1-alpha)
E.cond.pois <- alpha*y.sim.1[1:(N.1-1)] + lambda
V.cond.pois <- alpha*(1-alpha)*y.sim.1[1:(N.1-1)] + lambda
res.pois <- (y.sim.1[2:N.1] - E.cond.pois)/sqrt(V.cond.pois)

y.lim <- c(min(res.pois), max(res.pois))

pdf("./figs/residuals_theory.pdf", width=10, height=5)
par(mfrow=c(1,2))
plot(2:N.1, res.nb, pch=1, col="darkviolet", xlab="t",
     ylab="std. Pearson residuals [NB-INAR(1)]",
     main=bquote("mean: "~.(round(mean(res.nb),2))~", var"~.(round(sd(res.nb)^2,2))),
     cex.lab=1.3, cex.axis=1.2, ylim=y.lim
     )
#abline(h=c(-1,1), lty=2)
plot(2:N.1, res.pois, pch=1, col="firebrick", xlab="t",
     ylab="std. Pearson residuals [INAR(1)]", 
     main=bquote("mean: "~.(round(mean(res.pois),2))~", var"~.(round(sd(res.pois)^2,2))),
     cex.lab=1.3, cex.axis=1.2, ylim=y.lim
     )
#abline(h=c(-1,1), lty=2)
dev.off()

### pmf and cdf of NB-INAR(1) ###
dnbinar1 <- function(alpha, k, l, n, pi){
  K <- min(k, l)
  p <- 0
  for (j in 0:K){
    p <- p + dbinom(j, size=l, prob=alpha)*dnbinom(k-j, size=n, prob=pi)
  }
  return(p)
}

pnbinar1 <- function(alpha, k, l, n, pi){
  f <- 0
  for (i in 0:k){
    f <- f + dnbinar1(alpha, i, l, n, pi)
  }
  return(f)
}

### pmf and cdf of INAR(1) ###
dinar1 <- function(alpha, k, l, lambda){
  K <- min(k, l)
  p <- 0
  for (j in 0:K){
    p <- p + dbinom(j, size=l, prob=alpha)*dpois(k-j, lambda)
  }
  return(p)
}

pinar1 <- function(alpha, k, l, n, pi){
  f <- 0
  for (i in 0:k){
    f <- f + dinar1(alpha, i, l, lambda)
  }
  return(f)
}

### PIT histograms of NB-INAR(1) simulations ###
h.nb <- matrix(NA, ncol=2, nrow=10)
y.sim.ls <- list(sim.1=y.sim.1, sim.2=y.sim.2)
for (sim in 1:2){
  y.sim <- y.sim.ls[[sim]]
  U <- seq(0, 1, by=0.1)
  F.vec <- rep(0, length(U))
  for (i in 1:length(U)){
    u <- U[i]
    for (j in 2:length(y.sim)){
      y.j <- y.sim[j]
      y.j.min1 <- y.sim[j-1]
      f.t.min1 <- ifelse(y.j-1<0, 0, pnbinar1(alpha, y.j-1, y.j.min1, n, pi))
      f.t <- pnbinar1(alpha, y.j, y.j.min1, n, pi)
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
  F.vec.std <- F.vec/(length(y.sim)-1)
  h.nb[, sim] <- F.vec.std[2:length(U)] - F.vec.std[1:(length(U)-1)]
}

### PIT histograms of NB-INAR(1) simulations ###
h.pois <- matrix(NA, ncol=2, nrow=10)
for (sim in 1:2){
  y.sim <- y.sim.ls[[sim]]
  U <- seq(0, 1, by=0.1)
  F.vec <- rep(0, length(U))
  for (i in 1:length(U)){
    u <- U[i]
    for (j in 2:length(y.sim)){
      y.j <- y.sim[j]
      y.j.min1 <- y.sim[j-1]
      f.t.min1 <- ifelse(y.j-1<0, 0, pinar1(alpha, y.j-1, y.j.min1, lambda))
      f.t <- pinar1(alpha, y.j, y.j.min1, lambda)
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
  F.vec.std <- F.vec/(length(y.sim)-1)
  h.pois[, sim] <- F.vec.std[2:length(U)] - F.vec.std[1:(length(U)-1)]
}

pdf("./figs/pits_100_theory.pdf", width=10, height=4)
par(mfrow=c(1,2))
barplot(height=h.nb[,1], names=seq(0.05, 0.95, by=0.1), ylab="prob",
        main="NB-INAR(1) - 100 simulations", col="darkviolet", cex.lab=1.4)
barplot(height=h.pois[,1], names=seq(0.05, 0.95, by=0.1), ylab="prob",
        main="INAR(1) - 100 simulations", col="firebrick", cex.lab=1.4)
dev.off()

pdf("./figs/pits_1000_theory.pdf", width=10, height=4)
par(mfrow=c(1,2))
barplot(height=h.nb[,2], names=seq(0.05, 0.95, by=0.1), ylab="prob",
        main="NB-INAR(1) - 1000 simulations", col="darkviolet", cex.lab=1.4)
barplot(height=h.pois[,2], names=seq(0.05, 0.95, by=0.1), ylab="prob",
        main="INAR(1) - 100 simulations", col="firebrick", cex.lab=1.4)
dev.off()
