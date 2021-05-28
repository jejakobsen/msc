setwd(dirname(rstudioapi::getSourceEditorContext()$path))

I.nb.inar.fn <- function(pi, alpha){
  I <- (1/pi + alpha)/(1 + alpha)
  return(I)
}

I.nb.inarch.fn <- function(pi, alpha){
  I <- (1/pi)/(1 - alpha^2)
  return(I)
}

I.gp.inar.fn <- function(theta, alpha){
  I <- (1/(1-theta)^2 + alpha)/(1 + alpha)
  return(I)
}

I.gp.inarch.fn <- function(theta, alpha){
  I <- (1/(1-theta)^2)/(1 - alpha^2)
  return(I)
}

pi.fix <- 0.025
a.seq <- seq(0.0, 0.9, by=0.01)
I.nb.inar <- I.nb.inar.fn(pi.fix, a.seq)
I.nb.inarch <- I.nb.inarch.fn(pi.fix, a.seq)

#theta.fix <- 0.975
#I.gp.inar <- I.gp.inar.fn(theta.fix, a.seq)
#I.gp.inarch <- I.gp.inarch.fn(theta.fix, a.seq)
pdf("./figs/disp_nb.pdf", width=10, height=5)
par(mfrow=c(1,1))
par(cex = 1.4)
plot(a.seq, I.nb.inarch, type='l', 
     ylim=c(1, max(I.nb.inarch)),
     xlab=expression(alpha), ylab="Dispersion Index",
     main=expression(pi==0.025), col="red", lwd=2)
lines(a.seq, I.nb.inar, col="blue", lwd=2)
legend("topleft", legend=c("NB-INARCH(1)", "NB-INAR(1)"),
       col=c("red", "blue"), lty=c(1, 1), lwd=c(2,2))
dev.off()

#plot(a.seq, I.gp.inarch, type='l', 
#     ylim=c(1, max(I.gp.inarch)),
#     xlab=expression(alpha), ylab="Dispersion Index",
#     main=expression(theta==0.975), col="red", lwd=2)
#lines(a.seq, I.gp.inar, col="blue", lwd=2)
#legend("topleft", legend=c("GP-INARCH(1)", "GP-INAR(1)"),
#       col=c("red", "blue"), lty=c(1, 1), lwd=c(2,2))

########################################################

a.fix <- 0.3
pi.v <- seq(0.05, 0.5, by=0.01)
theta.v <- seq(0.5, 0.95, by=0.01)

I.nb.inar.a.fix <- I.nb.inar(pi.v, a.fix)
I.nb.inarch.a.fix <- I.nb.inarch(pi.v, a.fix)

I.gp.inar.a.fix <- I.gp.inar(theta.v, a.fix)
I.gp.inarch.a.fix <- I.gp.inarch(theta.v, a.fix)

a.v <- seq(0.05, 0.5, by=0.01)
pi.fix <- 0.2
theta.fix <- 0.8

I.nb.inar.a.v <- I.nb.inar(pi.fix, a.v)
I.nb.inarch.a.v <- I.nb.inarch(pi.fix, a.v)

I.gp.inar.a.v <- I.gp.inar(theta.fix, a.v)
I.gp.inarch.a.v <- I.gp.inarch(theta.fix, a.v)

par(mfrow=c(2,2))
plot(pi.v, I.nb.inar.a.fix, type='l', ylim=c(min(I.nb.1), max(I.nb.2)))
lines(pi.v, I.nb.inarch.a.fix)

plot(theta.v, I.gp.inar.a.fix, type='l', ylim=c(min(I.gp.1), max(I.gp.2)))
lines(theta.v, I.gp.inarch.a.fix)

plot(a.v, I.nb.inar.a.v, type='l', 
     ylim=c(min(I.nb.inar.a.v), max(I.nb.inarch.a.v)))
lines(a.v, I.nb.inarch.a.v)

plot(a.v, I.gp.inar.a.v, type='l', 
     ylim=c(min(I.gp.inar.a.v), max(I.gp.inarch.a.v)))
lines(a.v, I.gp.inarch.a.v)


#I.col <- 31.9
#I.eth <- 3493.6

#pi.col <- 1/(I.col*(1-0.3^2))
#pi.eth <- 1/(I.eth*(1-0.3^2))

#theta.col <- 1 - sqrt(1/(I.col*(1-0.3^2)))
#theta.eth <- 1 - sqrt(1/(I.eth*(1-0.3^2)))

#c(pi.col, pi.eth)
#c(theta.col, theta.eth)
