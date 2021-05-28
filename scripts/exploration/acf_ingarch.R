setwd(dirname(rstudioapi::getSourceEditorContext()$path))

acf.ingarch <- function(alpha, beta, lag){
  # Returns the acf of an INGARCH(1,1) evaluated at alpha and beta, 
  # and one or several lags
  const <- alpha*(1-beta*(alpha+beta))/(1-(alpha+beta)^2 + alpha^2)
  rho <- const*(alpha + beta)^(lag-1) 
  return(rho)
}

# Compute acf with fixed alpha and varying beta

lags <- seq(1, 15, by=1)

a <- 0.8
b.1 <- 0.05
b.2 <- 0.1
b.3 <- 0.15

rho.a.1 <- acf.ingarch(a, b.1, lags)
rho.a.2 <- acf.ingarch(a, b.2, lags)
rho.a.3 <- acf.ingarch(a, b.3, lags)

# Compute acf with fixed beta and varying alpha

b <- 0.8
a.1 <- 0.05
a.2 <- 0.1
a.3 <- 0.15

rho.b.1 <- acf.ingarch(a.1, b, lags)
rho.b.2 <- acf.ingarch(a.2, b, lags)
rho.b.3 <- acf.ingarch(a.3, b, lags)

# Plot acf's

pdf("./figs/acf_ingarch11.pdf", width=10, height=5)
par(mfrow=c(1, 2))
plot(lags, rho.a.1, type='l', ylim=c(0, 1), xlab="lag", ylab="acf",
     main=expression(alpha==0.8), cex.main=1.4, lty=3)
lines(lags, rho.a.2, lty=2)
lines(lags, rho.a.3, lty=1)
par(cex = 1.4) # change fontsize of legend
legend("topright", legend=c(expression(beta==0.05), expression(beta==0.1), expression(beta==0.15)),
       lty=c(3, 2, 1))
par(cex=1) # change fontsize back

plot(lags, rho.b.1, type='l', ylim=c(0, 1), xlab="lag", ylab="acf",
     main=expression(beta==0.8), cex.main=1.4, lty=3)
lines(lags, rho.b.2, lty=2)
lines(lags, rho.b.3, lty=1)
par(cex = 1.4) # change fontsize of legend
legend("topright", legend=c(expression(alpha==0.05), expression(alpha==0.1), expression(alpha==0.15)),
       lty=c(3, 2, 1))
dev.off()



