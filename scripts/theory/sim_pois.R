setwd(dirname(rstudioapi::getSourceEditorContext()$path))

alphas <- c(0.2, 0.8)
lambda <- 3
mu <- lambda/(1-alphas)

N <- 30

set.seed(1346) # 149
y.1 <- rep(NA, N)
y.2 <- rep(NA, N)
y.1[1] <- rpois(1, mu[1])
y.2[1] <- rpois(1, mu[2])
for (i in 2:N){
  y.1[i] <- rbinom(1, size=y.1[i-1], prob=alphas[1]) + rpois(1, lambda)
  y.2[i] <- rbinom(1, size=y.2[i-1], prob=alphas[2]) + rpois(1, lambda)
}

pdf("./figs/sim_pois_ts.pdf", width=10, height=5)
par(mfrow=c(1,2))
plot(1:N, y.1, type='l', ylab=expression(y[t]), xlab="t", col="firebrick", lty=2, cex.lab=1.4, cex.axis=1.2,
     main=expression(paste(alpha, "=0.2, ", lambda, "=3")), cex.main=1.4)
points(1:N, y.1, col="firebrick", pch=16)
plot(1:N, y.2, type='l', ylab=expression(y[t]), xlab="t", col="skyblue", lty=2, cex.lab=1.4, cex.axis=1.2,
     main=expression(paste(alpha, "=0.8, ", lambda, "=3")), cex.main=1.4)
points(1:N, y.2, col="skyblue", pch=16)
dev.off()