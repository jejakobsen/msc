setwd(dirname(rstudioapi::getSourceEditorContext()$path))

dnbinar1 <- function(alpha, k, l, n, pi){
  K <- min(k, l)
  p <- 0
  for (j in 0:K){
    p <- p + dbinom(j, size=l, prob=alpha)*dnbinom(k-j, size=n, prob=pi)
  }
  return(p)
}

alpha <- 0.2
l <- 200
k <- seq(0,400, by=1)

n <- 0.073 
pi <- 0.0053 

p.1 <- rep(NA, length(k))
p.2 <- rep(NA, length(k))

for (i in 1:length(k)){
  p.1[i] <- dnbinar1(alpha, k[i], l, n, pi)
  p.2[i] <- dbinom(k[i], size=l, prob=alpha)
}

leg.1 <- expression(y[t]*"|"*y[t-1])
leg.2 <- expression(alpha %.% y["t-1"]*"|"*y["t-1"])

pdf("./figs/binom_nbinar.pdf", width=10, height=6)
op <- par(cex = 1.5)
plot(0:(length(p)-1), p.1, type='l', ylim=c(0, max(p.2)),
     xlim=c(0,200), col="blue", ylab="prob", xlab=expression(y[t]), main=expression("|"*y["t-1"]*"=200"))
lines(0:(length(p)-1), p.2, col="red")
legend("topright", legend=c(leg.1, leg.2), lty=c(1,1), col=c("blue", "red"))
dev.off()

