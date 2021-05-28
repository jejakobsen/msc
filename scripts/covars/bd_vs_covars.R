setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("prep_data.R")
countries <- c("colombia", "congo", "ethiopia",
               "mali", "myanmar", "nigeria",
               "iraq", "pakistan", "sleone",
               "uganda")
country.labs <- c("COL", "CON", "ETH", 
                  "MLI", "MMR", "NIG",
                  "IRQ", "PAK", "SLE",
                  "UGA")
data <- prep_data(countries)
Y <- data[[1]]
X <- data[[2]]
inds.start <- data[[3]]
inds.stop <- data[[4]]

Y.mean <- rep(NA, 10)
x.1.mean <- rep(NA, 10)
x.2.mean <- rep(NA, 10)
x.3.mean <- rep(NA, 10)

for (i in 1:10){
  x.1.mean[i] <- mean(X[inds.start[i]:inds.stop[i], 1])
  x.2.mean[i] <- mean(X[inds.start[i]:inds.stop[i], 2])
  x.3.mean[i] <- mean(X[inds.start[i]:inds.stop[i], 3])
  Y.mean[i] <- mean(Y[inds.start[i]:inds.stop[i]])
}

Y.std.mean <- (Y.mean - mean(Y.mean))/sd(Y.mean)

y.lab <- bquote(bar(y)["i"]~"'")
x.lab.1 <- bquote(bar(x)["1,i"]~"'")
x.lab.2 <- bquote(bar(x)["2,i"]~"'")
x.lab.3 <- bquote(bar(x)["3,i"]~"'")
cex.text <- 1.5
cex.labs <- 1.8

pdf("./figs/bd_vs_covars.pdf", width=10, height=6)
par(mfrow=c(1,3))
plot(x.1.mean, Y.std.mean, ylab="", xlab=x.lab.1, cex.lab=cex.labs)
title(ylab=y.lab, line=2, cex.lab=1.8)
text(x.1.mean, Y.std.mean, labels=country.labs, pos=3, cex=cex.text)
plot(x.2.mean, Y.std.mean, ylab="", xlab=x.lab.2, cex.lab=cex.labs)
title(ylab=y.lab, line=2, cex.lab=1.8)
text(x.2.mean[1:8], Y.std.mean[1:8], labels=country.labs[1:8], pos=3, cex=cex.text)
text(x.2.mean[9], Y.std.mean[9], labels=country.labs[9], pos=2, cex=cex.text)
text(x.2.mean[10], Y.std.mean[10], labels=country.labs[10], pos=4, cex=cex.text)
plot(x.3.mean, Y.std.mean, ylab="", xlab=x.lab.3, cex.lab=cex.labs)
title(ylab=y.lab, line=2, cex.lab=1.8)
text(x.3.mean, Y.std.mean, labels=country.labs, pos=3, cex=cex.text)
dev.off()