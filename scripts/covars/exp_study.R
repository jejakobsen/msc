setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("prep_data.R")
countries <- c("colombia", "congo", "ethiopia",
               "iraq", "mali", "myanmar",
               "nigeria", "pakistan", "sleone",
               "uganda")
data <- prep_data(countries)

x.1 <- data[[2]][, 1]
x.2 <- data[[2]][, 2]
x.3 <- data[[2]][, 3]
x.4 <- data[[2]][, 4]

x.1.mm <- seq(min(x.1), max(x.1), by=0.01)
x.2.mm <- seq(min(x.2), max(x.2), by=0.01)
x.3.mm <- seq(min(x.3), max(x.3), by=0.01)
x.4.mm <- seq(min(x.4), max(x.4), by=0.01)

f.1 <- exp(-(-1.38*x.1.mm + 1.04*x.1.mm^2))
f.2 <- exp(-(6.22*x.2.mm))
f.3 <- exp(-(4.02*x.3.mm))
f.4 <- exp(-(-4.12*x.4.mm)) 

pdf("./figs/exp_study.pdf", width=10, height=8)
par(mfrow=c(2,2))
plot(x.1.mm, f.1, type='l', main="",
     xlab="std. v2x_polyarchy", ylab="", cex.lab=1.6, cex.axis=1.3)
title(ylab=expression(h["pa"]), line=2, cex.lab=1.6)
plot(x.2.mm, f.2, type='l', main="",
     xlab="std. log(gdp_pc)", ylab="", cex.lab=1.6, cex.axis=1.3)
title(ylab=expression(h["gdp"]), line=2, cex.lab=1.6)
plot(x.3.mm, f.3, type='l', main="",
     xlab="std. log(pop_tot)", ylab="", cex.lab=1.6, cex.axis=1.3)
title(ylab=expression(h["pop"]), line=2, cex.lab=1.6)
plot(x.4.mm, f.4, type='l', main="",
     xlab="std. years", ylab="", cex.lab=1.6, cex.axis=1.3)
title(ylab=expression(h["time"]), line=2, cex.lab=1.6)
dev.off()
