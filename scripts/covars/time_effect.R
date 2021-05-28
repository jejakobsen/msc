setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("prep_data.R")
countries <- c("colombia", "congo", "ethiopia",
               "iraq", "mali", "myanmar",
               "nigeria", "pakistan", "sleone",
               "uganda")
data <- prep_data(countries)

Y <- data[[1]]
x.1 <- data[[2]][, 1]
x.4 <- data[[2]][, 4]

inds.start <- data[[3]]
inds.stop <- data[[4]]

Y.eth <- Y[inds.start[3]:inds.stop[3]]
Y.irq <- Y[inds.start[4]:inds.stop[4]]
x.4.i <- data[[2]][inds.start[3]:inds.stop[3], 4]

unique.years <- unique(x.4)

n.unique <- length(unique.years)

Y.sum.year <- rep(NA, n.unique+1)
Y.sum.eth.year <- rep(NA, n.unique+1)
Y.sum.irq.year <- rep(NA, n.unique+1)

for (i in 1:n.unique){
  year <- unique.years[i]
  year.inds <- which(x.4 == year)
  Y.sum.year[i] <- sum(Y[year.inds])
  Y.sum.eth.year[i] <- sum(Y.eth[which(x.4.i == year)])
  Y.sum.irq.year[i] <- sum(Y.irq[which(x.4.i == year)])
}

Y.sum.year[32] <- Y.sum.year[31]
Y.sum.eth.year[32] <- Y.sum.eth.year[31]
Y.sum.irq.year[32] <- Y.sum.irq.year[31]

pdf("./figs/time_effect_bd.pdf", width=10, height=6)
par(mfrow=c(1,2))
plot(1989:2020, Y.sum.year, type='s', ylab="battle_deaths", xlab="years", cex.lab=1.4)
lines(1989:2020, Y.sum.eth.year, type='s', col="forestgreen", lty=2)
lines(1989:2020, Y.sum.irq.year, type='s', col="firebrick", lty=3, lwd=1.4)
legend("topright", legend=c("Total", "Ethiopia", "Iraq"), 
       col=c("black", "forestgreen", "firebrick"), lwd=c(1, 1, 1.4),
       lty=c(1,2,3), cex=1.4)
plot(1989:2020, Y.sum.year-Y.sum.eth.year-Y.sum.irq.year, type='s', xlab="years",
     ylab="Total - Ethiopia - Iraq", cex.lab=1.4, ylim=c(0, max(Y.sum.year)))
dev.off()