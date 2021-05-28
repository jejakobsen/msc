setwd(dirname(rstudioapi::getSourceEditorContext()$path))

uga <- read.csv("../../data/uganda.csv")
sle <- read.csv("../../data/sleone.csv")

uga.col <- "firebrick"
sle.col <- "royalblue1"

pdf("./figs/sle_uga_bd.pdf", width=10, height=6)
par(mfrow=c(1,2))
plot(1:1617, uga$battle_deaths, type='l', ylab="battle_deaths", xlab="t", cex.lab=1.4,
     cex.axis=1.2, main="Uganda", col=uga.col)
plot(1:1617, sle$battle_deaths, type='l', ylab="battle_deaths", xlab="t", cex.lab=1.4,
     cex.axis=1.2, main="Sierra Leone", col=sle.col)
dev.off()
