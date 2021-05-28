setwd(dirname(rstudioapi::getSourceEditorContext()$path))

con <- read.csv("../../data/congo.csv")

pdf("./figs/con_bd.pdf", width=10, height=5)
par(mfrow=c(1,1))
plot(1:1617, con$battle_deaths, type='l', col="cornflowerblue",
     xlab="t", ylab="battle_deaths", cex.lab=1.2, cex.axis=1.2,
     main="Congo")
dev.off()
