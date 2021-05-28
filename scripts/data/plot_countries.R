setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd("../../data")

congo <- read.csv("congo.csv")
uganda <- read.csv("uganda.csv")
colombia <- read.csv("colombia.csv")

setwd("../scripts/data")
getwd()

N <- length(congo$battle_deaths)

congo.col <- "cornflowerblue"
uganda.col <- "firebrick"
colombia.col <- "darkgoldenrod1"

y.lab <- "battle_deaths"
x.lab <- "years"
year.inds <- which(congo$week == 1)
year.labs <- congo$year[year.inds]

pdf("./figs/3countries_bd.pdf", width=10, height=7)
par(mfrow=c(3,1))
plot(1:N, congo$battle_deaths, type='l', col=congo.col, 
     main="Congo", ylab=y.lab, xaxt="n", xlab=x.lab)
axis(1, at=year.inds, labels=year.labs)

plot(1:N, uganda$battle_deaths, type='l', col=uganda.col, 
     main="Uganda", ylab=y.lab, xaxt="n", xlab=x.lab)
axis(1, at=year.inds, labels=year.labs)

plot(1:N, colombia$battle_deaths, type='l', col=colombia.col,
     main="Colombia", ylab=y.lab, xaxt="n", xlab=x.lab)
axis(1, at=year.inds, labels=year.labs)
dev.off()

polyarchy.min <- min(congo$v2x_polyarchy, uganda$v2x_polyarchy, colombia$v2x_polyarchy)
polyarchy.max <- max(congo$v2x_polyarchy, uganda$v2x_polyarchy, colombia$v2x_polyarchy)

gdp.min <- min(congo$gdp_pc, uganda$gdp_pc, colombia$gdp_pc)
gdp.max <- max(congo$gdp_pc, uganda$gdp_pc, colombia$gdp_pc)

pop.min <- min(congo$pop_tot, uganda$pop_tot, colombia$pop_tot)
pop.max <- max(congo$pop_tot, uganda$pop_tot, colombia$pop_tot)

pdf("./figs/3countries_pa.pdf", width=10, height=6)
par(mfrow=c(1,1))
plot(1:N, congo$v2x_polyarchy, type='l', col=congo.col, 
     main="", ylab="v2x_polyarchy", xaxt="n", xlab=x.lab,
     ylim=c(polyarchy.min, polyarchy.max))
lines(1:N, uganda$v2x_polyarchy, col=uganda.col)
lines(1:N, colombia$v2x_polyarchy, col=colombia.col)
axis(1, at=year.inds, labels=year.labs)
legend("bottomright", legend=c("congo", "uganda", "colombia"),
       lty=c(1,1,1), col=c(congo.col, uganda.col, colombia.col))
dev.off()

pdf("./figs/3countries_wbi.pdf", width=10, height=6)
par(mfrow=c(1,2))
plot(1:N, congo$gdp_pc, type='l', col=congo.col, 
     main="", ylab="gdp_pc", xaxt="n", yaxt="n", xlab=x.lab,
     ylim=c(gdp.min, gdp.max))
lines(1:N, uganda$gdp_pc, col=uganda.col)
lines(1:N, colombia$gdp_pc, col=colombia.col)
axis(1, at=year.inds, labels=year.labs)
axis(2, at=c(500, 2000, 4000, 6000, 7000), 
     labels=c("500", "2k", "4k", "6k", "7k"))
legend("topleft", legend=c("congo", "uganda", "colombia"),
       lty=c(1,1,1), col=c(congo.col, uganda.col, colombia.col))

plot(1:N, congo$pop_tot, type='l', col=congo.col, 
     main="", ylab="pop_tot", xaxt="n", xlab=x.lab,
     ylim=c(pop.min, pop.max), yaxt="n")
lines(1:N, uganda$pop_tot, col=uganda.col)
lines(1:N, colombia$pop_tot, col=colombia.col)
axis(1, at=year.inds, labels=year.labs)
axis(2, at=c(20000000, 40000000, 60000000, 80000000), 
     labels=c("20M", "40M", "60M", "80M"))
legend("topleft", legend=c("congo", "uganda", "colombia"),
       lty=c(1,1,1), col=c(congo.col, uganda.col, colombia.col))
dev.off()
