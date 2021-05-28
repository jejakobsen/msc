setwd(dirname(rstudioapi::getSourceEditorContext()$path))

irq <- read.csv("../../data/iraq.csv")
con <- read.csv("../../data/congo.csv")
mmr <- read.csv("../../data/myanmar.csv")

con.col <- "cornflowerblue"
mmr.col <- "chartreuse4"
irq.col <- "black"

y.lab <- "battle_deaths"
x.lab <- "years"

year.inds <- which(con$week == 1)
year.labs <- con$year[year.inds]

pdf("./figs/3countries_bd.pdf", width=10, height=7)
par(mfrow=c(3,1))
plot(1:1617, irq$battle_deaths, col=irq.col, type='l', main="Iraq",
     xaxt="n", xlab=x.lab, ylab=y.lab, cex.lab=1.4, cex.axis=1.4, cex.main=1.8)
axis(1, at=year.inds, labels=year.labs)
plot(1:1617, con$battle_deaths, col=con.col, type='l', main="Congo",
     xaxt="n", xlab=x.lab, ylab=y.lab, cex.lab=1.4, cex.axis=1.4, cex.main=1.8)
axis(1, at=year.inds, labels=year.labs)
plot(1:1617, mmr$battle_deaths, col=mmr.col, type='l', main="Myanmar",
     xaxt="n", xlab=x.lab, ylab=y.lab, cex.lab=1.4, cex.axis=1.4, cex.main=1.8)
axis(1, at=year.inds, labels=year.labs)
dev.off()

