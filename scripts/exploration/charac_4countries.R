setwd(dirname(rstudioapi::getSourceEditorContext()$path))

setwd("../../data")

uganda <- read.csv("uganda.csv")
colombia <- read.csv("colombia.csv")
congo <- read.csv("congo.csv")
ethiopia <- read.csv("ethiopia.csv")

setwd("../scripts/exploration")

bd.con <- congo$battle_deaths
bd.uga <- uganda$battle_deaths
bd.col <- colombia$battle_deaths
bd.eth <- ethiopia$battle_deaths

# Plot of battle_deaths (Ethiopia)
year.inds <- which(ethiopia$week == 1)
year.labs <- ethiopia$year[year.inds]

pdf("./figs/bd_4countries.pdf", width=10, height=7)
par(mfrow=c(2, 2))
plot(1:length(bd.col), bd.col, type='l', col="darkgoldenrod1", xaxt="n",
     xlab="years", ylab="battle_deaths", main="Colombia")
axis(1, at=year.inds, labels=year.labs)
plot(1:length(bd.uga), bd.uga, type='l', col="firebrick", xaxt="n",
     xlab="years", ylab="battle_deaths", main="Uganda")
axis(1, at=year.inds, labels=year.labs)
plot(1:length(bd.con), bd.con, type='l', col="cornflowerblue", xaxt="n",
     xlab="years", ylab="battle_deaths", main="Congo")
axis(1, at=year.inds, labels=year.labs)
plot(1:length(bd.eth), bd.eth, type='l', col="darkgreen", xaxt="n",
     xlab="years", ylab="battle_deaths", main="Ethiopia")
axis(1, at=year.inds, labels=year.labs)
dev.off()

# SACF plots of battle_deaths

lag_max <- 15
y.lab <- "sacf"

pdf("./figs/sacf_4countries.pdf", width=10, height=8)
par(mfrow=c(2,2))
acf(bd.col, lag.max=lag_max, ylab=y.lab,
    main="Colombia")
acf(bd.uga, lag.max=lag_max, ylab=y.lab,
    main="Uganda")
acf(bd.con, lag.max=lag_max, ylab=y.lab, 
    main="Congo")
acf(bd.eth, lag.max=lag_max, ylab=y.lab, 
    main="Ethiopia")
dev.off()

# Overdispersion index of data
I.col <- round(sd(bd.col)^2/mean(bd.col), 1)
I.uga <- round(sd(bd.uga)^2/mean(bd.uga), 1)
I.con <- round(sd(bd.con)^2/mean(bd.con), 1)
I.eth <- round(sd(bd.eth)^2/mean(bd.eth), 1) 

c(I.col, I.uga, I.con, I.eth)

# Zero Inflation index of data
N <- length(bd.con)

p.0.col <- sum(bd.col == 0)/N
p.0.uga <- sum(bd.uga == 0)/N
p.0.con <- sum(bd.con == 0)/N
p.0.eth <- sum(bd.eth == 0)/N

I.zero.col <- round(1 + log(p.0.col)/mean(bd.col), 3)
I.zero.uga <- round(1 + log(p.0.uga)/mean(bd.uga), 3)
I.zero.con <- round(1 + log(p.0.con)/mean(bd.con), 3)
I.zero.eth <- round(1 + log(p.0.eth)/mean(bd.eth), 3)

c(I.zero.col, I.zero.uga, I.zero.con, I.zero.eth)

