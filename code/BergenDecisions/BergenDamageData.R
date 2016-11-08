rm(list=ls())
A <- read.csv("StormfloYearFylkeAntall.csv", header=TRUE)
A <- A[-37,] ## remove 2016 as data not complete
V <- read.csv("StormfloYearFylkeVerdi.csv", header=TRUE)
V <- V[-37,] ## remove 2016 as data not complete
CPI <- read.csv("KPI.csv", header=TRUE)
CPI <- CPI[-1,] ## remove 2016 as data not yet available
CPI$CPI <- CPI$CPI/CPI$CPI[1] ## normalize according to 2015 

## plot data for Hordaland and Rogaland
png(file="DamagesAmounts.png", width=720, height=480, points=18)
par(mfrow=c(2,3),mex=0.75)
plot(A$Hordaland, V$Hordaland/CPI$CPI[1:36], main="Hordaland 1980-2015", xlab="Nr. of damages", ylab="Amount (1000 NOK)", ylim=c(0,65000), xlim=c(0,500))
hist(A$Hordaland, main="Hordaland 1980-2015", xlab="Nr. of damages", col="gray60", breaks=seq(0,500,by=10), xlim=c(0,500), ylim=c(0,18))
hist(V$Hordaland/CPI$CPI[1:36], main="Hordaland 1980-2015", xlab="Amount (1000 NOK)", col="gray60", breaks=50, xlim=c(0,65000), ylim=c(0,22)) 

plot(A$Rogaland, V$Rogaland/CPI$CPI[1:36], main="Rogaland 1980-2015", xlab="Nr. of damages", ylab="Amount (1000 NOK)", ylim=c(0,65000), xlim=c(0,500))
hist(A$Rogaland, main="Rogaland 1980-2015", xlab="Nr. of damages", col="gray60", breaks=seq(0,500,by=10), xlim=c(0,500), ylim=c(0,18))
hist(V$Rogaland/CPI$CPI[1:36], main="Rogaland 1980-2015", xlab="Amount (1000 NOK)", col="gray60", breaks=50, xlim=c(0,65000), ylim=c(0,22)) 
dev.off()

### Combine Hordaland and Rogaland data
sf.nr <- c(A$Hordaland, A$Rogaland)
sf.damage <- c(V$Hordaland/CPI$CPI[1:36], V$Rogaland/CPI$CPI[1:36])
x11()
hist(sf.damage, main="Rogaland and Hordaland 1980-2015", xlab="Damage amount per year (1000 NOK)", col="gray60", breaks=50) 

#######################################################################
## Fitting Burr distribution to combined damage data from Hordaland and Rogaland
library(actuar)

ind <- which(sf.damage==0)
sf.damage[ind] <- 0.1

log.lik <- function(theta)
{
    res <- sum(dburr(sf.damage/1e3, shape1=theta[1], shape2=theta[2], rate=theta[3], log=TRUE))
    return(-res)
}

theta.start <- c(0.2, 1.5, 1.5)

optim.out <- optim(theta.start, fn=log.lik, method="BFGS", control=list(maxit=1000))

## plot data and fitted distribution
## png(file="../../submission/DamageDistribution.png", width=480, height=480, points=12)
pdf(file="../../submission/DamageDistribution.pdf", width=5, height=5, points=12)
par(mex=0.75)
hist(sf.damage/1e3, main="", xlab="Amount (million NOK)", col="gray60", breaks=50, xlim=c(0,65), freq=FALSE)
x <- seq(0, 65, by=0.01)
y <- dburr(x, shape1=optim.out$par[1], shape2=optim.out$par[2], rate=optim.out$par[3])
lines(x,y,col="red",lwd=2)
dev.off()

#######################################################################
## Get data on change in damage cost as a function for sea level rise from Hallegatte et al. (2013)
E <- read.csv("EuropeanLossData.csv", header=TRUE)
E[,2:4] <- E[,2:4]/E[,2]

png(file="EuropeanIncreaseLoss.png", width=480, height=480, points=14)
par(mex=0.75)
x <- c(0,20,40)
plot(x, E[1, 2:4], type="o", col="gray60", ylim=c(1,max(E[,4])), xlim=c(0,40), xlab="Sea level rise (cm)", ylab="Relative mean annual damage", main="Hallegatte et al. (2013) for 15 European cities")
for(i in 2:15) lines(x, E[i, 2:4], type="o", col="gray60")
points(20, mean(E[,3]), col="red", pch=15)
points(40, mean(E[,4]), col="red", pch=15)
dev.off()

## Conservative extrapolation: Linear extrapolation of the increase from 20 to 40 cm
## and the decrease for below 0 cm 
res <- lm(apply(E[,3:4],2,mean)~x[2:3])

png(file="EuropeanIncreaseLossExtrapolation.png", width=720, height=480, points=14)
par(mex=0.75)
x <- c(0,20,40)
plot(x, E[1, 2:4], type="o", col="gray60", xlim=c(0,100), ylim=c(0,250),  xlab="Sea level rise (cm)", ylab="Relative mean annual damage", main="Hallegatte et al. (2013) for 15 European cities")
for(i in 2:15) lines(x, E[i, 2:4], type="o", col="gray60")
points(20, mean(E[,3]), col="red", pch=15)
points(40, mean(E[,4]), col="red", pch=15)
lines(c(0,20),c(1,mean(E[,3])), col="red",lwd=2)
z <- c(20,100)
lines(z, res$coef[1] + z * res$coef[2], col="red", lwd=2)
dev.off()

## Uncertainty assessment by performing linear extrapolation per city
pdf(file="../../submission/EuropeanIncreaseLossExtrapolationUncertainty.pdf", width=5, height=5, points=12)
## png(file="EuropeanIncreaseLossExtrapolationUncertainty.png", width=480, height=480, points=14)
par(mex=0.75)
x <- c(0,20,40)
plot(x, E[1, 2:4], type="o", col="black", xlim=c(-100,130), ylim=c(-180,1000),  xlab="Sea level anomaly (cm)", ylab="Relative mean annual damage", main=" ", axes=FALSE)
box()
axis(1)
axis(2, at=c(-200, 1, 200, 400, 600, 800, 1000), labels=c(0.005, 1, 200, 400, 600, 800, 1000))
z <- c(40,130)
w <- c(-100,0)
for(i in 1:15)
{
    lines(x, E[i, 2:4], type="o", col="black")
    res <- lm(as.double(E[i,3:4])~x[2:3])
    lines(z, res$coef[1] + z * res$coef[2], col="gray70")
    res <- lm(as.double(E[i,2:3])~x[1:2])
    lines(w, res$coef[1] + w * res$coef[2], col="gray70")
}
points(20, median(E[,3]), col="red", pch=15)
points(40, median(E[,4]), col="red", pch=15)
res <- lm(apply(E[,2:3],2,median)~x[1:2])
w <- c(-100, 20)
lines(w, res$coef[1] + w * res$coef[2], col="red", lwd=2)
res <- lm(apply(E[,3:4],2,median)~x[2:3])
z <- c(20,130)
lines(z, res$coef[1] + z * res$coef[2], col="red", lwd=2)
dev.off()

## Getting all slopes
x <- c(0,20,40)
low.slope <- rep(NA, 15)
high.slope <- rep(NA, 15)
for(i in 1:15)
{
    res <- lm(apply(E[i,2:3],2,mean)~x[1:2])
    low.slope[i] <- res$coef[2]
    res <- lm(apply(E[i,3:4],2,mean)~x[2:3])
    high.slope[i] <- res$coef[2]
}

## Function that calculates the multiplicative change in damage for sea level x and ensemble member k 
get.damage.mult <- function(x, k)
{
    if(x < 0) return(-1/(x * low.slope[k]))
    else if(x < 20) return(x * low.slope[k])
    else return(E[k,3] + x * high.slope[k])
}

## Get sea level rise projections for Bergen 
load("Simulation.Rdata")

## Calculate yearly damage distributions under local sea level projections
I <- 10000
yearly.damage <- array(NA, dim=c(I, 85))
orig.damage <- array(rburr(I*85, shape1=optim.out$par[1], shape2=optim.out$par[2], rate=optim.out$par[3]), dim=c(I, 85))
damage.scenario <- sample(1:15, I, replace=TRUE)
for(i in 1:I)
{
    for(j in 1:85)
    {
        yearly.damage[i,j] <- orig.damage[i,j] * get.damage.mult(sim[i, (j+16)], damage.scenario[i])
    }
}

## Calculate yearly damage distributions under no-change compared to 2015 level
constant.damage <- array(NA, dim=c(I, 85))
for(i in 1:I)
{
    for(j in 1:85)
    {
        constant.damage[i,j] <- orig.damage[i,j] * get.damage.mult(sim[i, 16], damage.scenario[i])
    }
}

## Various related plots 
x11()
median.damage <- upper.damage <- lower.damage <- rep(NA, 85)
median.damage <- apply(yearly.damage, 2, median)
upper.damage <- apply(yearly.damage, 2, quantile, 1-0.125)
lower.damage <- apply(yearly.damage, 2, quantile, 0.125)
years <- c(2016:2100)
plot(years, upper.damage, col="gray60", type="l")
lines(years, median.damage, lwd=2)
lines(years, lower.damage, col="gray60", type="l")

png(file="FutureLoss.png", width=480, height=480, points=14)
par(mex=0.75)
boxplot(yearly.damage/1000, ylim=c(0,9e5/1000), outline=FALSE, range=1.5, border="gray20", col="gray80", ylab="Annual damage (million NOK)", xlab="Year", axes=FALSE)
box(which="inner")
axis(2)
ind <- c(5, 25, 45, 65, 85)
axis(1, at=ind, labels=years[ind])
dev.off()

png(file="AccumulatedFutureLoss.png", width=480, height=480, points=14)
par(mex=0.75)
cum.damage <- apply(yearly.damage,1,sum)
hist(log(cum.damage/1000), main="", xlab="Total damage 2016-2100 (log 1e6 NOK)", col="gray60", breaks=100, freq=FALSE, xlim=c(5, 25)) 
dev.off()

## Cumulated total damage costs under sea level rise 
cumsum.damage <- array(NA, dim=dim(yearly.damage))
for(i in 1:dim(yearly.damage)[1]) 
    cumsum.damage[i,] <- cumsum(yearly.damage[i,])

upper.cumsum <- apply(cumsum.damage,2,quantile,0.95)
lower.cumsum <- apply(cumsum.damage,2,quantile,0.05)

png(file="CumulatedFutureLoss95.png", width=480, height=480, points=14)
par(mex=0.75)
plot(2016:2100, log(upper.cumsum/1000),type="l", ylim=c(0,15), xlab="Year", ylab="Accumulated damage in log 1e6 NOK", main="")
dev.off()

png(file="CumulatedFutureLoss5.png", width=480, height=480, points=14)
par(mex=0.75)
plot(2016:2100, log(lower.cumsum/1000),type="l", ylim=c(0,15),xlab="Year", ylab="Accumulated damage in log 1e6 NOK", main="")
dev.off()

png(file="CumulatedFutureLoss5_95.png", width=480, height=480, points=14)
par(mex=0.75)
plot(2016:2100, log(upper.cumsum/1000),type="l", ylim=c(0,15), xlab="Year", ylab="Accumulated damage in log 1e6 NOK", main="")
lines(2016:2100, log(lower.cumsum/1000))
dev.off()

## Cumulated additional damage cost due to sea level rise
add.damage <- yearly.damage - constant.damage
cumsum.add.damage <- array(NA, dim=dim(add.damage))
for(i in 1:dim(add.damage)[1]) 
    cumsum.add.damage[i,] <- cumsum(add.damage[i,])
upper.add.cumsum <- apply(cumsum.add.damage,2,quantile,0.9)
lower.add.cumsum <- apply(cumsum.add.damage,2,quantile,0.1)

png(file="CumulatedFutureAdditionalLoss5_95.png", width=480, height=480, points=14)
par(mex=0.75)
plot(2016:2100, upper.add.cumsum/1e6/(1:85),type="l", ylim=c(0,15), xlab="Year",
     ylab="Accumulated damage in billion NOK", main="")
lines(2016:2100, lower.add.cumsum/1e6/(1:85))
dev.off()

png(file="CumulatedFutureAdditionalLoss5_95.png", width=480, height=480, points=14)
par(mex=0.75)
x11()
plot(2016:2100, upper.add.cumsum/1e3/1.02^(1:85),type="l", ylim=c(0,15), xlab="Year",
      ylab="Accumulated damage in billion NOK", main="")
lines(2016:2100, lower.add.cumsum/1e3/1.02^(1:85))
lines(2016:2100, 1.1/1.02^(1:85), col="red")
dev.off()

x11()
plot(2016:2100, upper.add.cumsum/1e3/1.02^(1:85),type="l", ylim=c(0,20), xlab="Year",
      ylab="Accumulated damage in billion NOK", main="", xlim=c(2016,2040))
lines(2016:2100, lower.add.cumsum/1e3/1.02^(1:85))
lines(2016:2100, 1.1/1.02^(1:85), col="red")
