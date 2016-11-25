rm(list=ls())

### Read in damage data obtained from Finance Norway's website:
### www.finansnorge.no/statistikk/skadeforsikring/Naturskadestatistikk-NASK/
A <- read.csv("StormfloYearFylkeAntall.csv", header=TRUE)
A <- A[-37,] ## Remove 2016 as data not complete
V <- read.csv("StormfloYearFylkeVerdi.csv", header=TRUE)
V <- V[-37,] ## Remove 2016 as data not complete

### Read in consumer price index (CPI).
CPI <- read.csv("KPI.csv", header=TRUE)
CPI <- CPI[-1,] ## Remove 2016 as data not yet available
CPI$CPI <- CPI$CPI/CPI$CPI[1] ## Normalize to 2015 

### Combine Hordaland and Rogaland data.
### Standardize costs to 2015 values using CPI. 
sf.nr <- c(A$Hordaland, A$Rogaland)
sf.damage <- c(V$Hordaland/CPI$CPI[1:36], V$Rogaland/CPI$CPI[1:36])
sf.damage <- sf.damage/1e3 ## Normalize to million NOK. 

### Fit Burr distribution to damage data (in MNOK)  
library(actuar)
ind <- which(sf.damage==0)
sf.damage[ind] <- 1e-04 ## Improves numerical stability of estimates. 
log.lik <- function(theta)
{
  res <- sum(dburr(sf.damage, 
                   shape1=theta[1], 
                   shape2=theta[2], 
                   rate=theta[3], 
                   log=TRUE))
  return(-res)
}
theta.start <- c(5, 0.5, 0.05)
optim.out <- optim(theta.start, 
                   fn=log.lik, 
                   method="BFGS", 
                   control=list(maxit=1000))

### Get Hallegatte et al. (2013) data on relation between change in damage 
### cost and change in sea level (csv files copied manually from
### online supplementary information).   
E <- read.csv("EuropeanLossData.csv", header=TRUE)
E[,2:4] <- E[,2:4]/E[,2] 

### Hallegatte et al. (2013) provide data for 0, 20, 40 cm sea level rise.
### Conservative extrapolation: Linear extrapolation of [20,40] beyond 40
### and [0,20] below 0. 
x <- c(0, 20, 40)
low.slope <- rep(NA, 15)
high.slope <- rep(NA, 15)
for(i in 1:15)
{
  res <- lm(apply(E[i,2:3],2,mean)~x[1:2])
  low.slope[i] <- res$coef[2]
  res <- lm(apply(E[i,3:4],2,mean)~x[2:3])
  high.slope[i] <- res$coef[2]
}

### Function that calculates the multiplicative change in damage for sea level x 
### and Hallegatte et al. (2013) estimates for city k.  
get.damage.mult <- function(x, k)
{
  if(x < 0) return(-1/(x * low.slope[k]))
  else if(x < 20) return(x * low.slope[k])
  else return(E[k,3] + x * high.slope[k])
}

### Read in sea level rise projections for Bergen. 
load("Simulation.Rdata")

### Discount rate based on Norwegian government recommendations. 
discount.rate <- c( rep(1.04, 40), rep(1.03, 35), rep(1.02, 10)) 
accum.discount.rate <- cumprod(discount.rate)

### Sample damage distributions for each year 2016-2100 (85 years)
### under various settings. 

### Sample 10 000 iid damage costs for each year 2016-2100
### This sample is used for all subsequent scenarios 
I <- 10000
orig.damage <- array(rburr(I*85, 
                           shape1=optim.out$par[1], 
                           shape2=optim.out$par[2], 
                           rate=optim.out$par[3]), 
                     dim=c(I, 85))

### Sample 10 000 damage change profiles
### This sample is used for all subsequent scenarios 
damage.scenario <- sample(1:15, I, replace=TRUE)

### Combine damage costs, damage change profiles and sea level
### projections 
yearly.damage <- array(NA, dim=c(I, 85))
for(i in 1:I)
{
  for(j in 1:85)
  {
    yearly.damage[i,j] <- orig.damage[i,j] * get.damage.mult(sim[i, (j+16)],
                                                             damage.scenario[i])
  }
  yearly.damage[i,] <- yearly.damage[i,]/accum.discount.rate
}

### Combine damage costs, damage change profiles and sea level
### projections assuming all subsequent years are like 2016. 
constant.damage <- array(NA, dim=c(I, 85))
for(i in 1:I)
{
  for(j in 1:85)
  {
    constant.damage[i,j] <- orig.damage[i,j] * get.damage.mult(sim[i, 16], 
                                                               damage.scenario[i])
  }
  constant.damage[i,] <- constant.damage[i,]/accum.discount.rate
}

### Combine damage costs, damage change profiles and sea level
### projections under adaptation. Adaptation measure is assumed
### to reduce 50% of the yearly damages to damages under 75 cm
### lower sea level.  
adapted.damage <- array(NA, dim=c(85, I, 85))
adaptation.cost <- 1000/CPI$CPI[8] ## Cost was 1000 MNOK in 2009 
for(k in 1:85)
{
    adapted.slr <- c( rep(0, k-1), rep(75, 85-k+1) )
    for(i in 1:I)
    {
        for(j in 1:85)
        {
            adapted.damage[k,i,j] <- orig.damage[i,j]/2 *
                get.damage.mult(sim[i, (j+16)], damage.scenario[i]) +
                orig.damage[i,j]/2 *
                get.damage.mult(sim[i, (j+16)]-adapted.slr[j], damage.scenario[i])
        }
        m <- which(adapted.slr==75)[1]
        adapted.damage[k,i,m] <- adapted.damage[k,i,m] + adaptation.cost
        adapted.damage[k,i,] <- adapted.damage[k,i,]/accum.discount.rate
    }    
}

save(yearly.damage, constant.damage, adapted.damage, file="calculatedDamage.RData")


### Cumulated additional damage due to sea level rise.
add.damage <- yearly.damage - constant.damage
cumsum.add.damage <- array(NA, dim=dim(add.damage))
for(i in 1:dim(add.damage)[1]) 
  cumsum.add.damage[i,] <- cumsum(add.damage[i,])
upper.95.add.cumsum <- apply(cumsum.add.damage,2,quantile,0.95)
lower.5.add.cumsum <- apply(cumsum.add.damage,2,quantile,0.05)
upper.90.add.cumsum <- apply(cumsum.add.damage,2,quantile,0.9)
lower.10.add.cumsum <- apply(cumsum.add.damage,2,quantile,0.1)
median.add.cumsum <- apply(cumsum.add.damage,2,quantile,0.5)


### Cumulated damage for various adaptation options
cumsum.adapt.damage <- array(NA, dim=dim(adapted.damage))
for(i in 1:85)
{
    for(j in 1:I)
    {
        cumsum.adapt.damage[i,j,] <- cumsum(adapted.damage[i,j,])
    }
}
cumsum.yearly.damage <- array(NA, dim=dim(yearly.damage))
for(i in 1:I)
    cumsum.yearly.damage[i,] <- cumsum(yearly.damage[i,])

### Calculate summary statistics for total damages for various
### adaptation options
adapt.median <- apply(cumsum.adapt.damage[,,85],1,median)
adapt.upper.95 <- apply(cumsum.adapt.damage[,,85],1,quantile,0.95)
adapt.upper.90 <- apply(cumsum.adapt.damage[,,85],1,quantile,0.9)
adapt.lower.05 <- apply(cumsum.adapt.damage[,,85],1,quantile,0.05)
adapt.lower.10 <- apply(cumsum.adapt.damage[,,85],1,quantile,0.1)

### Cumulated additional damage due to sea level rise with
### adaptation in 2044 (most cost effective adaptation). 
add.damage.2044 <- adapted.damage[29,,] - constant.damage
cumsum.add.damage.2044 <- array(NA, dim=dim(add.damage.2044))
for(i in 1:dim(add.damage.2044)[1]) 
  cumsum.add.damage.2044[i,] <- cumsum(add.damage.2044[i,])
upper.95.add.cumsum.2044 <- apply(cumsum.add.damage.2044,2,quantile,0.95)
lower.5.add.cumsum.2044 <- apply(cumsum.add.damage.2044,2,quantile,0.05)
upper.90.add.cumsum.2044 <- apply(cumsum.add.damage.2044,2,quantile,0.9)
lower.10.add.cumsum.2044 <- apply(cumsum.add.damage.2044,2,quantile,0.1)
median.add.cumsum.2044 <- apply(cumsum.add.damage.2044,2,quantile,0.5)

#######################################################################
### Plots for paper
#######################################################################

## Plots showing the underlying data used in the adaptation decision analysis
pdf(file="../../submission/DecisionAnalysisBergen.pdf", width=10, height=5, points=12)
### (a) Plot of damage data and fitted distribution
par(mex=0.75, mfrow=c(1,2))
hist(sf.damage, main="(a)", xlab="Amount (million NOK)", col="gray60", breaks=50, xlim=c(0,65),
     freq=FALSE)
x <- seq(0, 65, by=0.01)
y <- dburr(x, shape1=optim.out$par[1], shape2=optim.out$par[2], rate=optim.out$par[3])
lines(x,y,col="red",lwd=2)
### (b) Plot of extrapolated damage change profiles 
x <- c(0,20,40)
plot(x, E[1, 2:4], type="o", col="black", xlim=c(-100,130), ylim=c(-180,1000),
     xlab="Sea level anomaly (cm)", ylab="Relative mean annual damage", main="(b)", axes=FALSE)
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

pdf(file="../../submission/CumulativeDamageCostsBergen.pdf", width=5, height=5, points=12)
par(mex=0.75)
### Cumulative additional damage costs without adaptation compared to no sea level rise
plot(2016:2100, upper.90.add.cumsum/1e3,type="l", ylim=c(0,10), xlab="Year",
      ylab="Accumulated damage (billion NOK)", main="", lty=2)
lines(2016:2100, lower.10.add.cumsum/1e3, lty=2)
lines(2016:2100, median.add.cumsum/1e3, lwd=2)
### Cumulative additional damage costs for adaptation in 2044 compared to no sea level rise 
lines(2016:2100, upper.90.add.cumsum.2044/1e3, lty=2, col="red")
lines(2016:2100, lower.10.add.cumsum.2044/1e3, lty=2, col="red")
lines(2016:2100, median.add.cumsum.2044/1e3, lwd=2, col="red")
dev.off()

library(ggplot2)


png(file="TotalDamageCostsAdaptation.png", width=480, height=480, points=14)
par(mex=0.75)
plot(2016:2100, adapt.median/1e3, type="l", ylim=c(0,10), xlab="Year of adaptation measure",
      ylab="Total damage 2016-2100 (billion NOK)", main="")
lines(2016:2100, adapt.lower.10/1e3, type="l",lty=2)
lines(2016:2100, adapt.upper.90/1e3, type="l",lty=2)
a <- median(cumsum.yearly.damage[,85])/1e3
lines(c(2016,2100),c(a,a), col="red") 
dev.off()




#######################################################################
### Other plots 
#######################################################################
### Plot damage data for Hordaland and Rogaland.
png(file="DamagesAmounts.png", width=720, height=480, points=18)
par(mfrow=c(2,3),mex=0.75)
plot(A$Hordaland, V$Hordaland/CPI$CPI[1:36], main="Hordaland 1980-2015", xlab="Nr. of damages", ylab="Amount (1000 NOK)", ylim=c(0,65000), xlim=c(0,500))
hist(A$Hordaland, main="Hordaland 1980-2015", xlab="Nr. of damages", col="gray60", breaks=seq(0,500,by=10), xlim=c(0,500), ylim=c(0,18))
hist(V$Hordaland/CPI$CPI[1:36], main="Hordaland 1980-2015", xlab="Amount (1000 NOK)", col="gray60", breaks=50, xlim=c(0,65000), ylim=c(0,22)) 
plot(A$Rogaland, V$Rogaland/CPI$CPI[1:36], main="Rogaland 1980-2015", xlab="Nr. of damages", ylab="Amount (1000 NOK)", ylim=c(0,65000), xlim=c(0,500))
hist(A$Rogaland, main="Rogaland 1980-2015", xlab="Nr. of damages", col="gray60", breaks=seq(0,500,by=10), xlim=c(0,500), ylim=c(0,18))
hist(V$Rogaland/CPI$CPI[1:36], main="Rogaland 1980-2015", xlab="Amount (1000 NOK)", col="gray60", breaks=50, xlim=c(0,65000), ylim=c(0,22)) 
dev.off()

### Plot of data from Hallegatte et al. (2013) without extrapolation 
png(file="EuropeanIncreaseLoss.png", width=480, height=480, points=14)
par(mex=0.75)
x <- c(0,20,40)
plot(x, E[1, 2:4], type="o", col="gray60", ylim=c(1,max(E[,4])), xlim=c(0,40), xlab="Sea level rise (cm)", ylab="Relative mean annual damage", main="Hallegatte et al. (2013) for 15 European cities")
for(i in 2:15) lines(x, E[i, 2:4], type="o", col="gray60")
points(20, median(E[,3]), col="red", pch=15)
points(40, median(E[,4]), col="red", pch=15)
dev.off()




