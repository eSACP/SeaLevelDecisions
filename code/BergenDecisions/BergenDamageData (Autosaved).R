### Decision analysis for Bergen. All the data files are located in the same
### directory as this script. The R working directory should thus be set
### to the current directory. 

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
    y <- as.double(E[i,2:4])
    res <- lm(y[1:2]~x[1:2])
    low.slope[i] <- res$coef[2]
    res <- lm(y[2:3]~x[2:3])
    high.slope[i] <- res$coef[2]
}

### Function that calculates the multiplicative change in damage for sea level x 
### and Hallegatte et al. (2013) estimates for city k.  
get.damage.mult <- function(x, k)
{
  if(x < 0) return(1/(1 + abs(x) * low.slope[k]))
  else if(x < 20) return(1 + x * low.slope[k])
  else return(as.double(E[k,3]) + x * high.slope[k])
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
    print(k)
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

# save(orig.damage, damage.scenario, yearly.damage, constant.damage, adapted.damage, file="calculatedDamage.RData")
#  load("calculatedDamage.RData")

### Cumulated additional damage due to sea level rise.
add.damage <- yearly.damage - constant.damage
cumsum.add.damage <- array(NA, dim=dim(add.damage))
cumsum.yearly.damage <- array(NA, dim=dim(add.damage))
cumsum.constant.damage <- array(NA, dim=dim(add.damage))
for(i in 1:dim(add.damage)[1]) 
{
    cumsum.add.damage[i,] <- cumsum(add.damage[i,])
    cumsum.yearly.damage[i,] <- cumsum(yearly.damage[i,])
    cumsum.constant.damage[i,] <- cumsum(constant.damage[i,])
}
upper.95.add.cumsum <- apply(cumsum.add.damage,2,quantile,0.95)
lower.5.add.cumsum <- apply(cumsum.add.damage,2,quantile,0.05)
median.add.cumsum <- apply(cumsum.add.damage,2,quantile,0.5)
upper.95.yearly.cumsum <- apply(cumsum.yearly.damage,2,quantile,0.95)
lower.5.yearly.cumsum <- apply(cumsum.yearly.damage,2,quantile,0.05)
median.yearly.cumsum <- apply(cumsum.yearly.damage,2,quantile,0.5)
upper.95.constant.cumsum <- apply(cumsum.constant.damage,2,quantile,0.95)
lower.5.constant.cumsum <- apply(cumsum.constant.damage,2,quantile,0.05)
median.constant.cumsum <- apply(cumsum.constant.damage,2,quantile,0.5)

 
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
adapt.lower.05 <- apply(cumsum.adapt.damage[,,85],1,quantile,0.05)

### Cumulated additional damage due to sea level rise with
### adaptation in 2047 (most cost effective adaptation). 
add.damage.2047 <- adapted.damage[32,,] - constant.damage
cumsum.add.damage.2047 <- array(NA, dim=dim(add.damage.2047))
cumsum.adapt.damage.2047 <- array(NA, dim=dim(add.damage.2047))
for(i in 1:dim(add.damage.2047)[1])
{
    cumsum.add.damage.2047[i,] <- cumsum(add.damage.2047[i,])
    cumsum.adapt.damage.2047[i,] <- cumsum(adapted.damage[32,i,])
}
upper.95.add.cumsum.2047 <- apply(cumsum.add.damage.2047,2,quantile,0.95)
lower.5.add.cumsum.2047 <- apply(cumsum.add.damage.2047,2,quantile,0.05)
median.add.cumsum.2047 <- apply(cumsum.add.damage.2047,2,quantile,0.5)
upper.95.adapt.cumsum.2047 <- apply(cumsum.adapt.damage.2047,
                                    2,quantile,0.95)
lower.5.adapt.cumsum.2047 <- apply(cumsum.adapt.damage.2047,
                                   2,quantile,0.05)
median.adapt.cumsum.2047 <- apply(cumsum.adapt.damage.2047,
                                  2,quantile,0.5)

######################################################################
### Investigation of the effects of uncertainty (no adaptation)

### Cumulative damage with medians

## Median damage scenario 
z <- c(0, 20, 40)
y <- apply(E[,2:4], 2, median)
res <- lm(y[1:2]~z[1:2])
low.med.slope <- res$coef[2]
res <- lm(y[2:3]~z[2:3])
high.med.slope <- res$coef[2]


get.median.damage.mult <- function(x)
{
    if(x < 0) return(1/(1 + abs(x) * low.med.slope))
    else if(x < 20) return(1 + x * low.med.slope)
    else return(y[2] + x * high.med.slope)
}

## Median sea level rise
median.sim <- apply(sim, 2, median)[17:101]

## Median damage cost
median.damage <- apply(orig.damage, 2, median)

## Putting the three together
yearly.median.damage <- rep(NA, 85)
for(j in 1:85)
  {
    yearly.median.damage[j] <- median.damage[j] * get.median.damage.mult(median.sim[j]) 
  }
yearly.median.damage <- yearly.median.damage/accum.discount.rate

## Uncertainty in damage mult. fct. only 
yearly.mf.damage <- array(NA, dim=c(I, 85))
for(i in 1:I)
{
  for(j in 1:85)
  {
    yearly.mf.damage[i,j] <- median.damage[j] * get.damage.mult(median.sim[j],
                                                             damage.scenario[i])
  }
  yearly.mf.damage[i,] <- yearly.mf.damage[i,]/accum.discount.rate
}
total.mf.damage <- apply(yearly.mf.damage,1,sum)

## Uncertainty in slr only 
yearly.slr.damage <- array(NA, dim=c(I, 85))
for(i in 1:I)
{
  for(j in 1:85)
  {
    yearly.slr.damage[i,j] <- median.damage[j] * get.median.damage.mult(sim[i, (j+16)])
  }
  yearly.slr.damage[i,] <- yearly.slr.damage[i,]/accum.discount.rate
}
total.slr.damage <- apply(yearly.slr.damage,1,sum)

## Uncertainty in damages only 
yearly.dam.damage <- array(NA, dim=c(I, 85))
for(i in 1:I)
{
  for(j in 1:85)
  {
    yearly.dam.damage[i,j] <- orig.damage[i,j] * get.median.damage.mult(median.sim[j])
  }
  yearly.dam.damage[i,] <- yearly.dam.damage[i,]/accum.discount.rate
}
total.dam.damage <- apply(yearly.dam.damage,1,sum)

total.damage <- apply(yearly.damage, 1, sum)

#######################################################################
### Plots for paper
#######################################################################

## Plots showing the underlying data used in the adaptation decision analysis
pdf(file="../../submission/DecisionAnalysisBergenPartI.pdf", width=5, height=5, points=12)
### (a) Plot of damage data and fitted distribution
## par(mex=0.75, mfrow=c(1,2))
par(mex=0.75, mar=c(5,4,2,2)+0.1)
hist(sf.damage, main="", xlab="Amount (million NOK)", col="gray60", breaks=50,  xlim=c(0,65),
     freq=FALSE)
x <- seq(0, 65, by=0.01)
y <- dburr(x, shape1=optim.out$par[1], shape2=optim.out$par[2], rate=optim.out$par[3])
lines(x,y,col="red",lwd=2)
dev.off()
pdf(file="../../submission/DecisionAnalysisBergenPartII.pdf", width=5, height=5, points=12)
par(mex=0.75, mar=c(5,4,2,2)+0.1)
### (b) Plot of extrapolated damage change profiles 
x <- c(0,20,40)
plot(x, E[1, 2:4], type="o", col="black", xlim=c(-100,130), ylim=c(-180,1000),
     xlab="Sea level anomaly (cm)", ylab="Relative mean annual damage", main="", axes=FALSE)
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

## Plot comparing accumulated damage costs under various scenarios
pdf(file="../../submission/CumDamageCostsBergen.pdf", width=5, height=5, points=12)
## png(file="CumDamageCostsBergen.png", width=480, height=480, points=12)
par(mex=0.75)
### Cumulative damage costs without adaptation 
plot(2016:2100, upper.95.yearly.cumsum/1e3,type="l",
     ylim=c(0,14), xlab="Year",
     ylab="Accumulated damage (billion NOK)", main="", lty=3)
### Cumulative damage costs for no sea level rise
polygon(c(2016:2100, rev(2016:2100)),
        c(upper.95.constant.cumsum/1e3, rev(lower.5.constant.cumsum/1e3)),
        col="gray80", border="gray80")
lines(2016:2100, median.constant.cumsum/1e3, lwd=2, col="gray40")
### Cumulative damage costs without adaptation 
lines(2016:2100, lower.5.yearly.cumsum/1e3, lty=3)
lines(2016:2100, median.yearly.cumsum/1e3, lwd=2)
### Cumulative damage costs for adaptation in 2044
lines(2016:2100, upper.95.adapt.cumsum.2047/1e3, lty=3, col="red")
lines(2016:2100, lower.5.adapt.cumsum.2047/1e3, lty=3, col="red")
lines(2016:2100, median.adapt.cumsum.2047/1e3, lwd=2, col="red")
dev.off()

## Plot comparing total damage cost under different adaptation timings
pdf(file="../../submission/TotalDamageCostsAdaptation.pdf", width=10, height=5, points=12)
par(mex=0.75)
a <- median(cumsum.yearly.damage[,85])/1e3
plot(c(2016,2100), c(a,a), type="l", ylim=c(0,14), xlab="Year of adaptation measure",
      ylab="Total damage 2016-2100 (billion NOK)", main="")
year <- (2016:2100)
k <- 0.35
for(i in 1:85)
{
    a <- year[i]
    b <- adapt.upper.95[i]/1e3
    c <- adapt.lower.05[i]/1e3
    polygon(c(a-k, a+k, a+k, a-k), c(c, c, b, b), col="gray80", border="gray80")
    d <- adapt.median[i]/1e3
    lines(c(a-k, a+k), c(d,d), lwd=2, col="red")
}
a <- median(cumsum.yearly.damage[,85])/1e3
lines(c(2016-k,2100+k),c(a,a), col="black", lwd=2)
a <- quantile(cumsum.yearly.damage[,85], 0.95)/1e3
lines(c(2016-k,2100+k),c(a,a), col="black", lty=3)
a <- quantile(cumsum.yearly.damage[,85], 0.05)/1e3
lines(c(2016-k,2100+k),c(a,a), col="black", lty=3)
dev.off()

## Plot comparing distributions of total damage for various uncertainty settings
pdf(file="../../submission/Uncertainty.pdf", width=10, height=5, points=12)
par(mex=0.75, mar=c(5,4,2,2)+0.1)
hist(total.damage/1e3, freq=FALSE, ylim=c(0,5), xlim=c(0,10), breaks=seq(0,170,by=0.05),
     col="black", border="black", xlab="Total damage 2016-2100 (billion NOK)",
     ylab="Density", xaxs="i", yaxs="i", main="") # black
hist(total.slr.damage/1e3, freq=FALSE, ylim=c(0,5), xlim=c(0,10), breaks=seq(0,170,by=0.05),
     col="#7D26CD50", border="#7D26CD", add=TRUE) # purple
hist(total.mf.damage/1e3, freq=FALSE, ylim=c(0,5), xlim=c(0,10), breaks=seq(0,170,by=0.05),
     col="#4876FF50", border="#4876FF", add=TRUE) # blue
hist(total.dam.damage/1e3, freq=FALSE, ylim=c(0,5), xlim=c(0,10), breaks=seq(0,170,by=0.05),
     col="#008B4550", border="#008B45", add=TRUE) # green
abline(v=median(total.damage/1e3), col="black", lwd=3)
abline(v=median(total.slr.damage/1e3), col="#7D26CD", lwd=3)
abline(v=median(total.mf.damage/1e3), col="#4876FF", lwd=3)
abline(v=median(total.dam.damage/1e3), col="#008B45", lwd=3)
abline(v=sum(yearly.median.damage/1e3), col="yellow", lwd=3)
legend("topright", legend=c("Full uncertainty","SLR uncertainty","Effect uncertainty","Damage uncertainty", "No uncertainty"),
       col=c("black", "#7D26CD", "#4876FF", "#008B45", "yellow"), lty=1, lwd=2)
box()
dev.off()

### Same as above but on a log-log scale
pdf(file="../../submission/UncertaintyLog.pdf", width=10, height=5, points=12)
par(mex=0.75, mar=c(5,4,2,2)+0.1)
buckets <- seq(log(50),log(165000), by=0.1)
buckets <- exp(buckets)
my.hist <- hist(total.damage, breaks=buckets, plot=FALSE)
my.slr.hist <- hist(total.slr.damage, breaks=buckets, plot=FALSE)
my.mf.hist <- hist(total.mf.damage, breaks=buckets, plot=FALSE)
my.dam.hist <- hist(total.dam.damage, breaks=buckets, plot=FALSE)
plot(log(my.slr.hist$breaks[-1]), log(my.slr.hist$counts), type="h", col="#7D26CD",
     main="", ylab="Log frequency", xlab="Total damage 2016-2100 (million NOK)",
     lwd=2, axes=FALSE)
ticks <- c(50, 150, 500, 1500, 5000, 15000, 50000, 150000)
axis(1, at = log(ticks), labels=ticks)
axis(2)
box()
lines(log(my.hist$breaks[-1])+0.02, log(my.hist$counts), col="black", type="h", lwd=2)
lines(log(my.mf.hist$breaks[-1])+0.04, log(my.mf.hist$counts), col="orange", type="h", lwd=2)
lines(log(my.dam.hist$breaks[-1])+0.06, log(my.dam.hist$counts), col="#008B45", type="h", lwd=2)
abline(v=log(sum(yearly.median.damage)),  col="gray50", lwd=2)
points(log(median(total.damage)), 7.8, col="black", pch=16)
points(log(median(total.slr.damage)),7.8,  col="#7D26CD", pch=16)
points(log(median(total.dam.damage)),7.8,  col="#008B45", pch=16)
points(log(sum(yearly.median.damage)),7.8,  col="gray50", pch=16)
points(log(median(total.mf.damage)),7.8,  col="orange", pch=16)
legend("topright",
       legend=c("Full uncertainty","SLR uncertainty","Effect uncertainty",
                "Damage uncertainty", "No uncertainty"),
       col=c("black", "#7D26CD", "orange", "#008B45", "gray50"), lty=1, lwd=2)
dev.off()

### Map of Bergen
library(RgoogleMaps)
MyMap = GetMap(center=c(60.3913, 5.3221), zoom=13, destfile="../../submission/BergenMap.png", size=c(500,500), maptype="terrain", GRAYSCALE=TRUE)
MyMap2 = GetMap(center=c(55.4765, 8.4594), zoom=10, destfile="../../submission/EsbjergMap.png", size=c(500,500), maptype="terrain", GRAYSCALE=TRUE)


## mymarkers <- cbind.data.frame(lat=c(60.383723, 60.400241), lon=c(5.316847, 5.312512))
## pdf(file="../../submission/BergenMap.pdf", width=5, height=5, points=12)
## tmp = PlotOnStaticMap(MyMap, lat=mymarkers[,"lat"], lon=mymarkers[,"lon"], cex=2.5, pch=20, col="red", add=T) 
## dev.off()

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




