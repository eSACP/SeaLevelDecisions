### Decision analysis for Bergen
### Data files are located in the folder "data"
### R working directory should be set to current directory 

rm(list=ls())

library(actuar)
library(RgoogleMaps)
source("getDamageData.R")
source("fitBurrDist.R")
source("getDamageMult.R")
source("getHallegatteData.R")
source("getChangeSlopes.R")
source("getDamageTrajectories.R")
source("getDiscountRate.R")
source("getAdaptationCost.R")
source("getUncertaintyTrajectories.R")
source("plotFigure1.R")
source("plotFigure2.R")
source("plotFigure3.R")
source("plotFigure4.R")
source("plotFigure5.R")

### Read in yearly damage data from Rogaland and Hordaland 1980 to 2015
sf.damage <- getDamageData()

### Get data on relation between change in damage cost and sea level 
### Get data from Hallegatte et al. (2013)
E <- getHallegatteData()
### Extrapolate to any sea level rise
slopes <- getChangeSlopes(E)

### Get adaptation cost d
adaptation.cost <- getAdaptationCost() 

### Fit Burr distribution to damage data (in MNOK)  
par.est.res <- fitBurrDist(sf.damage, E)   
par.est <- par.est.res$par ## parameter estimates
mult <- par.est.res$mult ## multiplicative damage data corrections

### Plot showing the damage data 
plotFigure1(sf.damage/mult, par.est)

### Plot showing the changes in damages with sea level rise
plotFigure2(E)

### Damage distributions for 2016-2100 under various scenarios
## res26 <- getDamageTrajectories(E, par.est, slopes, adaptation.cost, inputfile="data/Simulation_2_6.Rdata") 
## save(res26, file="data/calculatedDamage_nonstat26.RData")

load("data/calculatedDamage_nonstat.RData")
load("data/calculatedDamage_nonstat26.RData")
load("data/calculatedDamage_nonstat45.RData")

### Plot comparing accumulated damage costs under various scenarios
plotFigure3(res) 

### Plot comparing total damage cost under different adaptation timings
plotFigure4(res) 

### Investigation of the effects of uncertainty (no adaptation)
res.unc <- getUncertaintyTrajectories(E=E,
                                      slopes=slopes,
                                      orig.damage=res$orig,
                                      damage.scenario=res$scenario,
                                      inputfile="data/Simulation_8_5.Rdata")
total.damage <- apply(res$yearly, 1, sum)
res.unc26 <- getUncertaintyTrajectories(E=E,
                                        slopes=slopes,
                                        orig.damage=res26$orig,
                                        damage.scenario=res26$scenario,
                                        inputfile="data/Simulation_2_6.Rdata")
total.damage26 <- apply(res26$yearly, 1, sum)
res.unc45 <- getUncertaintyTrajectories(E=E,
                                        slopes=slopes,
                                        orig.damage=res45$orig,
                                        damage.scenario=res45$scenario,
                                        inputfile="data/Simulation_4_5.Rdata")
total.damage45 <- apply(res45$yearly, 1, sum)

### Plot comparing distributions of total damage for various uncertainty settings
plotFigure5(res.unc, total.damage, res.unc45, total.damage45, res.unc26, total.damage26, ifPdf=TRUE, fileName="figures/UncertaintyLog.pdf")

### Map of Bergen
MyMap = GetMap(center=c(60.3913, 5.3221), zoom=13, destfile="figures/BergenMap.png", size=c(500,500), maptype="terrain", GRAYSCALE=TRUE)
### Map of Esbjerg
MyMap2 = GetMap(center=c(55.4765, 8.4594), zoom=10, destfile="figures/EsbjergMap.png", size=c(500,500), maptype="terrain", GRAYSCALE=TRUE)







