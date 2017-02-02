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
source("plotFigure5a.R")
source("plotFigure5b.R")

### Read in yearly damage data from Rogaland and Hordaland 1980 to 2015
sf.damage <- getDamageData()

### Fit Burr distribution to damage data (in MNOK)  
par.est <- fitBurrDist(sf.damage)

### Plot showing the damage data 
plotFigure1(sf.damage, par.est)

### Get data on relation between change in damage cost and sea level 
### Get data from Hallegatte et al. (2013)
E <- getHallegatteData()
### Extrapolate to any sea level rise
slopes <- getChangeSlopes(E)

### Plot showing the changes in damages with sea level rise
plotFigure2(E)

### Get adaptation cost 
adaptation.cost <- getAdaptationCost() 

### Damage distributions for 2016-2100 under various scenarios
## res <- getDamageTrajectories(E, slopes, adaptation.cost) 
## save(res, file="data/calculatedDamage.RData")
load("data/calculatedDamage.RData")

### Plot comparing accumulated damage costs under various scenarios
plotFigure3(res) 

### Plot comparing total damage cost under different adaptation timings
plotFigure4(res) 

### Investigation of the effects of uncertainty (no adaptation)
res.unc <- getUncertaintyTrajectories(E=E, slopes=slopes, orig.damage=res$orig, damage.scenario=res$scenario)
total.damage <- apply(res$yearly, 1, sum)

### Plot comparing distributions of total damage for various uncertainty settings
plotFigure5a(res.unc, total.damage)
### Same as above but on a log-log scale
plotFigure5b(res.unc, total.damage)

### Map of Bergen
MyMap = GetMap(center=c(60.3913, 5.3221), zoom=13, destfile="figures/BergenMap.png", size=c(500,500), maptype="terrain", GRAYSCALE=TRUE)
### Map of Esbjerg
MyMap2 = GetMap(center=c(55.4765, 8.4594), zoom=10, destfile="figures/EsbjergMap.png", size=c(500,500), maptype="terrain", GRAYSCALE=TRUE)







