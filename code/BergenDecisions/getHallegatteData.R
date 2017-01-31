### Get Hallegatte et al. (2013) data on relation between change in damage 
### cost and change in sea level
### 
### Data in csv file is copied manually from online supplementary information  

getHallegatteData <- function()
{
    E <- read.csv("data/EuropeanLossData.csv", header=TRUE)
    E[,2:4] <- E[,2:4]/E[,2]
    return(E)
}
