### Read in damage data obtained from Finance Norway's website:
### www.finansnorge.no/statistikk/skadeforsikring/Naturskadestatistikk-NASK/
### Standardize costs using CPI
### Return standardized costs in MNOK

getDamageData <- function(years=1980:2015, normYear=2015)
{
    ## Read in damage data 
    V <- read.csv("data/StormfloYearFylkeVerdi.csv", header=TRUE) 
    ## Select appropriate years
    ind <- which(V[,1] %in% years) 
    V <- V[ind,] 
    
    ## Read in consumer price index (CPI).
    CPI <- read.csv("data/KPI.csv", header=TRUE)
    ## Select same years are above
    ind <- which(CPI[,1] %in% years) 
    CPI <- CPI[ind,]
    ind <- which(CPI[,1]==normYear)
    CPI$CPI <- CPI$CPI/CPI$CPI[ind] ## Normalize to normYear 

    ## Combine Hordaland and Rogaland data.
    ## Standardize costs to normYear values using CPI. 
    sf.damage <- c(V$Hordaland/CPI$CPI, V$Rogaland/CPI$CPI)
    ## Normalize to MNOK 
    sf.damage <- sf.damage/1e3 

    return(sf.damage)
}
