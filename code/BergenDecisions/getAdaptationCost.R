### Adaptation cost estimate was 1000 MNOK in 2009.
### Normalized to 2015 values using CPI

getAdaptationCost <- function(years=1980:2015, normYear=2015)
{
    CPI <- read.csv("data/KPI.csv", header=TRUE)
    ind <- which(CPI[,1] %in% years) 
    CPI <- CPI[ind,]
    ind <- which(CPI[,1]==normYear)
    CPI$CPI <- CPI$CPI/CPI$CPI[ind]

    ind <- which(CPI[,1]==2009)

    return(1000/CPI$CPI[ind])
}
