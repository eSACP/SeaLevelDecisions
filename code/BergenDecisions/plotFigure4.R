## Plot comparing total damage cost under different adaptation timings

plotFigure4 <- function(res, ifPdf=TRUE, fileName="figures/TotalDamageCostsAdaptation.pdf")
{
    I <- 10000

    ## Calculate cumulative damages over 2016-2100
    ## We present the distribution through median and 90% interval

    ## Cumulated additional damage due to sea level rise.
    cumsum.yearly.damage <- array(NA, dim=dim(res$yearly))
    for(i in 1:dim(res$yearly)[1]) 
    {
        cumsum.yearly.damage[i,] <- cumsum(res$yearly[i,])
    }
    
    ## Cumulated damage for various adaptation options
    cumsum.adapt.damage <- array(NA, dim=dim(res$adapted))
    for(i in 1:85)
    {
        for(j in 1:I)
        {
            cumsum.adapt.damage[i,j,] <- cumsum(res$adapted[i,j,])
        }
    }
    cumsum.yearly.damage <- array(NA, dim=dim(res$yearly))
    for(i in 1:I)
        cumsum.yearly.damage[i,] <- cumsum(res$yearly[i,])
    
    ## Calculate summary statistics for total damages for various
    ## adaptation options
    adapt.median <- apply(cumsum.adapt.damage[,,85],1,median)
    adapt.upper.95 <- apply(cumsum.adapt.damage[,,85],1,quantile,0.95)
    adapt.lower.05 <- apply(cumsum.adapt.damage[,,85],1,quantile,0.05)

    ## Make the plot
    
    if(ifPdf) pdf(file=fileName, width=10, height=5, points=12)
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
    if(ifPdf) dev.off()

}
