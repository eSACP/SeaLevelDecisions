## Plot comparing accumulated damage costs under various scenarios

plotFigure3 <- function(res, ifPdf=TRUE, fileName="figures/CumDamageCostsBergen.pdf")
{
    I <- 10000

    ## Calculate cumulative damages over 2016-2100
    ## We present the distribution through median and 90% interval

    ## Cumulated additional damage due to sea level rise.
    cumsum.yearly.damage <- array(NA, dim=dim(res$yearly))
    cumsum.constant.damage <- array(NA, dim=dim(res$yearly))
    for(i in 1:dim(res$yearly)[1]) 
    {
        cumsum.yearly.damage[i,] <- cumsum(res$yearly[i,])
        cumsum.constant.damage[i,] <- cumsum(res$constant[i,])
    }

    upper.95.yearly.cumsum <- apply(cumsum.yearly.damage,2,quantile,0.95)
    lower.5.yearly.cumsum <- apply(cumsum.yearly.damage,2,quantile,0.05)
    median.yearly.cumsum <- apply(cumsum.yearly.damage,2,quantile,0.5)
    
    upper.95.constant.cumsum <- apply(cumsum.constant.damage,2,quantile,0.95)
    lower.5.constant.cumsum <- apply(cumsum.constant.damage,2,quantile,0.05)
    median.constant.cumsum <- apply(cumsum.constant.damage,2,quantile,0.5)

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

    ## Cumulated damage due to sea level rise with
    ## adaptation in 2047 (most cost effective adaptation). 
    cumsum.adapt.damage.2047 <- array(NA, dim=dim(res$adapted[32,,]))
    
    for(i in 1:dim(res$adapted[32,,])[1])
    {
        cumsum.adapt.damage.2047[i,] <- cumsum(res$adapted[32,i,])
    }
    upper.95.adapt.cumsum.2047 <- apply(cumsum.adapt.damage.2047,
                                        2,quantile,0.95)
    lower.5.adapt.cumsum.2047 <- apply(cumsum.adapt.damage.2047,
                                       2,quantile,0.05)
    median.adapt.cumsum.2047 <- apply(cumsum.adapt.damage.2047,
                                      2,quantile,0.5)

    ## Making the plot 
    if(ifPdf) pdf(file=fileName, width=5, height=5, points=12)
    par(mex=0.75)
    ## Cumulative damage costs without adaptation 
    plot(2016:2100, upper.95.yearly.cumsum/1e3,type="l",
         ylim=c(0,14), xlab="Year",
         ylab="Accumulated damage (billion NOK)", main="", lty=3)
    ## Cumulative damage costs for no sea level rise
    polygon(c(2016:2100, rev(2016:2100)),
            c(upper.95.constant.cumsum/1e3, rev(lower.5.constant.cumsum/1e3)),
            col="gray80", border="gray80")
    lines(2016:2100, median.constant.cumsum/1e3, lwd=2, col="gray40")
    ## Cumulative damage costs without adaptation 
    lines(2016:2100, lower.5.yearly.cumsum/1e3, lty=3)
    lines(2016:2100, median.yearly.cumsum/1e3, lwd=2)
    ## Cumulative damage costs for adaptation in 2047
    lines(2016:2100, upper.95.adapt.cumsum.2047/1e3, lty=3, col="red")
    lines(2016:2100, lower.5.adapt.cumsum.2047/1e3, lty=3, col="red")
    lines(2016:2100, median.adapt.cumsum.2047/1e3, lwd=2, col="red")
    if(ifPdf) dev.off()

}
