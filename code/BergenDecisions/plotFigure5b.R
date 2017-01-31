### Plot comparing distributions of total damage for various uncertainty settings on a log-log scale

plotFigure5b <- function(res.unc, total.damage, ifPdf=TRUE, fileName="figures/UncertaintyLog.pdf")
{
    total.mf.damage <- res.unc$mf
    total.slr.damage <- res.unc$slr
    total.dam.damage <- res.unc$dam

    if(ifPdf) pdf(file="figures/UncertaintyLog.pdf", width=10, height=5, points=12)
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
    abline(v=log(sum(res.unc$yearly.median)),  col="gray50", lwd=2)
    points(log(median(total.damage)), 7.8, col="black", pch=16)
    points(log(median(total.slr.damage)),7.8,  col="#7D26CD", pch=16)
    points(log(median(total.dam.damage)),7.8,  col="#008B45", pch=16)
    points(log(sum(res.unc$yearly.median)),7.8,  col="gray50", pch=16)
    points(log(median(total.mf.damage)),7.8,  col="orange", pch=16)
    legend("topright",
           legend=c("Full uncertainty","SLR uncertainty","Effect uncertainty",
                    "Damage uncertainty", "No uncertainty"),
           col=c("black", "#7D26CD", "orange", "#008B45", "gray50"), lty=1, lwd=2)
    if(ifPdf) dev.off()
    
}
