### Plot comparing distributions of total damage for various uncertainty settings

plotFigure5a <- function(res.unc, total.damage, ifPdf=TRUE, fileName="figures/Uncertainty.pdf")
{
    total.mf.damage <- res.unc$mf
    total.slr.damage <- res.unc$slr
    total.dam.damage <- res.unc$dam

    if(ifPdf) pdf(file=fileName, width=10, height=5, points=12)
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
    abline(v=sum(res.unc$yearly.median/1e3), col="yellow", lwd=3)
    legend("topright", legend=c("Full uncertainty","SLR uncertainty","Effect uncertainty","Damage uncertainty", "No uncertainty"),
           col=c("black", "#7D26CD", "#4876FF", "#008B45", "yellow"), lty=1, lwd=2)
    box()
    if(ifPdf) dev.off()
}
