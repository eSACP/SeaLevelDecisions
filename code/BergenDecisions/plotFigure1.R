## Plots showing the underlying data used in the adaptation decision analysis

plotFigure1 <- function(sf.damage, par.est, ifPdf=TRUE, fileName="figures/DecisionAnalysisBergenPartI.pdf")
{
    if(ifPdf) pdf(file=fileName, width=5, height=5, points=12)
    ## (a) Plot of damage data and fitted distribution
    par(mex=0.75, mar=c(5,4,2,2)+0.1)
    hist(sf.damage, main="", xlab="Amount (million NOK)", col="gray60", breaks=50,  xlim=c(0,65),
         freq=FALSE)
    x <- seq(0, 65, by=0.01)
    y <- dburr(x, shape1=par.est[1], shape2=par.est[2], rate=par.est[3])
    lines(x,y,col="red",lwd=2)
    if(ifPdf) dev.off()
}
