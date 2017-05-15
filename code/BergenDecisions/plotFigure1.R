## Plots showing the underlying data used in the adaptation decision analysis

plotFigure1 <- function(sf.damage, par.est, ifPdf=TRUE, fileName="figures/DecisionAnalysisBergenPartI.pdf")
{
    if(ifPdf) pdf(file=fileName, width=10, height=5, points=12)
    ## (a) Plot of damage data and fitted distribution
    par(mex=0.75, mar=c(5,4,2,2)+0.1, mfrow=c(1,2))
    hist(sf.damage, main="", xlab="Cost (million NOK)", col="gray60", breaks=100,  xlim=c(0,25),
         freq=FALSE)
    x <- seq(0, 25, by=0.01)
    y <- dburr(x, shape1=par.est[1], shape2=par.est[2], rate=par.est[3])
    lines(x,y,col="red",lwd=2)

    ## (b) QQ-plot of obs vs. estimated quantiles
    quants <- seq(0.02,0.99, by=0.01)
    q.obs <- as.numeric(quantile(sf.damage, quants))
    q.mod <- qburr(quants, shape1=par.est[1], shape2=par.est[2], rate=par.est[3])
    q.max <- 25
    plot(q.mod, q.obs, type="b", xlim=c(0,q.max), ylim=c(0,q.max), main="",
         xlab="Estimated quantiles", ylab="Observed quantiles", axes=FALSE)
    axis(1)
    axis(2)
    lines(c(0,q.max), c(0,q.max), col="blue")
    if(ifPdf) dev.off()
}
