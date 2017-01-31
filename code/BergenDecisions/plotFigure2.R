## Plots showing the underlying data used in the adaptation decision analysis

plotFigure2 <- function(E, ifPdf=TRUE, fileName="figures/DecisionAnalysisBergenPartII.pdf")
{
    if(ifPdf) pdf(file="figures/DecisionAnalysisBergenPartII.pdf", width=5, height=5, points=12)
    par(mex=0.75, mar=c(5,4,2,2)+0.1)
    ## (b) Plot of extrapolated damage change profiles 
    x <- c(0,20,40)
    plot(x, E[1, 2:4], type="o", col="black", xlim=c(-100,130), ylim=c(-180,1000),
         xlab="Sea level anomaly (cm)", ylab="Relative mean annual damage", main="", axes=FALSE)
    box()
    axis(1)
    axis(2, at=c(-200, 1, 200, 400, 600, 800, 1000), labels=c(0.005, 1, 200, 400, 600, 800, 1000))
    z <- c(40,130)
    w <- c(-100,0)
    for(i in 1:15)
    {
        lines(x, E[i, 2:4], type="o", col="black")
        res <- lm(as.double(E[i,3:4])~x[2:3])
        lines(z, res$coef[1] + z * res$coef[2], col="gray70")
        res <- lm(as.double(E[i,2:3])~x[1:2])
        lines(w, res$coef[1] + w * res$coef[2], col="gray70")
    }
    points(20, median(E[,3]), col="red", pch=15)
    points(40, median(E[,4]), col="red", pch=15)
    res <- lm(apply(E[,2:3],2,median)~x[1:2])
    w <- c(-100, 20)
    lines(w, res$coef[1] + w * res$coef[2], col="red", lwd=2)
    res <- lm(apply(E[,3:4],2,median)~x[2:3])
    z <- c(20,130)
    lines(z, res$coef[1] + z * res$coef[2], col="red", lwd=2)
    if(ifPdf) dev.off()
}
