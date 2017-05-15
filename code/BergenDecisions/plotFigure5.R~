### Plot comparing 90% intervals of total damage for various uncertainty settings on a log scale

plotFigure5c <- function(res.unc, total.damage, res.unc45, total.damage45, res.unc26, total.damage26, ifPdf=TRUE, fileName="figures/UncertaintyLog.pdf")
{
    quant.mf.damage <- as.numeric(quantile(res.unc$mf, c(0.05, 0.95)))
    quant.slr.damage <- as.numeric(quantile(res.unc$slr, c(0.05, 0.95)))
    quant.dam.damage <- as.numeric(quantile(res.unc$dam, c(0.05, 0.95)))
    quant.damage <- as.numeric(quantile(total.damage, c(0.05, 0.95)))

    quant.mf.damage26 <- as.numeric(quantile(res.unc26$mf, c(0.05, 0.95)))
    quant.slr.damage26 <- as.numeric(quantile(res.unc26$slr, c(0.05, 0.95)))
    quant.dam.damage26 <- as.numeric(quantile(res.unc26$dam, c(0.05, 0.95)))
    quant.damage26 <- as.numeric(quantile(total.damage26, c(0.05, 0.95)))

    quant.mf.damage45 <- as.numeric(quantile(res.unc45$mf, c(0.05, 0.95)))
    quant.slr.damage45 <- as.numeric(quantile(res.unc45$slr, c(0.05, 0.95)))
    quant.dam.damage45 <- as.numeric(quantile(res.unc45$dam, c(0.05, 0.95)))
    quant.damage45 <- as.numeric(quantile(total.damage45, c(0.05, 0.95)))

    
    if(ifPdf) pdf(file="figures/UncertaintyLog.pdf", width=10, height=5, points=12)
    par(mex=0.75, mar=c(5,2,2,2)+0.1)
    n <- 8
    plot(1, type="n", main="", ylab="", xlab="Total cost 2016-2100 (million NOK)",
         xlim=c(log(n),log(5000)), ylim=c(0,18), axes=FALSE)
    ticks <- c(10, 20, 50, 150, 500, 1500, 5000)
    axis(1, at = log(ticks), labels=ticks)
    k <- 0.4
    polygon(rep(c(log(n), log(5000)), each=2), c(0,6,6,0), col="gray75", border=FALSE)
    polygon(rep(log(quant.damage), each=2), c(1-k, 1+k, 1+k, 1-k), col="#6594ED60", border=FALSE)
    lines(log(rep(median(total.damage),2)), c(1-k, 1+k), col="#6594ED", lwd=5)
    polygon(rep(log(quant.slr.damage), each=2), c(2-k, 2+k, 2+k, 2-k), col="#7D26CD60", border=FALSE)
    lines(log(rep(median(res.unc$slr),2)), c(2-k, 2+k), col="#7D26CD", lwd=5)
    polygon(rep(log(quant.mf.damage), each=2), c(3-k, 3+k, 3+k, 3-k), col="#FFA50060", border=FALSE)
    lines(log(rep(median(res.unc$mf),2)), c(3-k, 3+k), col="#FFA500", lwd=5)
    polygon(rep(log(quant.dam.damage), each=2), c(4-k, 4+k, 4+k, 4-k), col="#008B4560", border=FALSE)
    lines(log(rep(median(res.unc$dam),2)), c(4-k, 4+k), col="#008B45", lwd=5)
    lines(rep(log(sum(res.unc$yearly.median)),2), c(5-k, 5+k), col="black", lwd=5)
    text(log(12), 5.2, "RCP 8.5", cex=1.5)
    polygon(rep(c(log(n), log(5000)), each=2), c(6,12,12,6), col="gray85", border=FALSE)
    polygon(rep(log(quant.damage45), each=2), c(7-k, 7+k, 7+k, 7-k), col="#6594ED60", border=FALSE)
    lines(log(rep(median(total.damage45),2)), c(7-k, 7+k), col="#6594ED", lwd=5)
    polygon(rep(log(quant.slr.damage45), each=2), c(8-k, 8+k, 8+k, 8-k), col="#7D26CD60", border=FALSE)
    lines(log(rep(median(res.unc45$slr),2)), c(8-k, 8+k), col="#7D26CD", lwd=5)
    polygon(rep(log(quant.mf.damage45), each=2), c(9-k, 9+k, 9+k, 9-k), col="#FFA50060", border=FALSE)
    lines(log(rep(median(res.unc45$mf),2)), c(9-k, 9+k), col="#FFA500", lwd=5)
    polygon(rep(log(quant.dam.damage45), each=2), c(10-k, 10+k, 10+k, 10-k), col="#008B4560", border=FALSE)
    lines(log(rep(median(res.unc45$dam),2)), c(10-k, 10+k), col="#008B45", lwd=5)
    lines(rep(log(sum(res.unc45$yearly.median)),2), c(11-k, 11+k), col="black", lwd=5)
    text(log(12), 11.2, "RCP 4.5", cex=1.5)

    polygon(rep(c(log(n), log(5000)), each=2), c(12,18,18,12), col="gray95", border=FALSE)
    polygon(rep(log(quant.damage26), each=2), c(13-k, 13+k, 13+k, 13-k), col="#6594ED60", border=FALSE)
    lines(log(rep(median(total.damage26),2)), c(13-k, 13+k), col="#6594ED", lwd=5)
    polygon(rep(log(quant.slr.damage26), each=2), c(14-k, 14+k, 14+k, 14-k), col="#7D26CD60", border=FALSE)
    lines(log(rep(median(res.unc26$slr),2)), c(14-k, 14+k), col="#7D26CD", lwd=5)
    polygon(rep(log(quant.mf.damage26), each=2), c(15-k, 15+k, 15+k, 15-k), col="#FFA50060", border=FALSE)
    lines(log(rep(median(res.unc26$mf),2)), c(15-k, 15+k), col="#FFA500", lwd=5)
    polygon(rep(log(quant.dam.damage26), each=2), c(16-k, 16+k, 16+k, 16-k), col="#008B4560", border=FALSE)
    lines(log(rep(median(res.unc26$dam),2)), c(16-k, 16+k), col="#008B45", lwd=5)
    lines(rep(log(sum(res.unc26$yearly.median)),2), c(17-k, 17+k), col="black", lwd=5)
    text(log(12), 17.2, "RCP 2.6", cex=1.5)

    box()
    legend("topright",
           legend=rev(c("Full uncertainty","SLR uncertainty","Effect uncertainty",
                    "Damage uncertainty", "No uncertainty")),
           col=rev(c("#6594ED", "#7D26CD", "orange", "#008B45", "black")), lty=1, lwd=5)
    if(ifPdf) dev.off()
    
}
