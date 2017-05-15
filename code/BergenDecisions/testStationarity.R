### Testing an inclusion of observed sea level when calculating
### parameter estimates of Burr distribution

testStationarity <- function(sf.damage, theta.start=c(5, 0.5, 0.05, 0))
{
    ## Improve numerical stability of estimates by replacing zeros with a small value 
    ind <- which(sf.damage==0)
    sf.damage[ind] <- 1e-04

    load("data/ObsSeaLevel.RData")
    obs <- rep(BergenSeaLevel, 2)
    ind.obs.neg <- which(obs < 0)
     
    ## The negative log likelihood function to be optimized
    log.lik <- function(theta)
    {
        mult.factor <- 1 + abs(obs) * theta[4]
        mult.factor[ind.obs.neg] <- 1/mult.factor[ind.obs.neg]
        data <- sf.damage/mult.factor
        res <- sum(dburr(data, 
                         shape1=theta[1], 
                         shape2=theta[2], 
                         rate=theta[3], 
                         log=TRUE))
        return(-res)
    }
    
    ## Optimize the loss function
    optim.out <- optim(theta.start, 
                       fn=log.lik, 
                       method="BFGS", 
                       control=list(maxit=1000))

    ## Return parameter estimates
    return(optim.out$par)
}

### Running some tests 

quants <- c(0.02, seq(0.05,0.95, by=0.05), 0.98)
mult <- rep(NA, 36)
x11()
par(mfrow=c(3,5), mar=c(4,4,1,1))
for(t in 1:15)
{
    for(i in 1:36) mult[i] <- getDamageMult(BergenSeaLevel[i], t, E, slopes)
    par.est.t <- fitBurrDist(sf.damage/c(mult,mult))   
    q.mod <- qburr(quants, shape1=par.est[1],
                   shape2=par.est[2], rate=par.est[3])
    q.mod.t <- qburr(quants, shape1=par.est.t[1],
                   shape2=par.est.t[2], rate=par.est.t[3])
    q.max <- 75
    plot(q.mod, q.mod.t, type="b", xlim=c(0,q.max), ylim=c(0,q.max), main="",
     xlab="Original estimate", ylab="New estimate", axes=FALSE)
    axis(1)
    axis(2)
    lines(c(0,q.max), c(0,q.max), col="blue")
}
