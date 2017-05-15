### Fit a Burr distribution to the damage data (in MNOK)
### Uses functions from the package actuar

fitBurrDist <- function(sf.damage, E, theta.start=c(5, 0.5, 0.05))
{
    
    load("data/ObsSeaLevel.RData")
    mult <- rep(NA, 72)
    level <- rep(BergenSeaLevel, 2)
    ## Median damage scenario 
    z <- c(0, 20, 40)
    y <- apply(E[,2:4], 2, median)
    res <- lm(y[1:2]~z[1:2])
    low.med.slope <- res$coef[2]
    res <- lm(y[2:3]~z[2:3])
    high.med.slope <- res$coef[2]
    get.median.damage.mult <- function(x)
    {
        if(x < 0) return(1/(1 + abs(x) * low.med.slope))
        else if(x < 20) return(1 + x * low.med.slope)
        else return(y[2] + x * high.med.slope)
    }
    for(i in 1:72) mult[i] <- get.median.damage.mult(level[i])

    sf.damage <- sf.damage/mult
    
    ## Improve numerical stability of estimates by replacing zeros with a small value 
    ind <- which(sf.damage==0)
    sf.damage[ind] <- 1e-04

    ## The negative log likelihood function to be optimized
    log.lik <- function(theta)
    {
        res <- sum(dburr(sf.damage, 
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
    return(list(par=optim.out$par, mult=mult))
}
