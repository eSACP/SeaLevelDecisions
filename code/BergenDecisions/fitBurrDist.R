### Fit a Burr distribution to the damage data (in MNOK)
### Uses functions from the package actuar

fitBurrDist <- function(sf.damage, theta.start=c(5, 0.5, 0.05))
{
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
    return(optim.out$par)
}
