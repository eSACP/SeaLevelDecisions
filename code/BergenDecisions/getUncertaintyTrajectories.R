### Investigation of the effects of uncertainty (no adaptation)

getUncertaintyTrajectories <- function(E, slopes, orig.damage, damage.scenario)
{
    ## Cumulative damage with medians

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

    ## Read in sea level rise projections for Bergen. 
    load("data/Simulation.Rdata")
    I <- dim(sim)[1]
    
    ## Get discount rates 
    discount.rate <- getDiscountRate()
    accum.discount.rate <- cumprod(discount.rate)

    ## Median sea level rise
    median.sim <- apply(sim, 2, median)[17:101]

    ## Median damage cost
    median.damage <- apply(orig.damage, 2, median)

    ## Putting the three together
    yearly.median.damage <- rep(NA, 85)
    for(j in 1:85)
    {
        yearly.median.damage[j] <- median.damage[j] * get.median.damage.mult(median.sim[j]) 
    }
    yearly.median.damage <- yearly.median.damage/accum.discount.rate
    
    ## Uncertainty in damage mult. fct. only 
    yearly.mf.damage <- array(NA, dim=c(I, 85))
    for(i in 1:I)
    {
        for(j in 1:85)
        {
            yearly.mf.damage[i,j] <- median.damage[j] * getDamageMult(median.sim[j],
                                                                      damage.scenario[i],
                                                                      E, slopes)
        }
        yearly.mf.damage[i,] <- yearly.mf.damage[i,]/accum.discount.rate
    }
    total.mf.damage <- apply(yearly.mf.damage,1,sum)
    
    ## Uncertainty in slr only 
    yearly.slr.damage <- array(NA, dim=c(I, 85))
    for(i in 1:I)
    {
        for(j in 1:85)
        {
            yearly.slr.damage[i,j] <- median.damage[j] * get.median.damage.mult(sim[i, (j+16)])
        }
        yearly.slr.damage[i,] <- yearly.slr.damage[i,]/accum.discount.rate
    }
    total.slr.damage <- apply(yearly.slr.damage,1,sum)

    ## Uncertainty in damages only 
    yearly.dam.damage <- array(NA, dim=c(I, 85))
    for(i in 1:I)
    {
        for(j in 1:85)
        {
            yearly.dam.damage[i,j] <- orig.damage[i,j] * get.median.damage.mult(median.sim[j])
        }
        yearly.dam.damage[i,] <- yearly.dam.damage[i,]/accum.discount.rate
    }
    total.dam.damage <- apply(yearly.dam.damage,1,sum)

   
    res.unc <- list()
    res.unc$mf <- total.mf.damage
    res.unc$slr <- total.slr.damage
    res.unc$dam <- total.dam.damage
    res.unc$yearly.median <- yearly.median.damage
    return(res.unc)
}
