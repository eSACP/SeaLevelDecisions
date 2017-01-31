### Sample damage distributions for each year 2016-2100 (85 years)
### under various settings. 

### Calculates three different types of trajectories
### -- No adaptation
### -- No sea level change
### -- Adaptation in year k for all k 

getDamageTrajectories <- function(E, slopes, adaptation.cost)
{
    ## Read in sea level rise projections for Bergen. 
    load("data/Simulation.Rdata")
    I <- dim(sim)[1]
    
    ## Get discount rates 
    discount.rate <- getDiscountRate()
    accum.discount.rate <- cumprod(discount.rate)

    ## Sample I iid damage costs for each year 2016-2100
    ## This sample is used for all subsequent scenarios 
    orig.damage <- array(rburr(I*85, 
                               shape1=par.est[1], 
                               shape2=par.est[2], 
                               rate=par.est[3]), 
                         dim=c(I, 85))
    
    ## Sample I damage change profiles
    ## This sample is used for all subsequent scenarios 
    damage.scenario <- sample(1:15, I, replace=TRUE)

    ## Combine damage costs, damage change profiles and sea level
    ## projections 
    yearly.damage <- array(NA, dim=c(I, 85))
    for(i in 1:I)
    {
        for(j in 1:85)
        {
            yearly.damage[i,j] <- orig.damage[i,j] * getDamageMult(sim[i, (j+16)],
                                                                   damage.scenario[i])
        }
        yearly.damage[i,] <- yearly.damage[i,]/accum.discount.rate
    }
    
    ## Combine damage costs, damage change profiles and sea level
    ## projections assuming all subsequent years are like 2016. 
    constant.damage <- array(NA, dim=c(I, 85))
    for(i in 1:I)
    {
        for(j in 1:85)
        {
            constant.damage[i,j] <- orig.damage[i,j] * getDamageMult(sim[i, 16], 
                                                                     damage.scenario[i])
        }
        constant.damage[i,] <- constant.damage[i,]/accum.discount.rate
    }
    
    ## Combine damage costs, damage change profiles and sea level
    ## projections under adaptation. Adaptation measure is assumed
    ## to reduce 50% of the yearly damages to damages under 75 cm
    ## lower sea level.  
    adapted.damage <- array(NA, dim=c(85, I, 85))

    for(k in 1:85)
    {
        print(k)
        adapted.slr <- c( rep(0, k-1), rep(75, 85-k+1) )
        for(i in 1:I)
        {
            for(j in 1:85)
            {
                adapted.damage[k,i,j] <- orig.damage[i,j]/2 *
                    getDamageMult(sim[i, (j+16)], damage.scenario[i]) +
                    orig.damage[i,j]/2 *
                    getDamageMult(sim[i, (j+16)]-adapted.slr[j], damage.scenario[i])
            }
            m <- which(adapted.slr==75)[1]
            adapted.damage[k,i,m] <- adapted.damage[k,i,m] + adaptation.cost
            adapted.damage[k,i,] <- adapted.damage[k,i,]/accum.discount.rate
        }    
    }

    res <- list()
    res$orig <- orig.damage
    res$scenario <- damage.scenario
    res$yearly <- yearly.damage
    res$constant <- constant.damage
    res$adapted <- adapted.damage
    return(res)
}
