### Function that calculates the multiplicative change in damage for sea level x 
### and Hallegatte et al. (2013) estimates for city k.  
getDamageMult <- function(x, k, E=E, slopes=slopes)
{
    if(x < 0) return(1/(1 + abs(x) * slopes$low[k]))
    else if(x < 20) return(1 + x * slopes$low[k])
    else return(as.double(E[k,3]) + x * slopes$high[k])
}
