### Hallegatte et al. (2013) provide data for 0, 20, 40 cm sea level rise.
### Conservative extrapolation: Linear extrapolation of [20,40] beyond 40
### and [0,20] below 0. 

getChangeSlopes <- function(E)
{
    x <- c(0, 20, 40)
    N <- dim(E)[1]
    low.slope <- rep(NA, N)
    high.slope <- rep(NA, N)
    for(i in 1:N)
    {
        y <- as.double(E[i,2:4])
        res <- lm(y[1:2]~x[1:2])
        low.slope[i] <- res$coef[2]
        res <- lm(y[2:3]~x[2:3])
        high.slope[i] <- res$coef[2]
    }

    slopes <- list()
    slopes$low <- low.slope
    slopes$high <- high.slope
    return(slopes)
}
