### Discount rate based on Norwegian government recommendations.

getDiscountRate <- function()
{
    return( c( rep(1.04, 40), rep(1.03, 35), rep(1.02, 10)) )
}
