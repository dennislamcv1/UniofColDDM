# Startup Code ------------------------------------------------------------
require(lolcat)
require(dplyr)
require(flextable)
ro <- round.object # Easy version of Rounding Objects
nqtr <- function(x,d){noquote(t(round.object(x, d)))}
options(scipen=999)

# Binomial Distribution Practice Problems ---------------------------------
ro(table.dist.binomial(n =  , p = ),5)[1:17,]


# Poisson Distribution Practice Problems ----------------------------------
ro(table.dist.poisson(lambda = ),5)

