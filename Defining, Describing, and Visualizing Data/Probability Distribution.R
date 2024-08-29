# Startup Code ------------------------------------------------------------
require(lolcat)
require(dplyr)
require(flextable)
ro <- round.object # Easy version of Rounding Objects
nqtr <- function(x,d){noquote(t(round.object(x, d)))}
options(scipen=999)

# Example of Binomial Probability Distribution ----------------------------
table.dist.binomial(n = 2, p = 0.2)

# Bar plot of Binomial Probability Distribution
n<-2
P<-0.2
data<-dbinom(x = 0:n, size = n, prob = P)
names(data)<-0:n
barplot(data, xlab = "# of Defectives"
        , ylab = "P(D)", ylim = c(0,1)
        , axis.lty=1)

# Probability Distribution for Discrete Random Variable -------------------
# Import Daily Production File (no heading)

# Probability Distribution (Histogram)
hist.grouped(Daily.Production$V1
             , freq = F
             , anchor.value=50
             , ylim=c(0,0.20))

# Expected Value of a Discrete Random Variable
mean(Daily.Production$V1)
