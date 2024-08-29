# Startup Code ------------------------------------------------------------
require(lolcat)
require(dplyr)
require(flextable)
ro <- round.object # Easy version of Rounding Objects
nqtr <- function(x,d){noquote(t(round.object(x, d)))}
options(scipen=999)

# **** The Binomial Distribution **** -------------------------------------
# Binomial Example - manual calculation -----------------------------------
p<-0.80
q<-1-p
r<-45
n<-50

factorial(n)/(factorial(r)*factorial(n-r))*(p^r)*(q^(n-r))

# Binomial Example - dbinom -----------------------------------------------
dbinom(x = 45, size = 50, prob = 0.8)

# Binomial Example - table ------------------------------------------------
ro(table.dist.binomial(n = 50, p = 0.80),5)

# Barplot of Binomial Probability Distribution ----------------------------
n<-50
p<-0.80

# Code to make reasonable x axis
st_dev_norm<-(n*p*(1-p))^.5
bi_min<-as.integer(max(0,(p*n)-4*st_dev_norm))
bi_max<-as.integer(min(n,(p*n)+4*st_dev_norm))

# Create binomial distribution
bi_plot <- dbinom(x = bi_min:bi_max,
                  size = n,
                  prob = p)
names(bi_plot)<-bi_min:bi_max
barplot(
  height = bi_plot,
  main = paste("Binomial Distribution with \U03C0 = ", p, "and n =", n),
  xlab = "r",
  ylab = "Probability at r",
)

# Using pbinom ------------------------------------------------------------
# Binomial Probability of >=45
# Note that pbinom gives P[X>x] for upper tail probabilities
pbinom(q = 44, size = 50, prob = 0.80
       , lower.tail = F)

# **** The Poisson Distribution **** --------------------------------------

# Poisson Example - manual calculation ------------------------------------
lambda<-25
X<-10

(lambda^X/factorial(X))*exp(-lambda)

# Poisson Example - dpois -------------------------------------------------
dpois(x = 10, lambda = 25)

# Poisson Example - table -------------------------------------------------
ro(table.dist.poisson(lambda = 25),5)
ro(table.dist.poisson(lambda = 25)[7:51,],5)

# Barplot of Poisson Probability Distribution ----------------------------
lambda<-25
X<-10

data<-dpois(x = 5:52, lambda = lambda)
names(data)<-5:52
barplot(data
        , xlab = "Parts Produced Per Hour"
        , ylab = "P(X)", ylim = c(0, 0.09))

# Using ppois -------------------------------------------------------------

# Poisson Probability of 18 or fewer (P(X)<=x)
ppois(q = 18, lambda = 25, lower.tail = T)

# Poisson Probability of 20 or more (P(X)>x)
ppois(q = 19, lambda = 25, lower.tail = F) 

# Note that for upper tail, ppois calculates X>x (not including), 
# so go one fewer

# Poisson Probability of at least 10, 
# but no more than 20?

(ft9<-ppois(q = 9, lambda = 25, lower.tail = T))
(ft20<-ppois(q = 20, lambda = 25, lower.tail = T))

ft20-ft9

# Poisson distribution testing --------------------------------------------
set.seed(100)
poisdist<-rpois(n = 100, lambda = 25)
poisson.dist.test(poisdist)

# Import Discrete File - Test the Defects data
poisson.dist.test(Discrete$DEFECTS)

# Create histogram
hist.ungrouped(Discrete$DEFECTS)

