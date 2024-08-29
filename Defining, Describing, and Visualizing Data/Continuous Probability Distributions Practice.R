# Startup Code ------------------------------------------------------------
require(lolcat)
require(dplyr)
require(flextable)
ro <- round.object # Easy version of Rounding Objects
nqtr <- function(x,d){noquote(t(round.object(x, d)))}
options(scipen=999)

# Normal Distribution Practice Problems ---------------------------------
pnorm(q = 500, mean = 500, sd = 100, lower.tail = F)

#  Visualization of the area of interest ----------------------------------
mu<-500
sd<-100
LL<-mu-3.5*sd
UL<-mu+3.5*sd

f.x <- function(x) {dnorm(x, mean = mu, sd = sd)}
plot(f.x,LL,UL, ylab="Probability Density")
visualize.shade.between.functions(f2.x = f.x
                                  , from=550
                                  , to=650
                                  , col="#CFB87C")

# Probability of between 550 and 650?
x1<-pnorm(q = 550, mean = 500, sd = 100, lower.tail = T)
x2<-pnorm(q = 650, mean = 500, sd = 100, lower.tail = T)

x2-x1

# Exponential Distribution Practice Problems ------------------------------

# Step 1. Define what you know
x<-48
xmin<-25
mu<-72.5

# Perform Shape test if needed
  
# Step 2. Is it lower tail? Upper tail? Both?
# Lower tail
  
# Step 3. Make a visualization
  # Shade areas under the exponential curve
  # Create the exponential curve
mu<-72.5
LL<-25
UL<-5*mu

f.x <- function(x) {dexp(x, rate = 1/(mu-LL))}
plot(f.x,LL,UL, ylab="Probability Density")
visualize.shade.between.functions(f2.x = f.x, from=LL, to=48)

# Step 4. Find the area under the curve appropriate to the 
# question at hand.
pexp(q = , rate = 1/(), lower.tail = )
pexp.low(q = 48 , low = 25, mean = 72.5, lower.tail = T)


