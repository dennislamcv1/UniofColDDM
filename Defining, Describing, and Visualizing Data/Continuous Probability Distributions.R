# Startup Code ------------------------------------------------------------
require(lolcat)
require(dplyr)
require(flextable)
ro <- round.object # Easy version of Rounding Objects
nqtr <- function(x,d){noquote(t(round.object(x, d)))}
options(scipen=999)

# **** The Normal Distribution **** -------------------------------------

# Example 1 - Normal Distribution -------------------------------
# Manual calculation of Z-score
mu<-180
sd<-5
x<-172

(Z<-(x-mu)/sd)

#  Visualization of the area of interest ----------------------------------
mu<-180
sd<-5
LL<-mu-3.5*sd
UL<-mu+3.5*sd

f.x <- function(x) {dnorm(x, mean = mu, sd = sd)}
plot(f.x,LL,UL, ylab="Probability Density")
visualize.shade.between.functions(f2.x = f.x
                                  , from=LL
                                  , to=172
                                  , col="#CFB87C")

# Calculate the area under the normal curve using the Z Score
pnorm(q = -1.6, mean = 0, sd = 1, lower.tail = T)

# Calculate the area under the normal curve using the pnorm function
pnorm(q = 172, mean = 180, sd = 5, lower.tail = TRUE)

# Example 2 - The Normal Distribution -------------------------------------

#  Visualization of the area of interest ----------------------------------
mu<-5.20
sd<-0.05
LL<-mu-3.5*sd
UL<-mu+3.5*sd

f.x <- function(x) {dnorm(x, mean = mu, sd = sd)}
plot(f.x,LL,UL, ylab="Probability Density")
visualize.shade.between.functions(f2.x = f.x
                                  , from=LL
                                  , to=5.15
                                  , col="#CFB87C")

visualize.shade.between.functions(f2.x = f.x
                                  , from=5.35
                                  , to=UL
                                  , col="#CFB87C")                                  

# Calculate the area under the normal curve using the Z Score
pnorm(q = -1.0, mean = 0, sd = 1, lower.tail = T)
pnorm(q = 3, mean = 0, sd = 1, lower.tail = F)

# Calculate the area under the normal curve using the pnorm function
pnorm(q = 5.15, mean = 5.20, sd = .05, lower.tail = T)
pnorm(q = 5.35, mean = 5.20, sd = .05, lower.tail = F)

# Create output variables
(lower<-pnorm(q = 5.15, mean = 5.20, sd = .05, lower.tail = T))
(upper<-pnorm(q = 5.35, mean = 5.20, sd = .05, lower.tail = F))

#Add together for total area under the normal curve
(total=lower+upper)
(totalpercent=total*100)
round.object(totalpercent,2)


# Test for normality when n < 25 ------------------------------------------
set.seed(101)
normdata<-rnorm(n = 24, mean = 10, sd = 2)
anderson.darling.normality.test(normdata)
shapiro.wilk.normality.test(normdata)
summary.continuous(normdata) # picks the right test for you!


# Test for normality when n >= 25 -----------------------------------------
set.seed(102)
normdata2<-rnorm(n = 25, mean = 10, sd = 2)
summary.continuous(normdata2) # picks the right test for you!

# **** Actual vs Predicted **** -------------------------------------------
# Import the FlowRate.txt file

nqtr(summary.continuous(FlowRate$Flow, stat.min=T),4)
hist.grouped(FlowRate$Flow, anchor.value = 0
             , interval.size = 5, freq = F)
hist.add.distribution.curve.normal(FlowRate$Flow, freq=F)

# What percentage of values in the *sample* are < 15?
n<-50
sum(FlowRate$Flow < 15)/n

# What percentage of values in the population are 
# predicted to be < 15?
pnorm(q = 15, mean = mean(FlowRate$Flow), 
      sd = sd(FlowRate$Flow), 
      lower.tail = T)


hist.grouped(x = FlowRate$Flow
             ,anchor.value = 0
             ,interval.size = 5
             ,stat.lsl = 15
             ,freq = F)

hist.add.distribution.curve.normal(x = FlowRate$Flow
                                   ,freq=F)

# Exponential Distribution ------------------------------------------------
# Manual Calculation
exp(-60/100) # with origin parameter at zero

# pexp
pexp(q = 60, rate = 1/100, lower.tail = F) # upper tail


# Manual Calculation
1-exp(-(20-5)/(50-5)) # with origin parameter at 5

# pexp
pexp(q = 20-5, rate = 1/(50-5), lower.tail = T)

# pexp.low
pexp.low(q = 20, low = 5, mean = 50, lower.tail = T)

# Visualization
mu<-50
LL<-5
UL<-5*mu

f.x <- function(x) {dexp(x, rate = 1/(mu-LL))}
plot(f.x,LL,UL, ylab="Probability Density")
visualize.shade.between.functions(f2.x = f.x, from=20
                                  , to=UL, col="orange")
visualize.shade.between.functions(f2.x = f.x, from=LL, to=20)

# Test for exponentiality -------------------------------------------------
# Import exp1.txt file
summary.continuous(exp1$x)
hist.grouped(exp1$x)

shapiro.wilk.exponentiality.test(exp1$x)

# For n > 100 (only for origin parameter of zero)
# Import exp2.txt file
shapetest.exp.epps.pulley.1986(exp2$x)
