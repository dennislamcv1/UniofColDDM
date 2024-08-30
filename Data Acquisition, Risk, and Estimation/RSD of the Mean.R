# Startup code - run every time -------------------------------------------
require(lolcat)
ro <- round.object # Easy version of Rounding Objects
nqtr <- function(x,d){noquote(t(round.object(x, d)))}
options(scipen=999)

# Using the RSD to solve probability problems -----------------------------

# Example 1 ---------------------------------------------------------------

# Define the variables in the problem
mu1<-1.325
sigma1<-0.045
n1<-25
xbar1<-1.433
stderror1<-sigma1/sqrt(n1)

# Calculate the area under the normal curve using the pnorm function
pnorm(q = xbar1, mean = mu1
      ,sd = stderror1, lower.tail = F)

#  Visualization of the area of interest ----------------------------------
LL<-mu1-5*stderror1
UL<-mu1+15*stderror1

f.x <- function(x) {dnorm(x, mean = mu1, sd = stderror1)}
plot(f.x,LL,UL, ylab="Probability Density")

# Indicate the location of the xbar of 1.433
abline(v=1.433)

# Example 2 ---------------------------------------------------------------
# Define the variables in the problem
mu2<-50
sigma2<-14.4
n2<-16
xbar2<-55
stderror2<-sigma2/sqrt(n2)

# Calculate the area under the normal curve using the pnorm function
pnorm(q = xbar2, mean = mu2
      ,sd = stderror2, lower.tail = F)

#  Visualization of the area of interest ----------------------------------
LL<-mu2-3.5*stderror2
UL<-mu2+3.5*stderror2

f.x <- function(x) {dnorm(x, mean = mu2, sd = stderror2)}
plot(f.x,LL,UL, ylab="Probability Density")
visualize.shade.between.functions(f2.x = f.x
, from=xbar2
, to=UL
, col="#CFB87C")

# Indicate the location of the xbar of 55
abline(v=55)

# The Bank Problem --------------------------------------------------------
mu<-2000
stdev<-600
n<-100

# And normally distributed!

# Probability (think pnorm) that the AVERAGE 
# is between 1900 and 2050

# Calculate the Standard Error
(stderr<-stdev/sqrt(n))

# Area below 2050
pnorm(q = 2050, mean = mu
      , sd = stderr, lower.tail=T)

# Area below 1900
pnorm(q = 1900, mean = mu
      , sd = stderr, lower.tail=T)

(bank.out<-pnorm(q = c(2050,1900), mean = mu, sd = stderr))
bank.out[1]-bank.out[2]

#  Visualization of the area of interest ----------------------------------
mu<-2000
sd<-stderr
LL<-mu-3.5*sd
UL<-mu+3.5*sd

f.x <- function(x) {dnorm(x, mean = mu, sd = sd)}
plot(f.x,LL,UL, ylab="Probability Density")
visualize.shade.between.functions(f2.x = f.x
, from=1900
, to=2050
, col="#CFB87C")

abline(v=2000)
