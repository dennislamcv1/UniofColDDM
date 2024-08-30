# Startup Code
require(lolcat)
require(gtools)
require(matrixStats)
ro<-round.object
nqtr<-function(x,d){noquote(t(round.object(x, d)))}
options(scipen=999)

# RSD of the Means - Example --------------------------------------------
# Population Size = 5
# Measuring age
pop<-c(18,22,24,30,35)

# Calculate Population Mean
mean(pop) 

# Calculate Population Variance
varp <- function(x) mean((x-mean(x))^2)
varp(pop)

# Calculate Population Std Deviation
ro(sqrt(varp(pop)),4)


# Manual RSD
# Given the following N = 5 population values
# 18,22,24,30,35, do the following maintaining at least 4 decimal places:


# 1.Create all possible samples of size n=2 
# (This will be sampling with replacement, obviously.) 
# Put the values in 2 separate columns, one for the first sample value 
# and the second column for the second sample value.

dataset<-c(18,22,24,30,35)
x = permutations(n = 5, r = 2
                 , v = dataset, set = F
                 , repeats.allowed = T)

# n = size of sampling vector 
# r = size of samples 
# v = vector to sample from 

# 2. For each of these 25 samples compute: The sample mean, the sample variance, 
# and the sample standard deviation and put them in separate columns. 
# (You will have 25 rows of each.)

x<-as.data.frame(x)
x$Mean<-rowMeans(x)
x$Variance<-rowVars(as.matrix(x[,1:2]))
View(x)

# Make a picture of the RSD of Means
hist.grouped(x$Mean
             ,interval.size = 3
             , main = "RSD of the Mean, n=2")

# 3a.  For the 25 sample mean values, compute their mean.
mean(x$Mean)

# 3b.	For the 25 sample variances, compute their mean.
mean(x$Variance)

# 3c.	For the 25 sample means, compute their variance 
# (Since this is a Population, you must use the 
# version of the variance that uses "n" in the 
# denominator, NOT "n-1".  
varp <- function(x) mean((x-mean(x))^2)
varp(x$Mean)

# 4.	For the population of 5 scores, 
pop<-c(18,22,24,30,35)

# 4a.	Calculate the population mean.  
mean(pop)

# 4b.	Calculate the population variance 
# (Since it is for a population, be sure to use 
# the "n" version of the variance formula.)
varp <- function(x) mean((x-mean(x))^2)
varp(pop)

# 4c.	Calculate the population standard deviation 
# (use the "n" version of the formula.) 
sqrt(varp(pop))

# 5.	Divide the population Variance by the sample size.
# Can you find another summary calculation with the same value?
varp(pop)/2

# Yes, this is the same as the variance of the 25 means
# of the population (3c)


                                             
