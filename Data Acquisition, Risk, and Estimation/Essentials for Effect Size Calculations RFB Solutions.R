# Essentials for Effect Size Calculations

# Startup Code
require(lolcat)
ro<-round.object
nqtr<-function(x,d){noquote(t(round.object(x, d)))}
options(scipen=999)

# 1. Determine the Total Annual Loss -----------------------------------------
# A. The Current Total $ Cost or Loss (per year)
(lossperyear<-8200000*0.6)

# 2. Determine the Cost of the Solution -----------------------------------
# B. Cost of the Proposed Solution ($). 
#    The Proposed Solution is presumed to be able to 
#    provide a benefit.
(solutioncost<-2000000)


# 3. Determine the Solution Benefit - Apply the ROI Requirements -----------
# C. ROI Requirements applied to B, the cost of the 
#    Proposed Solution ($) as an annualized 
#    "Solution Benefit." 
(ROIperyear<-(solutioncost*2)/3) # annualized

# 4. Determine the Current % Nonconforming -----------------------------------
#    What you'll need to determine or calculate, but it may be given:  
#    (Draw a picture first):
mu<-60
sd<-8
USL<-72
LL<-mu-(4*sd)
UL<-mu+(4*sd)

f.x <- function(x) {dnorm(x, mean = mu, sd = sd)}
plot(f.x,LL,UL, ylab="Probability Density", lwd=2)
visualize.shade.between.functions(f2.x = f.x
                                  , from=USL
                                  , to= UL
                                  , col="#CFB87C")
abline(v=mu, h=0)

# D.	Current % nonconforming or defective (%)
# a.	This value will have to be given to you - or -
#     you will have to compute/determine it. 

# i.	Continuous data: From Mu, Sigma, SL (the Specification Limit)
#     and a knowledge of normality of the current distribution(process)
#     - or -
pnorm(q = USL, mean = mu, sd = sd, lower.tail = F)
(percloss<-pnorm(q = USL, mean = mu, sd = sd, lower.tail = F)*100)

# What percentage is above the upper specification limit?

# 5. Determine the $ loss per 1% Out of Specification ---------------------
# E. $ Loss value per 1% nonconforming = A/D
(lossper1<-lossperyear/percloss) # $/1%


# 6. Determine the Maximum Allowable Annual Loss -----------------------------
# F. Maximum Allowable (annual) Loss permitted given the 
#    Solution Benefit ($). This is in $ and determined 
#    as A-C.
(solutionbenefit<-lossperyear-ROIperyear) # $

# 7. Determine the New Maximum Allowable Defect Rate to meet ROI -------------
# G. New maximum (allowable) defect rate given the 
#    solution (%) = F/E
(newpercloss<-solutionbenefit/lossper1)
(newproploss<-newpercloss/100)

# 8. Determine Needed Shift in Performance -----------------------------------
# H. What does the new mean have to be, assuming that the 
#    distribution continues to be normal, and the standard
#    deviation the same?
#    Calculate new score associated with that percentage
(znew<-qnorm(newproploss, lower.tail = F))

# Calculate new mean from the new z-score
(munew<-USL-(znew*(sd)))

# Calculate effect size for mean
(deltamu<-munew-mu)

# Effect Size Visualization for Means -----------------------------------------------
# Create the normal curve and shade the upper tail area
mu<-60
sd<-8
USL<-72
LL<-mu-(4*sd)
UL<-mu+(4*sd)

f.x <- function(x) {dnorm(x, mean = mu, sd = sd)}
plot(f.x,LL,UL, ylab="Probability Density", lwd=2, main="Current vs New", xlab="Hours")
visualize.shade.between.functions(f2.x = f.x
                                  , from=USL
                                  , to= UL
                                  , col="#CFB87C")
abline(v=mu,h=0)

par(new=T)
# Add new curve with calculated new mean
f.x <- function(x) {dnorm(x, mean = munew, sd = sd)}
plot(f.x,LL,UL, lwd=2, lty=2, col="blue", xlab="",ylab="",xaxt="n",yaxt="n")
visualize.shade.between.functions(f2.x = f.x
                                  , from=USL
                                  , to= UL
                                  , col="blue")

# Add line at mean
abline(v = munew, lty=2, col="blue")

# 9. Determine the Sample Size Needed to Detect this Shift ----------------
#    What should the sample size be to detect this shift 
#    in means given:
effect<-deltamu
alpha<-0.05
beta<-0.10

# Use t
sample.size.mean.t.onesample(effect.size = deltamu
                             ,variance.est = sd^2
                             ,alpha = alpha
                             ,beta = beta
                             ,alternative = "less")


# Effect Size Calculations for Variability --------------------------------

## H. What does the new standard deviation have to be, 
#     assuming that the distribution continues to be 
#     normal, and the mean the same?
#     Calculate new score associated with that percentage
(znew<-qnorm(newproploss, lower.tail = F))

# Calculate new standard deviation from the new z-score
(sdnew<-(USL-mu)/znew)

# Calculate effect size for standard deviation
(deltasd<-sd-sdnew)

# Effect Size Visualization for Variance -----------------------------------------------

# Create the normal curve and shade the upper tail area
mu<-60
sd<-8
USL<-72
LL<-mu-(4*sd)
UL<-mu+(4*sd)

f.x <- function(x) {dnorm(x, mean = mu, sd = sd)}
plot(f.x,LL,UL, ylab="Probability Density", lwd=2, main="Current vs New", xlab="Hours")
visualize.shade.between.functions(f2.x = f.x
                                  , from=USL
                                  , to= UL
                                  , col="#CFB87C")
abline(v=mu,h=0)

par(new=T)

# Add new curve with calculated new standard deviation
f.x <- function(x) {dnorm(x, mean = mu, sd = sdnew)}
plot(f.x,LL,UL, lwd=2, lty=2, col="blue", xlab="",ylab="",xaxt="n",yaxt="n")
visualize.shade.between.functions(f2.x = f.x
                                  , from=USL
                                  , to= UL
                                  , col="blue")

# 9. Determine the Sample Size Needed to Detect this Shift ----------------
#    What should the sample size be to detect this shift 
#    in variability given:
(effect<-deltasd)
alpha<-0.05
beta<-0.10

# Use 
sample.size.variance.onesample(null.hypothesis.variance = sd^2
                               ,alternative.hypothesis.variance = sdnew^2
                               ,alpha = alpha
                               ,beta = beta
                               ,alternative = "less")


# Effect Size Calculations for Proportions --------------------------------
# Proportions
(p0<-pnorm(q = USL, mean = mu, sd = sd, lower.tail = F))
(p1<-newpercloss/100)
(deltap<-p0-p1)
(p2<-p0 + deltap)

# 9. Determine the Sample Size Needed to Detect this Shift ----------------
#    What is the sample size if I only have proportion data?
sample.size.proportion.test.onesample.exact(null.hypothesis.proportion = p0
                                            ,alternative.hypothesis.proportion = p1
                                            ,alpha = alpha
                                            ,beta = beta
                                            ,alternative = "less")



