# Introduction to Hypothesis Testing
# Startup Code
require(lolcat)
ro<-round.object
nqtr<-function(x,d){noquote(t(round.object(x, d)))}
options(scipen=999)

# Industrial Example 1 ----------------------------------------------------
Mu<-0.55
Xbar<-0.53
sigma<-0.04
n<-50

(stderr<-sigma/(sqrt(n)))
(Z<-(Xbar-Mu)/stderr)

visualize.norm(stat = 0.53
               , mu = Mu
               , sd = stderr
               , section = "lower")

pnorm(q = 0.53, mean = Mu
      , sd = stderr
      , lower.tail = T)

pnorm(-3.535534)

# What if sample size was 100?
n<-100
(stderr<-sigma/(sqrt(n)))

visualize.norm(stat = 0.53
               , mu = Mu
               , sd = stderr
               , section = "lower")

pnorm(q = 0.53, mean = Mu, sd = stderr, lower.tail = T)

# What if sample size was 10?
n<-10
(stderr<-sigma/(sqrt(n)))
visualize.norm(stat = 0.53
               , mu = Mu
               , sd = stderr
               , section = "lower")
pnorm(q = 0.53, mean = Mu, sd = stderr, lower.tail = T)


# Calculating beta and power for means, sigma known -----------------------------------------
# Note that effect size for one-tailed hypotheses
# is negative when we are dealing with means and the alternative is "less"

# When sigma is known, you may use z
nqtr(power.mean.z.onesample(sample.size = 100
                       ,effect.size = -5
                       ,variance = 16.9^2
                       ,alpha = 0.05
                       ,alternative = "less"),4)

# Calculating beta and power for means, sigma unknown -------------------------------
# Detecting a larger mean, alternative = "greater"
nqtr(power.mean.t.onesample(sample.size = 15
                            ,effect.size = 0.4
                            ,variance = 1.02^2
                            ,alpha = 0.05
                            ,alternative = "greater"),4)


# Calculating beta and power for variance / std deviation -----------------
# Detecting a change in variability, alternative = "two.sided"
power.variance.onesample(sample.size = 9
                         , null.hypothesis.variance = 15^2
                         , alternative.hypothesis.variance = 17.5^2
                         , alpha = 0.05
                         , alternative = "two.sided")

# Calculating beta and power for proportions ----------------------------
power.proportion.test.onesample.exact(sample.size = 100
                                      , null.hypothesis.proportion = 0.1
                                      , alternative.hypothesis.proportion = 0.05
                                      , alpha = 0.05
                                      , alternative = "two.sided")

# Calculating beta and power for rates (Poisson) --------------------------
power.count.poisson.onesample.exact(sample.size = 70
                                    , lambda.null.hypothesis = 3
                                    , lambda.alternative.hypothesis = 2.4286
                                    , alpha = 0.01
                                    , alternative = "less")

# Calculating Sample Size -------------------------------------------------
# Sample Size Calculations for Changes in Means ---------------------------

# Sample Size, sigma known, one sample
Mu<-75
sigma<-15
alpha<-0.05
beta<-0.20
delta<-7.5

# Sigma known, directional, one sample
nqtr(sample.size.mean.z.onesample(effect.size = delta
                             ,variance = sigma^2
                             ,alpha = alpha
                             ,beta = beta
                             ,alternative = "greater"),4)

nqtr(sample.size.mean.z.onesample(effect.size = -delta
                             ,variance = sigma^2
                             ,alpha = alpha
                             ,beta = beta
                             ,alternative = "less"),4)

# Sigma known, non-directional, one sample
nqtr(sample.size.mean.z.onesample(effect.size = delta
                             ,variance = sigma^2
                             ,alpha = alpha
                             ,beta = beta
                             ,alternative = "two.sided"),4)

# Sigma unknown, directional, one sample
nqtr(sample.size.mean.t.onesample(effect.size = delta
                             ,variance.est = sigma^2
                             ,alpha = alpha
                             ,beta = beta
                             ,alternative = "greater"),4)

nqtr(sample.size.mean.t.onesample(effect.size = -delta
                             ,variance.est = sigma^2
                             ,alpha = alpha
                             ,beta = beta
                             ,alternative = "less"),4)

# Sigma unknown, non-directional, one sample
nqtr(sample.size.mean.t.onesample(effect.size = delta
                             ,variance.est = sigma^2
                             ,alpha = alpha
                             ,beta = beta
                             ,alternative = "two.sided"),4)

# Sample Size Calculations for Changes in Variance ---------------------------
# Sample Size, one sample
sigma0<-15 # 225 variance
sigma1<-18 # 324 variance
sigma2<-12 # 144 variance
alpha<-0.05
beta<-0.20


# One sided, one sample
nqtr(sample.size.variance.onesample(null.hypothesis.variance = sigma0^2
                               ,alternative.hypothesis.variance = sigma2^2
                               ,alpha = alpha
                               ,beta = beta
                               ,alternative = "less"),4)

nqtr(sample.size.variance.onesample(null.hypothesis.variance = sigma0^2
                               ,alternative.hypothesis.variance = sigma1^2
                               ,alpha = alpha
                               ,beta = beta
                               ,alternative = "greater"),4)

# Two sided, one sample (testing for a change in either direction)
# In this situation, I use the two.sided test for both the larger and smaller 
# variance, and take the larger sample size of the two.

nqtr(sample.size.variance.onesample(null.hypothesis.variance = sigma0^2
                               ,alternative.hypothesis.variance = sigma1^2
                               ,alpha = alpha
                               ,beta = beta
                               ,alternative = "two.sided"),4)

nqtr(sample.size.variance.onesample(null.hypothesis.variance = sigma0^2
                               ,alternative.hypothesis.variance = sigma2^2
                               ,alpha = alpha
                               ,beta = beta
                               ,alternative = "two.sided"),4)

# Sample Size Calculations for Changes in Proportions ---------------------------
# Sample size for Proportions
pi0<-0.2
pi1<-0.3
pi2<-0.1
alpha<-0.05
beta<-0.10

# One sided, one sample
nqtr(sample.size.proportion.test.onesample.exact(null.hypothesis.proportion = pi0
                                            ,alternative.hypothesis.proportion = pi2
                                            ,alpha = alpha
                                            ,beta = beta
                                            ,alternative = "less"),4)

nqtr(sample.size.proportion.test.onesample.exact(null.hypothesis.proportion = pi0
                                            ,alternative.hypothesis.proportion = pi1
                                            ,alpha = alpha
                                            ,beta = beta
                                            ,alternative = "greater"),4)

# Two sided, one sample (testing for a change in either direction)
# In this situation, I use the two.sided test for both the larger and smaller 
# proportion, and take the larger sample size of the two.

nqtr(sample.size.proportion.test.onesample.exact(null.hypothesis.proportion = pi0
                                            ,alternative.hypothesis.proportion = pi1
                                            ,alpha = alpha
                                            ,beta = beta
                                            ,alternative = "two.sided"),4)

nqtr(sample.size.proportion.test.onesample.exact(null.hypothesis.proportion = pi0
                                            ,alternative.hypothesis.proportion = pi2
                                            ,alpha = alpha
                                            ,beta = beta
                                            ,alternative = "two.sided"),4)


# Sample Size Calculation for Poisson Rates -------------------------------
sample.size.count.poisson.onesample.exact()
