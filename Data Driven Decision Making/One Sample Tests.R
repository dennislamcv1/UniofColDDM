# Startup Code
require(lolcat)
ro<-round.object
nqtr<-function(x,d){noquote(t(round.object(x, d)))}
options(scipen=999)

# One Sample Z Test, Sigma is Known ---------------------------------------
# Axle example
mu<-80000
sigma<-4000
n<-100
xbar<-79600

z.test.onesample.simple(sample.mean = xbar
                        ,known.population.variance = sigma^2
                        ,sample.size = n
                        ,null.hypothesis.mean = mu
                        ,alternative = "two.sided"
                        ,conf.level = 0.95)

# One Sample t Test, Sigma is Unknown ---------------------------------------
mu<-12.50
sigma<-0.2
n<-60
xbar<-12.31

ro(t.test.onesample.simple(sample.mean = xbar
                        ,sample.variance = sigma^2
                        ,sample.size = n
                        ,null.hypothesis.mean = 12.50
                        ,alternative = "two.sided"
                        ,conf.level = 0.99),4)

# One Sample Test for Mean - Lipstick Case -----------------------------------------
View()

# Summary
hist.grouped()
nqtr(summary.continuous(),6)

# Hypothesized Value
mu<-0.733

ro(t.test.onesample(x = 
                    , null.hypothesis.mean = 
                    , alternative = "two.sided"
                    , conf.level = 0.90),4)

# Chi Square Test for Variance ---------------------------------------
n<-31
df<-n-1
sigma0<-13
sigma1<-15.9
alpha<-0.10

variance.test.onesample.simple(sample.variance = sigma1^2
                               ,sample.size = n
                               ,null.hypothesis.variance = sigma0^2
                               ,alternative = "two.sided"
                               ,conf.level = 0.90)

# One Sample Test for Variance - Delay Switch -----------------------------------------
View()

# Summary
hist.ungrouped()
nqtr(summary.continuous(, stat.sd=T),6)

ro(variance.test.onesample(g1 = 
                           , null.hypothesis.variance = 
                           , alternative = "two.sided"
                           , conf.level = 0.95),4)

# One Sample Proportion Test ----------------------------------------------
pi0<-0.5
pi1<-0.2
n<-10
r<-2

# Exact Binomial
pbinom(q = 2, size = 10, prob = 0.5, lower.tail = T)*2

ro(table.dist.binomial(n = 10, p = 0.5),3)

# Use this one 
ro(proportion.test.onesample.exact.simple(sample.proportion = pi1
                                       ,sample.size = n
                                       ,null.hypothesis.proportion = pi0
                                       ,alternative = "two.sided"
                                       ,conf.level = 0.95),4)
# Spark plug example
View()

mean()

proportion.test.onesample.exact(x = 
                                  ,null.hypothesis.proportion = 
                                  ,alternative = "two.sided"
                                  ,conf.level = 0.95)

# Wilcoxon Signed Ranks Test for Location (Median) ------------------------

# See if data are symmetrical about the median
hist.ungrouped(FormItEach$rating, xlim = c(0,5))
summary(FormItEach$rating)
boxplot(x = FormItEach$rating)

median.test.onesample.wilcoxon(x = FormItEach$rating
                               ,null.hypothesis.location = 4
                               ,alternative = "two.sided")
                               
# Brightness example
hist.ungrouped()
summary()

median.test.onesample.wilcoxon(x = 
                               ,null.hypothesis.location = 
                               ,alternative = )


# One Sample Poisson Exact Test -------------------------------------------
View(Eddy)

# Poisson Distribution Test
poisson.dist.test(Eddy$Major)

# Descriptive Summary
summary(Eddy$Major)
boxplot(Eddy$Major)
hist.ungrouped(Eddy$Major)

(samp.c<-sum(Eddy$Major))
(n<-length(Eddy$Major))
poisson.test.onesample.simple(sample.count = samp.c
                              ,sample.size = n
                              ,null.hypothesis.lambda = 1
                              ,conf.level = 0.99)

# Power calculation for Poisson -------------------------------------------

power.count.poisson.onesample.exact(sample.size = 130
                                    ,lambda.null.hypothesis = 1
                                    ,lambda.alternative.hypothesis = 0.8615
                                    ,alpha = 0.01
                                    ,alternative = "two.sided")

# Eddy Minor Indications Example
# Poisson Distribution Test
poisson.dist.test()

# Descriptive Summary
summary()
hist.ungrouped()

(samp.c<-sum())
(n<-length())
poisson.test.onesample.simple(sample.count = 
                              ,sample.size = 
                              ,null.hypothesis.lambda = 
                              ,conf.level = 0.99)

