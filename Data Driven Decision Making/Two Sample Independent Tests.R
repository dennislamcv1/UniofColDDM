# Startup Code
require(lolcat)
ro<-round.object
nqtr<-function(x,d){noquote(t(round.object(x, d)))}
options(scipen=999)
options(stringsAsFactors = FALSE)

# Two Independent Sample Hypothesis Tests ---------------------------------------------

# Two Sample t Test for Means, Equal Variance ---------------------------------------

# Use simple when all parameter estimates are given
ro(t.test.twosample.independent.simple(sample.mean.g1 = 0.0060
                                    ,sample.variance.g1 = 0.0015^2
                                    ,sample.size.g1 = 25
                                    ,sample.mean.g2 = 0.0090
                                    ,sample.variance.g2 = 0.0013^2
                                    ,sample.size.g2 = 30
                                    ,conf.level = 0.95),4)
                                    

variance.test.twosample.independent.simple(sample.variance.g1 = 0.0015^2
                                           ,sample.size.g1 = 25
                                           ,sample.variance.g2 = 0.0013^2
                                           ,sample.size.g2 = 30)

# Lipstick Cap Example ----------------------------------------------------
View(CapPull2)

# Use when you have a data file available
t.test.twosample.independent(g1 = CapPull2$pull[CapPull2$mold==1]
                             ,g2 = CapPull2$pull[CapPull2$mold==2])

# Make factors
str(CapPull2)
CapPull2$mold<-as.factor(CapPull2$mold)
str(CapPull2)

# Use when you want to use a formula of y~x and have 
# a data file with factors
t.test.twosample.independent.fx(fx = pull~mold
                                ,data = CapPull2)
# Data visualization
boxplot(pull~mold, data = CapPull2, col="red")

process.group.plot(fx = pull~mold,data = CapPull2)

summary.continuous(fx = pull~mold, data = CapPull2)

# Two Sample t Test, Unequal Variance ---------------------------------------

ro(t.test.twosample.independent.simple(sample.mean.g1 = 75
                                    ,sample.variance.g1 = 20^2
                                    ,sample.size.g1 = 12
                                    ,sample.mean.g2 = 82
                                    ,sample.variance.g2 = 9^2
                                    ,sample.size.g2 = 12
                                    ,conf.level = 0.90),4)

# Point estimate for the mean is the weighted mean
weighted.mean(x = c(75,82), w = c(12,12))


# Tool Life Example -------------------------------------------------------
View(ToolLife)

boxplot(life~vendor, ToolLife, col="red")
ToolLife$vendor<-as.factor(ToolLife$vendor)
process.group.plot(fx = life~vendor, data = ToolLife)

nqtr(summary.continuous(fx = life~vendor, data = ToolLife),4)

t.test.twosample.independent.fx(fx = life~vendor, data = ToolLife)

# Two Sample F Test for Variances -----------------------------------------

ro(variance.test.twosample.independent.simple(sample.variance.g1 = 0.0015^2
                                           ,sample.size.g1 = 25
                                           ,sample.variance.g2 = 0.0013^2
                                           ,sample.size.g2 = 30
                                           ,conf.level = 0.95),4)

# Compare results to t test
ro(t.test.twosample.independent.simple(sample.mean.g1 = 0.0060
                                    ,sample.variance.g1 = 0.0015^2
                                    ,sample.size.g1 = 25
                                    ,sample.mean.g2 = 0.0090
                                    ,sample.variance.g2 = 0.0013^2
                                    ,sample.size.g2 = 30
                                    ,conf.level = 0.95),4)

# Point estimate for the variance is the weighted mean of the variance
(pooled.var<-weighted.mean(x = c(0.0015^2,0.0013^2), w = c(25,30))) # pooled variance

# To get standard deviation, take the square root
sqrt(pooled.var)

# Thickness Example -------------------------------------------------------
View(Thick2)

# 1. Evaluate for normality
Thick2$batch<-as.factor(Thick2$batch)

ro(summary.continuous(thick~batch, data=Thick2),4)

# 1a. Create a boxplot and or histogram of both groups
process.group.plot(fx = thick~batch, data = Thick2)

# 2. Test for dispersion
ro(variance.test.twosample.independent.fx(thick~batch, data = Thick2),6)

# 3. Test for Means to get Confidence Intervals
ro(t.test.twosample.independent.fx(thick~batch, data = Thick2),6)

boxplot(thick~batch, data = Thick2, col="goldenrod")

# The Levene Test for Dispersion ------------------------------------------
View(ToolLife)

# Test for normality
summary.continuous(fx = life~vendor, data = ToolLife)
process.group.plot(fx = life~vendor, data = ToolLife)

# Review structure of the file
str(ToolLife)

# Reclassify vendor as a factor instead of an integer
ToolLife$vendor<-as.factor(ToolLife$vendor)

# Compute the group dispersion scores for the ADA
ToolLife$ADA<-compute.group.dispersion.ADA(fx = life~vendor
                             ,data = ToolLife)

# Use the t test to compare dispersion for each group
t.test.twosample.independent.fx(fx = ADA~vendor
                                ,data = ToolLife)

# Calculate the median by group
summary.impl(fx = life~vendor
             ,data = ToolLife, stat.n=T,stat.median = T)

# Compute the group dispersion scores for the ADM
ToolLife$ADM<-compute.group.dispersion.ADM(fx = life~vendor
                                               ,data = ToolLife)

# Compute the group dispersion scores for the ADMn-1
ToolLife$ADMn1<-compute.group.dispersion.ADMn1(fx = life~vendor
                                           ,data = ToolLife)

View(ToolLife)

# Use the t test to compare groups
t.test.twosample.independent.fx(fx = ADMn1~vendor
                                ,data = ToolLife)

# Export Data
write.table(x = ToolLife, file = "G:/My Drive/ToolLife2.txt", sep=" ",row.names = F)

# Two Sample Tests for Proportions ----------------------------------------

# Fisher's Exact Test
proportion.test.twosample.exact.simple(sample.proportion.g1 = 0.18
                                       ,sample.size.g1 = 750
                                       ,sample.proportion.g2 = 0.12
                                       ,sample.size.g2 = 750
                                       ,conf.level = 0.99)

barplot(height = c(0.18*750, 0.12*750)
        , names.arg = c("Blow Molder 1", "Blow Molder 2")
        , main = "Number Defective (n=750 / group)"
        , col = c("red","blue"), )

# Proportion Visual Nonconforming Example ---------------------------------
View(visual)

summary.impl(visual, stat.n = T,stat.mean = T)
table(visual$g1)
table(visual$g2)

proportion.test.twosample.exact.simple(sample.proportion.g1 = 0.054
                                         ,sample.size.g1 = 500
                                         ,sample.proportion.g2 = 0.036
                                         ,sample.size.g2 = 500
                                         ,conf.level = 0.95)
(count.g1<-sum(visual$g1>0))
(count.g2<-sum(visual$g2>0))

barplot(height = c(count.g1, count.g2)
        , names.arg = c("Group 1","Group 2")
        , main = "Number Defective (n=500 / group)"
        , col = c("red","blue"))

# Pooled proportion when you don't reject the null hypothesis
weighted.mean(x = c(0.18,0.12), w = c(500,500))

# Two Sample Independent Tests for Poisson Rates (Counts) -----------------
View(Eddycur) 

# Descriptive Summary
summary.impl(Eddycur$Old, stat.n = T, stat.mean = T)
summary.impl(Eddycur$New, stat.n = T, stat.mean = T)

boxplot(Eddycur)
  
# Test for Poisson distribution
poisson.dist.test(Eddycur$Old)
poisson.dist.test(Eddycur$New)

# Poisson test
# Remember that sample count has to be 
# n times lambda
(count_before<-sum(Eddycur$Old))
(n_before<-length(Eddycur$Old))

(count_after<-sum(Eddycur$New))
(n_after<-length(Eddycur$New))

poisson.test.twosample.simple(sample.count.g1 = 112
                              ,sample.size.g1 = 130
                              ,sample.count.g2 = 260
                              ,sample.size.g2 = 130
                              ,conf.level = 0.95)
                            

poisson.test.twosample.simple(sample.count.g1 = 4.5*12
                              ,sample.size.g1 = 12
                              ,sample.count.g2 = 7.2*10
                              ,sample.size.g2 = 10
                              ,conf.level = 0.95)

# Nonparametric Test for Independent Groups -------------------------------
View(Frosting)

# Descriptive Summary
process.group.plot(fx = Rating~Frosting, data = Frosting
                   , FUN = hist.grouped
                   , interval.size = 1, anchor.value = 3)

boxplot(Rating~Frosting, data = Frosting)

summary.impl(fx = Rating~Frosting, data = Frosting, stat.n = T
             , stat.mean = T, stat.median = T
             , stat.range = T)

# Mann Whitney U
median.test.twosample.independent.mann.whitney.fx(Rating ~ Frosting
                                                  , data = Frosting)


Frosting$Frosting<-factor(Frosting$Frosting
                             ,levels = c(1,2)
                             ,labels = c("Pink", "White"))

# Make 2 histograms on same scale
install.packages("ggplot2")
require(ggplot2)
ggplot(Frosting, aes(Rating, fill=Frosting)) + 
  geom_histogram(position="identity", colour="grey40", binwidth=1) +
  facet_grid(Frosting ~ .)


# Dining Example ----------------------------------------------------------
View(Dining)

par(mfrow = c(2, 1))
hist.ungrouped(Dining$Local, xlim=(c(0,5)), main="Local")
hist.ungrouped(Dining$Tourists, xlim=c(0,5), main="Tourists")
par(mfrow = c(1, 1))

median.test.twosample.independent.mann.whitney(g1 = Dining$Local
                                               , g2 = Dining$Tourists
                                               ,alternative = "two.sided")

