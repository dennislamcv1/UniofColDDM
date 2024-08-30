# Startup Code
require(lolcat)
require(rcompanion)
require(ggplot2)
ro<-round.object
nqtr<-function(x,d){noquote(t(round.object(x, d)))}
options(scipen=999)

# Repeated Measures t test for Means (Dependent by Nature) -------------------------------------------------
View(Noise)

summary.continuous(Noise)[-1,] # drop the motor identifier

# Calculate difference between Old and New - must check differences for 
# normality with a dependent test for continuous data
Noise$Diff<-Noise$New-Noise$Old

summary.continuous(Noise)[-1,]

# Drop first column
Noise<-Noise[-1]

# Repeated Measures t test
t.test.twosample.dependent(x1 = Noise$New
                           ,x2 = Noise$Old
                           ,alternative = "less")
                           
boxplot(Noise$Old, Noise$New
        ,col="red"
        ,names = c("Old","New")
        ,main = "Noise Level")

# Dbar method - uses differences (if that is all you have to work with)
t.test.twosample.dependent.simple.dbar(pair.differences.mean = mean(Noise$Diff)
                                       ,pair.differences.variance = var(Noise$Diff)
                                       ,alternative = "less"
                                       , sample.size = 10)

# ISO Plot ----------------------------------------------------------------

# Determine min and max limits for x and y axes
min  <- min(range(Noise$Old), range(Noise$New)) # for origin
maxx <- max(range(Noise$Old))
maxy <- max(range(Noise$New))

# Scatter Plot
plot(x = Noise$Old, y = Noise$New, pch=19,
     xlim = c(min,maxx), ylim = c(min,maxy))
abline(lm(formula = New~Old, data = Noise))
abline(coef = c(0,1), lwd = 1, lty = 2, col = "blue")

# Straightening Example
View(Straight)

nqtr(summary.continuous(Straight),4)
boxplot(Straight, col="blue")

Straight$Diff<-Straight$before-Straight$after

nqtr(summary.continuous(Straight),4)

# Iso plot
# Determine min and max limits for x and y axes
min  <- min(range(Straight$before), range(Straight$after)) # for origin
maxx <- max(range(Straight$before))
maxy <- max(range(Straight$after))

# Scatter Plot
plot(x = Straight$before, y = Straight$after, pch=19,
     xlim = c(min,maxx), ylim = c(min,maxy))
abline(lm(formula = before~after, data = Straight))
abline(coef = c(0,1), lwd = 1, lty = 2, col = "blue")

cor(Straight$before, Straight$after)

t.test.twosample.dependent(x1 = Straight$before
                           ,x2 = Straight$after
                           ,alternative = "two.sided")

# Matched Pairs t test (Dependent by Design) ---------------------------
cor.pearson.r.onesample.simple(sample.r = 0.60, sample.size = 30) # 30 pairs

(t.out<-ro(t.test.twosample.dependent.simple.meandiff(sample.mean.g1 = 35.24
                                           ,sample.mean.g2 = 38.02
                                           ,sample.variance.g1 = 5.18^2
                                           ,sample.variance.g2 = 5.63^2
                                           ,sample.size = 30
                                           ,rho.estimate = 0.60),4))

# Make Confidence Intervals easier to read
est<-t.out$estimate # send to object
est<-as.data.frame(est) # make it a dataframe
colnames(est)<-"Value"
View(est) # review in viewer

# or

require(dplyr)
require(flextable)
require(tibble)

est %>%
  tibble::rownames_to_column(var = "Statistic") %>%
  flextable() %>%
  add_header_lines(values = "Confidence Intervals") %>%
  theme_box()

# Braze Example

cor.pearson.r.onesample.simple(sample.r = , sample.size = ) 

# Don't forget to square the standard deviation
(t.out<-ro(t.test.twosample.dependent.simple.meandiff(sample.mean.g1 = 
                                                      ,sample.mean.g2 = 
                                                      ,sample.variance.g1 = 
                                                      ,sample.variance.g2 = 
                                                      ,sample.size = 
                                                      ,rho.estimate = ),4))


# Matched Pairs t test for Variances --------------------------------------
cor.pearson.r.onesample.simple(sample.r = 0.60
                               ,sample.size = 30)

(var.out <-
    variance.test.twosample.dependent.simple(
      sample.variance.g1 = 5.18^2,
      sample.variance.g2 = 5.63^2,
      sample.size = 30,
      rho.estimate = 0.60,
      conf.level = 0.99)
)

# Temper Example
View(Temper)

Temper$Diff<-Temper$before-Temper$after

# Testing for normality
nqtr(summary.continuous(),4)

boxplot()

# ISO Plot
# Determine min and max limits for x and y axes
min  <- min(range(), range()) # for origin
maxx <- max(range())
maxy <- max(range())

# Scatter Plot
plot(x = , y = , pch=19,
     xlim = c(min,maxx), ylim = c(min,maxy))
abline(lm(formula = , data = ))
abline(coef = c(0,1), lwd = 1, lty = 2, col = "blue")

# Correlation
cor()

# Dependent Test for Variation
variance.test.twosample.dependent(g1 = 
                                  , g2 = )
                                  
# Dependent Test for Means
t.test.twosample.dependent(x1 = 
                           , x2 = )

# McNemar's Test for Change - Dependent Proportions -----------------------
# Contingency table format = ct<-(a,c,b,d)
ct<-c(56,56,4,4)

# Create Contingency Table
(ct.new <- matrix(
  ct, nrow = 2,
  dimnames = list(
    "Before Maint" = c("Pass", "Fail"),
    "After Maint" = c("Pass", "Fail"))
))

# Perform McNemar's Test
(mcnemar.out<-proportion.test.mcnemar.simple(b = 4,c = 56))

# Confidence Intervals
(b<-mcnemar.out$estimate[2])
(c<-mcnemar.out$estimate[4])

# Confidence Interval for b
proportion.test.onesample.exact.simple(sample.proportion = 0.06666666667
                                       ,sample.size = 60)

# Confidence Interval for c
proportion.test.onesample.exact.simple(sample.proportion = 0.9333333333 
                                       ,sample.size = 60)

# Shiny Example
View(shiny) # data are in frequency format

(shiny.ct<-matrix(data = shiny$Count
                 , nrow = 2, ncol = 2
                 , byrow = T
                 , dimnames = list("Before Buffing" = c("Accept","Reject")
                                   ,"After Buffing" = c("Accept","Reject")))
)


proportion.test.mcnemar.simple(b =  , c = )

# Two Sample Dependent Test for Counts / Ordinal Data --------------------------------------
View(Fresh)

Fresh$Diff<-Fresh$BeforeTV-Fresh$AfterTV

summary.impl(Fresh, stat.n = T, stat.mean = T, stat.median = T, stat.range = T)

boxplot(Fresh$BeforeTV, Fresh$AfterTV, col="red")

par(mfrow = c(2,1))
hist.ungrouped(Fresh$BeforeTV)
hist.ungrouped(Fresh$AfterTV)
par(mfrow = c(1,1))

median.test.twosample.dependent.wilcoxon(g1 = Fresh$BeforeTV
                                         ,g2 = Fresh$AfterTV)

# Teaching Example
View(teaching)

teaching$Diff<-

summary.impl(teaching, stat.n = T, stat.mean = T, stat.median = T, stat.range = T)

boxplot()

par(mfrow = c(2,1))
hist.ungrouped(teaching$Before, xlim = c(0,5))
hist.ungrouped(teaching$After, xlim = c(0,5))
par(mfrow = c(1,1))

median.test.twosample.dependent.wilcoxon(g1 = 
                                         ,g2 = )

