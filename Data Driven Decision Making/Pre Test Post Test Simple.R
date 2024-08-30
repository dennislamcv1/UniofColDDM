# Startup Code
require(lolcat)
ro<-round.object
nqtr<-function(x,d){noquote(t(round.object(x, d)))}
options(scipen=999)

# Problem 1 - Compare both PRE groups -------------------------------------
# Independent Groups

# Test for Normality
summary.continuous(Red$PreG1a)
summary.continuous(Red$PreG2a)

hist.grouped(Red$PreG1a)
hist.grouped(Red$PreG2a)

boxplot(Red$PreG1a, Red$PreG2a)

# Test for variance
variance.test.twosample.independent(g1 = Red$PreG1a
                                    ,g2 = Red$PreG2a)

# Test for means
t.test.twosample.independent(g1 = Red$PreG1a
                             ,g2 = Red$PreG2a)

# Proportions
mean(scratches$preg1s)
mean(scratches$preg2s)

hist.ungrouped(scratches$preg1s)
hist.ungrouped(scratches$preg2s)

proportion.test.twosample.exact.simple(sample.proportion.g1 = 0.21
                                       ,sample.size.g1 = 200
                                       , sample.proportion.g2 = 0.19
                                       , sample.size.g2 = 200)

# Problem 2 - Group 1 Pre to Group 1 Post ---------------------------------
# Dependent Groups - Repeated Measures

Red$PrePostG1Diff<-Red$PreG1a-Red$PostG1a

# Test for normality
summary.continuous(Red$PreG1a)
summary.continuous(Red$PostG1a)
summary.continuous(Red$PrePostG1Diff)

hist.grouped(Red$PreG1a)
hist.grouped(Red$PreG2a)
hist.grouped(Red$PrePostG1Diff)

# Variance - Dependent
variance.test.twosample.dependent(g1 = Red$PreG1a
                                  ,g2 = Red$PostG1a)
# Means - Dependent
t.test.twosample.dependent(x1 = Red$PreG1a
                           , x2 = Red$PostG1a)

boxplot(Red$PreG1a, Red$PostG1a)

# McNemar's
table(scratches$preg1s, scratches$postg1s)

proportion.test.mcnemar.simple(b = 26, c = 35)


# Problem 3 - Group 2 Pre to Group 2 Post ---------------------------------
Red$PrePostG2Diff<-Red$PreG2a-Red$PostG2a

# Test for normality
summary.continuous(Red$PreG2a)
summary.continuous(Red$PostG2a)
summary.continuous(Red$PrePostG2Diff)

# Variance
variance.test.twosample.dependent(g1 = Red$PreG2a
                                  , g2 = Red$PostG2a)

# Means
t.test.twosample.dependent(x1 = Red$PreG2a
                           , x2 = Red$PostG2a)

boxplot(Red$PreG2a, Red$PostG2a)
abline(h=47)

# Proportions
table(scratches$preg2s, scratches$postg2s)

proportion.test.mcnemar.simple(b = 14, c = 34)

# Problem 4 - Compare Both POST Groups --------------------------------------------

# Test for normality - already performed on all groups

# Test for variance
variance.test.twosample.independent(g1 = Red$PostG1a
                                    ,g2 = Red$PostG2a)

# Test for means
t.test.twosample.independent(g1 = Red$PostG1a
                             ,g2 = Red$PostG2a)

# Proportions
mean(scratches$postg1s)
mean(scratches$postg2s)

hist.ungrouped(scratches$postg1s)
hist.ungrouped(scratches$postg2s)

proportion.test.twosample.exact.simple(sample.proportion.g1 = 0.165
                                       ,sample.size.g1 = 200
                                       , sample.proportion.g2 = 0.09
                                       , sample.size.g2 = 200)

