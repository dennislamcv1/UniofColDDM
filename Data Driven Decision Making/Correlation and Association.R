# Startup Code
require(lolcat)
ro<-round.object
nqtr<-function(x,d){noquote(t(round.object(x, d)))}
options(scipen=999)
options(stringsAsFactors = FALSE)

# Rivet Example -----------------------------------------------------------
View(Rivet)

# Scatterplot example
plot(x = Rivet$paint, y = Rivet$rivet, pch = 19)
abline(lm(rivet~paint, data = Rivet), col = "blue") # lm = linear model

# Install companion to applied regression
install.packages("car")
require(car)

scatterplot(rivet~paint, Rivet)

scatterplotMatrix(Rivet)


# Pearson Product Moment Correlation Coefficient --------------------------
cor(Rivet)
cor(x = Rivet$paint, y = Rivet$rivet)
cor.pearson.r.onesample(x = Rivet$paint
                        ,y = Rivet$rivet
                        ,null.hypothesis.rho = 0
                        , conf.level = 0.95)

# t test for Correlation when rho = 0 -------------------------------------
# t test for Correlation (Correlation)
(cor.pearson<-cor.pearson.r.onesample.simple(sample.r = 0.30
                               ,sample.size = 70
                               ,null.hypothesis.rho = 0
                               ,alternative = "two.sided"
                               ,conf.level = 0.95))


# Importance - r^2 --------------------------------------------------------
str(cor.pearson)
View(cor.pearson)
cor.pearson$estimate
cor.pearson$estimate[4]
(imp.pearson<-cor.pearson$estimate[4]*100)

# Fisher's Z F (Approximate Normal) Test for Correlation rho = rho0 -------
# Same as above, but rho > 0
n<-40
rho<-0.62
r<-0.75

cor.pearson.r.onesample.simple(sample.r = r
                               ,sample.size = n
                               ,null.hypothesis.rho = rho
                               ,alternative = "greater"
                               ,conf.level=0.95)



# Spearman's Rank Order Correlation rs ------------------------------------
View(Spearman)

cor(Spearman$Cracks, Spearman$Passes, method = "spearman")
cor.spearman.rank(x1 = Spearman$Cracks
                  ,x2 = Spearman$Passes)


plot(Spearman$Passes, Spearman$Cracks, pch=19) 
abline(lm(Spearman$Cracks ~ Spearman$Passes), col = "blue" , lwd = 1.5, lty = 1)

# Point Biserial Correlation between Continuous and Nominal ---------------
View(PolyThk)

cor(PolyThk$EOLThick,PolyThk$Line) # use this to get sign or

# Correlation Coefficient, rpbi
cor.test(x = PolyThk$Line
        ,y = PolyThk$EOLThick
        , conf.level = 0.90
        ,exact = T)

plot(PolyThk$Line, PolyThk$EOLThick, pch=19)

abline(
  lm(PolyThk$EOLThick ~ PolyThk$Line),
  col = "purple",
  lwd = 2,
  lty = 1)

t.test.twosample.independent.fx(fx = EOLThick ~ Line
                                , data = PolyThk
                                , conf.level = 0.90)

# Association for Nominal Data --------------------------------------------
# Contingency Table Format
(tbl.fmt <- matrix(
  c(2, 1, 2, 1, 2, 3),
  nrow = 2,
  ncol = 3,
  byrow = TRUE,
  dimnames = list(c("J1", "J2"), c("K1", "K2", "K3"))
))

# Frequency Format
(J <- c(1,1,1,2,2,2))
(K <- c(1,2,3,1,2,3))
(freq <- c(2,1,2,1,2,3))
(freq.fmt <-data.frame(J,K,freq))

# Freq format to cross tabulation
(tbl.from.freq <- transform.individual.format.to.xt(x_row = freq.fmt$J
                                                    ,x_col = freq.fmt$K
                                                    ,weight = freq.fmt$freq
                                                    ,x_row_name ="J"
                                                    ,x_col_name ="K"))
xtabs(freq~J+K, freq.fmt)

# Individual Format
(J <- c(1,1,1,1,1,2,2,2,2,2,2)) 
(K <- c(1,1,2,3,3,1,2,2,3,3,3)) 
(ind.fmt <- data.frame(J, K))

# Individual format to cross tabulation
(tbl.from.ind<-transform.individual.format.to.xt(x_row = ind.fmt$J
                                  ,x_col = ind.fmt$K
                                  ,x_row_name ="J"
                                  ,x_col_name ="K"))
xtabs(~J+K, ind.fmt)


# Casting Example ---------------------------------------------------------
View(Casting)

# Converts from Freq format to Table
castxt<-transform.independent.format.to.xt(x_row = Casting$Filter
                                           ,x_col = Casting$Cracked
                                           ,weight = Casting$Count
                                           ,x_row_name = "Filter"
                                           ,x_col_name = "Cracked")
castxt

# Observed vs Expected
(cast.out<-chisq.test(castxt, correct = F))
cast.out$observed
cast.out$expected

# Conduct test (Pearson Phi)
cor.pearson.phi(castxt)

# Customer and Defect Type Example ----------------------------------------

# 3 by 4 table 
(vec.cus.def <- c(25, 20, 4, 6, 10, 7, 12, 10, 41, 33, 25, 13))
(cust.def <- matrix(vec.cus.def, ncol = 4
  ,dimnames = list(c("Automotive", "Appliance", "Internal")
                   ,c("Blister", "Crack", "Pit", "Scratch"))))

cor.cramer.v(cust.def)

# For ROIStat
customer<-c(1,1,1,1,2,2,2,2,3,3,3,3)
defect<-c(1,2,3,4,1,2,3,4,1,2,3,4)
weight<-c(25,6,12,33,20,10,10,25,4,7,41,13)

df.cust.def<-data.frame(customer, defect, weight)

# Post Hoc

library(dplyr)
library(ggplot2)

df.cust.def %>% 
  group_by(customer, defect) %>% 
  summarise(weight = mean(weight, na.rm = TRUE), .groups = "drop") %>% 
  ggplot(aes(x = defect, y = weight, color = as.factor(customer))) + 
  geom_point() +
  geom_line() +
  ylab("Count") + 
  scale_color_discrete(name = "Customer")

# Correspondence Analysis

# Create table, save in 'custdef'
custdef <- transform.independent.format.to.xt(x_row = df.cust.def$customer
                                          , x_col = df.cust.def$defect
                                          , weight = df.cust.def$weight
                                          , x_row_name = "Customer"
                                          , x_col_name = "Defect")

custdef       # Look at transformed to table data frame

str(custdef)
is.matrix(custdef)
dimnames(custdef) = list(c("Automotive", "Appliance","Internal")
                         , c("Blister", "Crack", "Pit","Scratch"))
custdef

ro(cor.cramer.v(custdef),4)

require(vcd)
tile(custdef)

chisq.out<-chisq.test(custdef)
chisq.out$stdres #adjusted std residuals

std.res<-as.matrix(chisq.out$stdres)

require(ca) #correspondence analysis
prop.table(custdef, 1) # row percentages
prop.table(custdef, 2) # column percentages
fit <- ca(custdef)
print(fit) # basic results
summary(fit) # extended results
plot(fit) # symmetric map
plot(fit, mass = TRUE, contrib = "absolute", map =
       "rowgreen", arrows = c(FALSE, TRUE)) # asymmetric map
