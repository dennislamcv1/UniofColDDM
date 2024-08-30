# Startup Code ------------------------------------------------------------
require(lolcat)          
require(car)
require(rcompanion)
require(ggplot2)
options(scipen=99,digits = 10)
nqtr <- function(x,d){noquote(t(round.object(x, d)))}
ro <- round.object

# Import Data File --------------------------------------------------------
View(Solder)
str(Solder)

# Specify Factors with labels -------------------------------------------
group.labels<-c("New1","New2","New3","Current")
Solder$method<-factor(Solder$method, labels = group.labels)
str(Solder)

# Histograms --------------------------------------------------------------
process.group.plot(fx = push ~ method, data = Solder)
dev.off()

# Boxplots ----------------------------------------------------------------
boxplot(push ~ method,data=Solder, 
        xlab="Soldering Method", 
        ylab="Criterion Measure Means", 
        main="Push Force Means By Soldering Method", 
        col="red")   


# Means Plots (rcompanion) ------------------------------------------------
(sum.out<-groupwiseMean(formula = push~method, 
                        data = Solder, 
                        conf = 0.95, 
                        digits = 4))

# Line Plot ---------------------------------------------------------------
plot(so$mean, xaxt="n", type = "o", lty = 1, pch = 19 
     , cex = 1.1, lwd= 1.7, col="blue" 
     , xlab = "Soldering Method" , ylab = "Mean Values"
     , main = "Mean by Method")
axis(1, at=1:4, labels=group.labels)

