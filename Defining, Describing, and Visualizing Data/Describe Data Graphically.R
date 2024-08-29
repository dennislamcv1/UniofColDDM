# Startup Code ------------------------------------------------------------
require(lolcat)
options(stringsAsFactors = FALSE)

# Create data file --------------------------------------------------------
cfm<-c(68,72,72,74,72,69,75,75,72,73,70,71,71,72,73,72,70,72,73,74)
fans<-data.frame(cfm)
View(fans)

# Create a Run Chart ------------------------------------------------------
spc.run.chart(chart.series = fans$cfm
              , main = "Run Chart: Computer Fans"
              , ylab = "CFM"
              , pch = 19, cex=1.2, col="blue"
              , lty=1, lwd=2
              , type = "o")

mean(fans$cfm)
abline(h=72)

# Create an Ungrouped Histogram -------------------------------------------
# Number of unique data values
nrow(unique(fans))

# Ungrouped Histogram
hist.ungrouped(fans$cfm
               , main="Ungrouped Histogram"
               , xlab="CFM")
sort(fans$cfm)

# Create a Grouped Histogram --------------------------------------------

# Import castings file

# Number of unique data values
nrow(unique(castings))

# Grouped Histogram
hist.grouped(castings$weight
             , main="Grouped Histogram: Castings"
             , xlab="Weight")

# Grouped Frequency Distribution
frequency.dist.grouped(castings$weight)

# Change size of class intervals
frequency.dist.grouped(castings$weight
                       ,interval.size = 10)

hist.grouped(castings$weight
             , main="Grouped Histogram: Castings"
             , xlab="Weight"
             , interval.size = 10)
             
# Grouped Histogram with Density
hist.grouped(castings$weight
             , main="Grouped Histogram: Castings"
             , xlab="Weight"
             , interval.size = 10
             , freq = F)

# Freq = F changes to the density property
# where the width of the class interval times
# the height = the relative frequency

# Create a Density Plot ---------------------------------------------------
# Must set the frequency on the y axis to F
hist.grouped(castings$weight
             , xlab="Weight"
             , main = "Histogram with Density Plot"
             , freq=F)

lines(density(castings$weight
              , bw = "SJ"))

# Create Density Plot Only ------------------------------------------------
plot(density(castings$weight, bw = "SJ")
     , main="Density Plot of Casting Weight"
     , xlab="Weight")

# Create Filled Density Plot ----------------------------------------------
dp<-density(castings$weight, bw = "SJ")
plot(dp, main="Density Plot of Casting Weight"
     , xlab="Weight")
polygon(dp, col="red", border="black")

# Create a Boxplot --------------------------------------------------------
# 5 Number Summary
summary(castings$weight)

# Boxplot
boxplot(castings$weight
        , main = "Boxplot of Casting Weight"
        , ylab = "Weight"
        , col = "red")

# Notched Boxplot
boxplot(castings$weight
        , main = "Boxplot of Casting Weight"
        , ylab = "Weight"
        , col = "red"
        , notch = T)

# Create Multiple Boxplots ------------------------------------------------
# Import castings file

boxplot(weight ~ mold
        , data = castings3
        , main="Boxplot of Casting Weight by Mold"
        , ylab="Weight"
        , col = c("red","blue","green"))

# Create a Scatterplot ----------------------------------------------------
# Import rivet file

plot(x = Rivet$paint
   , y = Rivet$rivet
   , xlab = "Paint Thickness"
   , ylab = "Rivet Height"
   , pch = 19
   , cex = 0.8
   , main = "Paint Thickness vs Rivet Height")

# Create line of best fit
abline(lm(Rivet$rivet~Rivet$paint)
       ,col="blue"
       ,lwd=2)
