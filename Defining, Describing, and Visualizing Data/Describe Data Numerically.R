# Startup Code ------------------------------------------------------------
require(lolcat)
require(dplyr)
require(flextable)
ro <- round.object # Easy version of Rounding Objects
nqtr <- function(x,d){noquote(t(round.object(x, d)))}
options(scipen=999)

# Create data file --------------------------------------------------------
weight<-c(65,67,36,37,36,57,53,39,38,58)
preform<-data.frame(weight)
View(preform)

# Calculate the Mean ------------------------------------------------------
mean(preform$weight)
summary.continuous(preform$weight)
nqtr(summary.continuous(preform$weight),4)

# Calculate the Median ----------------------------------------------------
median(preform$weight)

# Calculate the Mode ------------------------------------------------------
table(preform$weight)
sample.mode(preform$weight)

# Calculate Central Tendency ----------------------------------------------
# Using summary.continuous
nqtr(summary.continuous(preform$weight
                        , stat.median=T),4)

# Using summary.impl
summary.impl(preform$weight
             , stat.mean=T
             , stat.median = T)

# Calculate the Range -----------------------------------------------------
# Find the range of values in the data set
range(preform$weight)
rng<-range(preform$weight)
rng[2]-rng[1]

# Calculate the Standard Deviation ----------------------------------------
sd(preform$weight)
round.object(sd(preform$weight),2)

# Calculate the Variance --------------------------------------------------
var(preform$weight)
round.object(var(preform$weight),2)

# Calculate Dispersion ----------------------------------------------------
nqtr(summary.continuous(preform$weight
                        , stat.sd=T
                        , stat.range=T),4)

# Using summary.impl
summary.impl(preform$weight
             , stat.var = T
             , stat.sd = T
             , stat.range = T)

# Calculate Skewness ------------------------------------------------------
summary.continuous(castings$weight)
hist.grouped(castings$weight)
lolcat::skewness(castings$weight)

# Calculate Kurtosis ------------------------------------------------------
summary.continuous(castings$weight)
hist.grouped(castings$weight)
lolcat::kurtosis(castings$weight)

# Calculate Correlation ---------------------------------------------------
# Import Rivet file

# Calculate Pearson Product-Moment Correlation Coefficient
cor(Rivet)
cor(x = Rivet$paint, y = Rivet$rivet)

# Create a Summary Statistics Table ---------------------------------------
# Import Cap file

(data.out<-summary.impl(Cap, stat.n = T
                        , stat.sd = T
                        , stat.var = T
                        , stat.mean = T))
nqtr(summary.continuous(Cap, stat.sd=T),4)

data.out %>%
  flextable() %>%
  add_header_lines(values = "Mean and Median") %>%
  theme_box()

