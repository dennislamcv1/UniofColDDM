# Startup Code
require(lolcat)
ro<-round.object
nqtr<-function(x,d){noquote(t(round.object(x, d)))}
options(scipen=999, digits=9)

# Generating Random Numbers and Random Sequences --------------------------

# Generate 10 Random Numbers between 1 and 10 using 
# a Uniform distribution
runif(n = 10, min = 1, max = 10)

# Generate 10 Random Numbers using the code above
# and round to the largest integer
floor(runif(n = 10, min = 1, max = 10))

# Create a random sequence using numbers 1-10
# Create a sequence
x<-seq(from = 1, to = 10, by = 1)

# Sample from the sequence without replacement
sample(x = x, size = 10, replace = FALSE)

# Use the 'random' package
install.packages('random')
require(random)
randomSequence(min = 1,max = 10,col = 2)
