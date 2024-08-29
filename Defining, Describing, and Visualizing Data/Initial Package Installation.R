
# Initial Package Installation --------------------------------------------

# Please install the following packages that
# we will be using throughout our course.

# The installations only need to performed ONCE,
# and updated (recommended - once a month)

# Install devtools package ------------------------------------------------
install.packages("devtools", dependencies = TRUE)

# Install the lolcat statistical package ----------------------------------
require(devtools)
install_github("burrm/lolcat")

# Install these additional packages ---------------------------------------
install.packages("car", dependencies = TRUE)
install.packages("ggplot2", dependencies=TRUE)
install.packages("plyr", dependencies = TRUE)
install.packages("dplyr", dependencies = TRUE)
install.packages("vcd", dependencies = TRUE)
install.packages("gmodels", dependencies = TRUE)
install.packages("knitr", dependencies = TRUE)
install.packages("SuppDists", dependencies = TRUE)
install.packages("fitdistrplus", dependencies = TRUE)
install.packages("matrixStats", dependencies = TRUE)
install.packages("flextable", dependencies = T)

# Activate all of the following libraries each time you run an R s --------
# suppressPackageStartupMessages()
library(lolcat)
library(car)
library(ggplot2)
library(plyr)
library(dplyr)
suppressPackageStartupMessages(library(vcd))
library(readr)

ro<-round.object
nqtr<-function(x,d){noquote(t(round.object(x, d)))}
options(scipen=999)

# options(tibble.print_max = Inf)    # Allows displaying a whole data frame vs 10 rows
# options(digits = 9)                # 7 is the defaulty if not set otherwise with this command


# Load Multiple Packages at Once ------------------------------------------
Packages <- c("lolcat","car","plyr","dplyr", "ggplot2", "vcd", "readr","gmodels")
lapply(X = Packages, FUN = library, character.only = TRUE)

# lapply is applying a function to every item on the list


# General functions - Always run on start up ------------------------------
ro <- round.object  # Easy version of Rounding Objects
nqtr <- function(x,d){noquote(t(round.object(x, d)))}
options(scipen=999) # No scientific notation

#####  Run all of the above each time you begin an R session and you will be set.


# Unload a package --------------------------------------------------------
# To unload a package use the following. The package name follows the 'package:' parameter

detach("package:plyr", unload=TRUE)

# Or you can uncheck the check box next the package 
# in the Packages window (lower right panel)


# Example 1 -----------------------------------------------------------------
x<-15;x
y<-c(1,2,3,4,5)


# Copy and Paste for Windows and for MAC -----------------------------------
df<-read.table("clipboard", sep="\t", header = TRUE)# For Windows
df<-read.table(pipe("pbpaste"), sep="\t", header=TRUE)  # For MAC
View(df) # View the file


# Example 2 ---------------------------------------------------------------
mean(Sample$Weight)



# Useful functions --------------------------------------------------------
length(Sample$Factor) # number of elements or components
str(Sample)    # structure of an object
class(Sample)  # class or type of an object
names(Sample)  # names

# Combine -----------------------------------------------------------------
# c(object,object,...)       # combine objects into a vector
(Abhishek<-c("Cool", "Leader", "Awesome"))
Mallory<-c(1,2,3)

(output<-data.frame(cbind(Abhishek,Mallory)))

cbind(object, object, ...) # combine objects as columns
rbind(object, object, ...) # combine objects as rows

# Print -------------------------------------------------------------------
Sample     # prints the object
output

# List Objects ------------------------------------------------------------
ls()       # list current objects


# Remove Objects ----------------------------------------------------------
rm(df) # delete an object


# Edit --------------------------------------------------------------------
newsample<- edit(Sample)  # edit copy and save as newobject
fix(Sample)               # edit in place

# Variable Labels ---------------------------------------------------------
names(Sample)[3] <- "Level"
colnames(Sample)<-c("Samp", "Date", "Level", "Wt","Ht")
rownames( )

# Value Labels ------------------------------------------------------------
# Reimport the Sample.txt file
str(Sample)
Sample$Factor<- factor(Sample$Factor
                      ,levels = c(1,2,3)
                      ,labels = c("Low", "Medium", "High"))

# Missing Values ----------------------------------------------------------
is.na(Sample) 		# returns TRUE when value missing
is.na(Sample$Height)

# Excluding Missing Values from Analyses ----------------------------------
# Arithmetic functions on missing values yield missing values.
mean(Sample$Height)
mean(Sample$Height, na.rm=T) # use na.rm=T

# Date Values -----------------------------------------------------------
# Change Character to Date
# use as.Date( ) to convert strings to dates
Sample$Time<-as.Date(Sample$Time)
str(Sample)
