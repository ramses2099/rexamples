##################################################
### PROG8435                                    ##
### Data Reduction Demonstration                ##
### Demonstration                               ##  
##################################################
#                                               ##
##################################################
#
##################################################
### Basic Set Up                                ##
##################################################

# Clear plots
if(!is.null(dev.list())) dev.off()

# Clear console
cat("\014") 

# Clean workspace
rm(list=ls())

#Set work directory
setwd("C:/Users/yunqi/OneDrive - Conestoga College/Course_teach/DataScience8435_24F/Week_4")

##################################################
### Remove Packages Installed                   ##
##################################################

##################################################
### Install Libraries                           ##
##################################################

#If the library is not already downloaded, download it

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

if(!require(lattice)){install.packages("lattice")}
library("lattice")

##############################
## Read in Data             ##
##############################

options(digits=5)

Master <- read.csv("PROG8435_Dimension_Data_Demo.csv", header = TRUE, sep = ",")

head(Master)

##############################
## Reduce Number of Vars    ##
##############################

# Find missing values
# Identify cols > 99% missing

summary(Master)

#Look like X02 is likely!

Master <- Master[-c(3)]

head(Master,8)

# Identify Low Variance

stat.desc(Master)  #Consider coef of var
summary(Master)

#Based on the above X04 seem likely. Let's check!

table(Master$X04)

Master <- Master[-c(4)]

head(Master,7)

#Identify High Correlation

cor(Master,method="spearman")

#X05 and X06 seem highly correlated
#Don't need to keep both, I'll drop the second one.

Master <- Master[-c(5)]

head(Master,3)
