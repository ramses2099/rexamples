## 
## setup:
## ---------------------------------
if(!is.null(dev.list())) dev.off()
cat("\014") 
rm(list=ls())

#Set work directory
setwd("C:/Users/yunqi/OneDrive - Conestoga College/Course_teach/DataScience8435_24F/Week_14")

# library:
if(!require(polycor)){install.packages("polycor")}
library(polycor)

if(!require(lattice)){install.packages("lattice")}
library("lattice")

if(!require(pastecs)){install.packages("pastecs")}
library("pastecs")

if(!require(corrgram)){install.packages("corrgram")}
library("corrgram")

options(scipen=9)


## load training data:
## ---------------------------------
BD_tr <- read.csv("BD_train.csv", header = TRUE, sep = ",")
head(BD_tr,8) 
names(BD_tr) <- c("Rec", "Nbr", "Tim", "Gender", "Donate")
str(BD_tr)
BD_tr <- as.data.frame(unclass(BD_tr), stringsAsFactors = TRUE)
summary(BD_tr)
stat.desc(BD_tr)

## train Logistic Regression model:
BD_logreg = glm(Donate ~ . ,
              family="binomial", data=BD_tr, na.action=na.omit)

summary(BD_logreg)


## use model to predict on test data:
## ---------------------------------
# load test data and preprocess:
BD_ts <- read.csv("BD_test.csv", header = TRUE, sep = ",")
head(BD_ts,8) 

# preprocess - with same way as dealing with training data:
names(BD_ts) <- c("Rec", "Nbr", "Tim", "Gender")
BD_ts <- as.data.frame(unclass(BD_ts), stringsAsFactors = TRUE)
summary(BD_ts)


# predict:
P_pred <- predict(BD_logreg, newdata=BD_ts, type="response")   # creates probabilities
round(head(P_pred,20),2)

Y_pred <- ifelse(P_pred > 0.5,"Y","N")   # Classifies prob (i.e. >50% then likely to donate)
head(Y_pred,20)

# write prediction result to a file (for submission):
BD_ts_submission <- cbind(BD_ts, Y_pred)
write.csv(BD_ts_submission, "Prog8435-24F-final_yqmiao.txt")







