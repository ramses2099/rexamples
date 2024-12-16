  ##################################################
  ##
  ### Logistic Demonstration                      ## 
  #                                               ##
  ##################################################
  # Written by 
  # ID: 
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
  setwd("C:/Users/yunqi/OneDrive - Conestoga College/Course_teach/DataScience8435_24F/Week_11")
  
  options(scipen=9)
  
  ##################################################
  ### Install Libraries                           ##
  ##################################################
  
  #If the library is not already downloaded, download it
  
  if(!require(polycor)){install.packages("polycor")}
  library(polycor)
  
  ##################################################
  ### Read data and do preliminary data checks    ##
  ##################################################
  
  # Read "comma separated value" files (".csv")
  # Blood Donation data set
  Blood <- read.csv("BloodDonate.csv", header = TRUE, sep = ",")
 
  head(Blood,8)  #Print a Few Observations to Verify
  
  #Rename for easier interpretation
  
  names(Blood) <- c("Rec", "Nbr", "Vol", "Tim", "Gender", "Donate")
  
  str(Blood)
  
  Blood <- as.data.frame(unclass(Blood), stringsAsFactors = TRUE)
  
  summary(Blood)

  ##################################################
  ### Correlations                                ##
  ##################################################
  
  pairs(Blood[sapply(Blood,is.numeric)], pch=46)
  
  ht <- hetcor(Blood)  #from polycor library
  round(ht$correlations,2)
  
  str(Blood)
  
  Blood$Donate <- as.factor(Blood$Donate)
  
  ##################################################
  ### Building the Model                          ##
  ##################################################
  
  # Full Model
  
  Don_glm = glm(Donate ~ . ,
                family="binomial", data=Blood, na.action=na.omit)
  
  
  # Stepwise Model
  # stp_Don_glm <- step(Don_glm)
  summary(Don_glm)
  # summary(stp_Don_glm)
  # plot(stp_Don_glm,which=4, id.n=6)
  # r <- residuals(stp_Don_glm)
  
  # head(r)
  
  # plot(r)
  
  ## Model Prediction:
  P_pred <- predict(Don_glm, newdata=Blood, type="response")   # creates probabilities
  round(head(P_pred,20),2)
  
  Y_pred <- ifelse(P_pred > 0.5,"Y","N")   # Classifies probablities (i.e. >50% then likely to donate)
  head(Y_pred,20)
  
  # Creates a Confusion Matrix
  ConfMatrix <- table(Blood$Donate, Y_pred,
                dnn=list("Act Donate","Predicted") ) 
  ConfMatrix
  
  Accuracy <- (ConfMatrix[2,2] + ConfMatrix[1,1])/sum(ConfMatrix)
  round(Accuracy, 4)
  
  
  
  
  
  
  #-------------------------------------------
  summary(Don_glm)  
  
  Vol_glm = glm(Donate ~ Rec + Vol + Tim + Gender,
                family="binomial", data=Blood, na.action=na.omit)
  summary(Vol_glm)
  
  Nbr_glm = glm(Donate ~ Rec + Nbr + Tim + Gender,
                family="binomial", data=Blood, na.action=na.omit)
  summary(Nbr_glm)
  
  
  resp_SW <- predict(Nbr_glm, newdata=Blood, type="response")   # creates probabilities
  round(head(resp_SW,20),2)
  Class_SW <- ifelse(resp_SW > 0.5,"Y","N")           # Classifies probablities (i.e. >50% then likely to donate)
  head(Class_SW,20)
  Conf <- table(Blood$Donate, Class_SW,
                dnn=list("Act Donate","Predicted") )  # Creates a Confusion Matrix
  Conf
  Acc_stp <- (Conf[2,2] + Conf[1,1])/sum(Conf)
  round(Acc_stp,3)
  
  plot(Nbr_glm,which=4, id.n=6)
  
  r <- residuals(Nbr_glm)
  
  head(r)
  