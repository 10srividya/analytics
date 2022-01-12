library(rattle)
rattle()
churn <- read.csv("C:\\Users/srividya/Downloads/churn.txt")

churn$IntlP.ind <- ifelse(churn$Int.l.Plan=="yes",1,0)
churn$VMP.ind <- ifelse(churn$VMail.Plan=="yes",1,0)
churn$CSC <- factor(churn$CustServ.Calls)

levels(churn$CSC)[1:2] <- "Low"

levels(churn$CSC)[2:3] <- "Medium"
churn$CSC_Hi <- ifelse(churn$CSC=="High",1,0)

install.packages("pROC")
library(pROC)

