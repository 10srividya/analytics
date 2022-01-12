library(readxl) #Loading the package to read excel files


cust_bank_data <- read_excel("C:\\Users/srividya/Downloads/customer_bank_data.xlsx")
summary(cust_bank_data)

str(cust_bank_data) # To check the data type of the columns, we could use the class() function as well

class(cust_bank_data$EDUCATION)

# We observe that education, defaulted are classified as "character" they should be nominal

cust_bank_data$EDUCATION <- as.factor(cust_bank_data$EDUCATION)

#Similarly for defaulted to be changed into flag, we use factors and exclude the $null$ value
cust_bank_data$DEFAULTED <- factor(cust_bank_data$DEFAULTED, levels=c("1","0"), labels=levels, exclude ="$null$")

#Load ggplot2  package to use ggplot function to plot graphs, etc
library(ggplot2) 

#Plot the distribution of defaulted with education as the color overlay 
ggplot(data=cust_bank_data, aes(x=DEFAULTED,fill=EDUCATION)) +
         geom_bar(color="black", position="stack")


#Education Reclassified into new values 1,2,3,4,5 with type as nominal(factor)
library(car)
library(dplyr)

cust_bank_data$Education_reclassified <- recode(cust_bank_data$EDUCATION, "SCHOOL"="1","UNDER GRADUATE" = "2","POST GRADUATE" = "3","DOCTRATE" ="4","POST DOCTORAL RESEARCH" ="5")

#View the statistics of the variables

summary(cust_bank_data)

# View the count, variance, std dev, std error of mean and range
# for age, years employed, income, cardebt,otherdebt

sd(cust_bank_data$INCOME) #standard deviation
sd(cust_bank_data$YEARSEMPLOYED)
sd(cust_bank_data$CARDDEBT)
sd(cust_bank_data$OTHERDEBT)
sd(cust_bank_data$AGE )

var(cust_bank_data$INCOME) #variance

library(psych)
describe(cust_bank_data) # for the whole table, variance mean sd min, max range skew kurtosis, standard error are displayed

#Calculate Debt Income Ratio
cust_bank_data$DEBT_INCOME_RATIO <- ((cust_bank_data$CARDDEBT+cust_bank_data$OTHERDEBT)/cust_bank_data$INCOME)*100

help(kmeans)

k2 <- kmeans(cust_bank_data_kmeans,iter.max=20,4)

install.packages("factoextra")
library(factoextra)
install.packages("cluster")
library(cluster)
install.packages("fviz_cluster")
library(tidyverse)
install.packages("gridExtra")
library(gridExtra)
cust_bank_data_kmeans <- data.frame(cust_bank_data$AGE,cust_bank_data$DEBT_INCOME_RATIO,cust_bank_data$YEARSEMPLOYED)


kmeans2 <- kmeans(cust_bank_data_kmeans, centers = 4, nstart = 25)
str(kmeans2)
kmeans2$size
kmeans2$centers
kmeans2$totss
kmeans2$withinss
kmeans2$iter
kmeans2$betweenss

fviz_cluster(kmeans2, data = cust_bank_data_kmeans) #to visualize the clusters
kmean_withinss <- function(k) {
  cluster <- kmeans(c(cust_bank_data$AGE,cust_bank_data$DEBT_INCOME_RATIO,cust_bank_data$YEARSEMPLOYED), 2)
  return (cluster$tot.withinss) 
}

