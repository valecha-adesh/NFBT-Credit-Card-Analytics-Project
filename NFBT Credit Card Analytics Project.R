
library(caret)
library(e1071)
library(kernlab)
library(dplyr)
library(arm)
library(robustbase)
library(chemometrics)

data<-read.csv(url("http://data.mishra.us/files/project-2.csv"))
View(data)
str(data)
summary(data)

#Correlation
colnames(data)
correlationMatrix <- cor(data[,c(2,3,4,5)])
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.7)
print(highlyCorrelated)
#There is no correlation among variables

#Running Logistic regression on whole dataset
summary(model<-train(offer_success~.,
                     data=data,
                     method="glm",
                     family="binomial"))


coef(model$finalModel)

#From the model summary we can say the following :
#variable credit_score is not significant as p value is greater than 0.05.
#Variables amount_spent and years are highly significant.
#variable transaction_frequency is significant and has negative coefficient that decreases the probability of offer_success.


#In terms of probability 
options(scipen=999)
invlogit(coef(model$finalModel))


#outlier detection

#Removing outcome variable (offer_success) from the dataset
data_new <- data[-1] 

#Calculating the Minimum Covariance Determinant(MCD)
x.mcd=covMcd(data_new, alpha=.5)  

#Estimator via the Fast MCD. Here alpha determines the values of h.
#Roughly h= sample size*alpha

#Prints robust mahalanobis distance
x.mcd$mah 

# prints ''outlyingness'' of observations. here 0 means outlier and 1 inlier
x.mcd$mcd.wt 

#Counting number of outliers
sum(x.mcd$mcd.wt == 0) 

#Counting number of Inliers
sum(x.mcd$mcd.wt == 1) 

#Creating a new data set where 0, 1 outlier scores are in the first column 
new<-data.frame(x.mcd$mcd.wt,data) 
new

#Creating a new dataset which excludes the outliers i.e. it is the clean data set.
data_Inlier <- new[-which(x.mcd$mcd.wt == 0), ] 

#Create a new dataset which is the outliers i.e. it is the outlier data set.
data_Outlier <- new[-which(x.mcd$mcd.wt == 1), ] 


summary(data_Inlier)
summary(data_Outlier)

#Running logistic regression on the new dataset - Inlier
summary(model<-train(offer_success~.,
                     data=data_Inlier,
                     method="glm",
                     family="binomial"))



#Inlier Data Analysis
#Variables amount_spent and years are significant and have positive impact on offer_success.
#Variables credit_score and transacion_frequency have p values greater than 0.05 and are not significant.


#Running logistic regression on the new dataset - Outlier
summary(model<-train(offer_success~.,
                     data=data_Outlier,
                     method="glm",
                     family="binomial"))

#Outlier Data Analysis
#All variables are significant.
#Variables amount_spent and years have positive impact on offer_success.
#Variables transaction_frequency and credit_score have negative impact on offer_success.

