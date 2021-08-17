# ----------------------------------------- THE FINALE -------------------------------------------- #

# Setting the directory .............
Path <- "C:/Users/Abir/Desktop/IVY- Data Analysis/R Programming/Materials/Final Project/Final R Project_IVY/Final R Project_IVY"
setwd(Path)
getwd()

# Calling the required packages ................

library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
library(caTools)
library(dplyr)
library(ggplot2)

# Reading the file ..................
data <- read.csv("Fn-UseC_-Marketing-Customer-Value-Analysis.csv", stringsAsFactors = TRUE)
str(data)

data1 <- data # creating a backup
data1$Number.of.Open.Complaints <- as.factor(data1$Number.of.Open.Complaints)
data1$Number.of.Policies <- as.factor(data1$Number.of.Policies)

str(data1)

library(pastecs)
options(scipen = 100)
# Basic Exploration .........
summary(data1)
Skew(data1$Customer.Lifetime.Value) # Skew is +ve
Kurt(data$Customer.Lifetime.Value) # Kurt > 3
hist(data1$Customer.Lifetime.Value, col = "Red",main = "CLV Histogram", xlab = "CLV")
# The summary indicates presence of outliers that needs to be treated.

# Descriptive analysis of Significant Variables
#Coverage
#Education ( only College, Doctor and Master)
#Employment Status ( Employed, Unemployed)
#Income
#Marital Status ( Single)
#Monthly Premium Auto
#Number of Open Complaints
#Number of Policies
#Vehicle Class ( Sports Car, SUV)

# Analysis of Monthly Premium auto
summary(data1$Monthly.Premium.Auto)
Skew(data1$Monthly.Premium.Auto)
Kurt(data1$Monthly.Premium.Auto)
hist(data1$Monthly.Premium.Auto, col = "Light Blue",main = "Monthly.Premium.Auto Histogram",xlab= "Monthly Premium Auto")
plot(x=data1$Monthly.Premium.Auto, y=data1$Customer.Lifetime.Value, col="#00AFBB", cex=1, xlab="MonthlyPremiumAuto", ylab="CustomerLifetimeValue",
     main="Scatterplot of MPA vs CLV")

# INcome
cor(data1$Income,data1$Customer.Lifetime.Value)
plot(x=data1$Income, y=data1$Customer.Lifetime.Value,
     col="#FC4E07", cex=1, xlab="Income", ylab="CustomerLifetimeValue",main="Scatterplot of Income vs CLV")

#No. of Opem Complaints
cor(data$Number.of.Open.Complaints,data$Customer.Lifetime.Value)
plot(x=data$Number.of.Open.Complaints, y=data$Customer.Lifetime.Value, col="#FC4E07", cex=1,
     xlab="NumberofOpenComplaints", ylab="CustomerLifetimeValue",main="Scatterplot of NumberofOpenComplaints vs CLV")

#Number of policies
cor(data$Number.of.Policies,data$Customer.Lifetime.Value)
plot(x=data$Number.of.Policies, y=data$Customer.Lifetime.Value, col="#FC4E07", cex=1,
     xlab="NumberofPolicies", ylab="CustomerLifetimeValue",main="Scatterplot of NumberofPolicies vs CLV")

#Coverage
ggplot(data, aes(x=Coverage, y= Customer.Lifetime.Value, fill = Coverage)) + 
  geom_boxplot() + 
  labs(x="Coverage",y = "Customer Life Time Value", fill="Coverage") + 
  ggtitle("Visualization of CLV wrt Coverage")


colnames(data1)[which(names(data)=="Customer.Lifetime.Value")]="clv"
str(data1)

data.frame(colSums(is.na(data1)))# It has no missing values.
#___________________________________________________________________________________________________________

# Outlier treatment through Quantile Method ..........
nrow(data1)
boxplot(data1$clv)
quantile(data1$clv,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))
data2=data1[data1$clv <30000,]#creating a new data frame with a cap on outliers
boxplot(data2$clv)
nrow(data2)

quantile(data2$clv,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))
data3 <- data2[data2$clv<18943,]
boxplot(data3$clv)
nrow(data3)
quantile(data3$clv,c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.8,0.85,0.9,0.95,0.99,0.995,1))
data4 <- data3[data3$clv<14212,]
boxplot(data4$clv)
# ----------------------------- End of Outlier Treatment --------------------------------------

data.final <- select(data4,-c(Customer,State,Effective.To.Date))
str(data4)#final dataset for modelling

# ----------------------------- SPLITTING THE DATA INTO TRAIN AND TEST DATASET -------------------------------------

set.seed(200)#This is used to produce reproducible results, everytime we run the model
spl = sample.split(data.final$clv, 0.7)#Splits the overall data into train and test data in 70:30 ratio

train.data = subset(data.final, spl == TRUE)
str(train.data)
dim(train.data)

test.data = subset(data.final, spl == FALSE)
str(test.data)
dim(test.data)

#------------------------------------------Fitting the model---------------------------------------#

#Iteration.1 We start with testing all variables

LinearModel_0=lm(clv~.,data=train.data)
summary(LinearModel_0)

#Iteration.2.
LinearModel_1 <-lm(clv ~Response+ Coverage+ Education+ EmploymentStatus+ Gender+ Income+ Marital.Status
                   + Monthly.Premium.Auto+ Months.Since.Last.Claim+ Number.of.Open.Complaints+ Number.of.Policies
                   + Policy.Type+ Policy+ Renew.Offer.Type+ Sales.Channel+ Vehicle.Class, data = train.data)
summary(LinearModel_1)

#Iteration.3.
LinearModel_2 <- lm(clv ~Coverage+ Education+ I(EmploymentStatus=="Employed")+ I(EmploymentStatus=="Unemployed")
                    + Gender+ Income+ Marital.Status+ Monthly.Premium.Auto+ Number.of.Open.Complaints+ Number.of.Policies
                    + I(Policy=="Corporate L3")+ I(Renew.Offer.Type=="Offer2" | Renew.Offer.Type=="Offer4")
                    + I(Vehicle.Class=="Luxury SUV")+I(Vehicle.Class=="Sports Car")+ I(Vehicle.Class=="SUV"), data = train.data)
summary(LinearModel_2)
##---------------------------------------------------Final Model
#Iteration.4.
FinalModel <- lm(clv ~Coverage+ Education+ I(EmploymentStatus=="Employed")+ I(EmploymentStatus=="Unemployed")
                 + Gender+ Income+ Marital.Status+ Monthly.Premium.Auto+ Number.of.Open.Complaints+ Number.of.Policies
                 + I(Renew.Offer.Type=="Offer2" | Renew.Offer.Type=="Offer4")+ I(Vehicle.Class=="Luxury SUV")
                 + I(Vehicle.Class=="Sports Car")+ I(Vehicle.Class=="SUV"), data = train.data)
summary(FinalModel)
#____________________________________________________________________________________________________________________________

# ------------------------------- Conducting Different Tests in the model ----------------------------

#Checking Multicollinearity in the model
vif(FinalModel)
# Multicollinearity doesnt exist in this model.

# Breusch-Pagan test. Finding out if our model is heteroscedastic.
# Null Hypothesis is that residuals are homoscedastic. A p-value< 0.05 would mean otherwise.
bptest(FinalModel)
# WE find that the p-value is < 0.05. Thus we reject the null hypothesis and conclude that the errors are heteroscedastic.

# Finding out if the model has Auto-correlation. To do so, we perform the Durbin-Watson Test.
# We consider  the Null Hyp, Ho : The Data is not Serially correlated. If the p- value < 0.05, we reject the null hypothesis.
dwt(FinalModel)
# We fail to reject the null hypothesis. There is no serial correlation in the model.

## Normality testing Null hypothesis is data is normal.

resids <- FinalModel$residuals

ad.test(resids) #get Anderson-Darling test for normality 
pearson.test(resids) #get Pearson chi-square test for normaility 
sf.test(resids[1:5000]) #get Shapiro-Francia test for normaility 

#In all the above tests we see that the p-value < 0.05. Thus, we reject the null hypothesis that the data is normal. 
qqnorm(resids)
ggdensity(resids)

## Get the predicted or fitted values
fitted(FinalModel)

plot(FinalModel)

## MAPE
train.data$pred <- fitted(FinalModel)
write.csv(train.data,"Final_mape.csv")

#Calculating MAPE
attach(train.data)
MAPE<-print((sum((abs(clv-pred))/clv))/nrow(train.data))
# MAPE is good 10%

#___________________________________________________________________________________________________________________________

# --------------------------------------- TESTING THE MODEL ON TEST DATA ---------------------------------------------

##Final model 
fit1<- lm(clv ~Coverage+ Education+ I(EmploymentStatus=="Employed")+ I(EmploymentStatus=="Unemployed")
          + Gender+ Income+ Marital.Status+ Monthly.Premium.Auto+ Number.of.Open.Complaints+ Number.of.Policies
          + I(Renew.Offer.Type=="Offer2" | Renew.Offer.Type=="Offer4")+ I(Vehicle.Class=="Luxury SUV")
          + I(Vehicle.Class=="Sports Car")+ I(Vehicle.Class=="SUV"), data = test.data)
summary(fit1)

#Final Test Model
fit2 <- lm(clv ~Coverage+ I(Education=="College")+ I(Education=="Doctor")+ I(Education=="Master")+ I(EmploymentStatus=="Employed")+ I(EmploymentStatus=="Unemployed")
           + Income+ I(Marital.Status=="Single")+ Monthly.Premium.Auto+ I(Number.of.Open.Complaints== 3)
           + I(Number.of.Open.Complaints== 4)+ I(Number.of.Open.Complaints== 5)+ Number.of.Policies
           + I(Vehicle.Class=="Sports Car")+ I(Vehicle.Class=="SUV"), data = test.data)
summary(fit2)

# Thus We've found our test model.

# ------------------------------- Conducting Different Tests in the model ----------------------------

#Checking Multicollinearity in the model
vif(fit2)
# Multicollinearity doesnt exist in this model.

# Breusch-Pagan test. Finding out if our model is heteroscedastic.
# Null Hypothesis is that residuals are homoscedastic. A p-value< 0.05 would mean otherwise.
bptest(fit2)
# WE find that the p-value is < 0.05. Thus we reject the null hypothesis and conclude that the errors are heteroscedastic.

# Finding out if the model has Auto-correlation. To do so, we perform the Durbin-Watson Test.
# We consider  the Null Hyp, Ho : The Data is not Serially correlated. If the p- value < 0.05, we reject the null hypothesis.
dwt(fit2)
# We fail to reject the null hypothesis. There is no serial correlation in the model.

## Normality testing Null hypothesis is data is normal.

test_resids <- fit2$residuals

ad.test(test_resids) #get Anderson-Darling test for normality 
pearson.test(test_resids) #get Pearson chi-square test for normaility 
sf.test(test_resids) #get Shapiro-Francia test for normaility 

#In all the above tests we see that the p-value < 0.05. Thus, we reject the null hypothesis that the data is normal. 

library(ggpubr)
qqnorm(test_resids)
ggdensity(test_resids)

## Get the predicted or fitted values
fitted(fit2)
test.data$pred <- fitted(fit2)
plot(fit2)

#Calculating MAPE
attach(test.data)
MAPE.test<-print((sum((abs(clv-pred))/clv))/nrow(test.data))
# MAPE is good 10%

write.csv(test.data,"Test_mape.csv")
# ____________________________________________________ FIN _______________________________________________________________