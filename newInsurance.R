insurance_premium <- read.csv("C:/Users/asif/Downloads/insurance.csv")
summary(insurance_premium)
str(insurance_premium)
colSums(is.na(insurance_premium))
IQR(insurance_premium$charges)*1.5+16640
boxplot(insurance_premium$charges)
insurancepremium<-subset(insurance_premium,insurance_premium$charges<=16640)
boxplot(insurancepremium$charges)
str(insurancepremium)
library(caret)
library(lattice)
library(ggplot2)
library(caret)
library(car)
library(carData)
library(e1071)
skewness(insurancepremium$charges)
set.seed(123)
index<-createDataPartition(insurancepremium$charges,p=0.7,list = F)
train_insurance<-insurancepremium[index,]


test_insurance<-insurancepremium[-index,]
head(train_insurance)
head(test_insurance)
length(index)
head(index,20)

set.seed(123)
index1<-sample(nrow(insurancepremium),0.75*nrow(insurancepremium))
nrow(insurancepremium)
index1<-sample(1003,0.75*1003)
train_insurance_sample<-insurancepremium[index1,]
test_insurance_sample<-insurancepremium[-index1,]




# model MUST BE built on training data : RMSE_training 
# validate model on testing data : RMSE_training
# difference of RMSE_train and RMSE_test should not exceed by 15%

insurance_model<-lm(charges~.,data=train_insurance)
train_insurance_new<-train_insurance
train_house_new$sqft_living<-NULL
insurance_model_new<-lm(charges~.,data=train_insurance_new)
vif(insurance_model_new)
train_insurance_new$pred_price<-predict(insurance_model_new)





summary(insurance_model)

summary(train_insurance$charges)

insurance_model2<-lm(charges~.-(bmi),data = train_insurance)
summary(insurance_model2)

# predict prices on training data and calculate residuals
train_insurance$pred_charges<-fitted(insurance_model2) # prediction of price
train_insurance$res<-residuals(insurance_model2) # y-yhat

# calculate Root Mean Square Error
rmsetrain<-sqrt(mean(train_insurance$res**2))

rmsetrain

test_insurance$pred_charges<-predict(insurance_model2,test_insurance)
head(test_insurance)

test_insurance$res<-test_insurance$charges - test_insurance$pred_charges
rmsetest<-sqrt(mean(test_insurance$res^2))
length(insurance_model2$residuals)
summary(insurance_model2)
rmsetrain 
rmsetest  

library(car)
library(carData)
vif(insurance_model2)
 
