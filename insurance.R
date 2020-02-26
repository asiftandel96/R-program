# to check null values
# check for the outliers in dependent variable
# take a subset of non-outlier observation
# remove not required variables
# 1. divide the data in training and testing 
# 2. run corrected model on training data set 
# 3. calculate RMSE for training data set 
# 4. predict results on testing data set 
# 5. calculate residules on testing dataset 
# 6. calculate RMSE on testing dataset 
# 7. check the difference of RMSE for training and testing;it must not exceed by 15%
# insurance data 
insurance<-read.csv("C:/Users/asif/Downloads/insurance (1).csv",header = T)
dim(insurance)
str(insurance)

install.packages("e1071")
library(e1071)
skewness(insurance$charges)
str(insurance)
insurance$X<-NULL
summary(insurance$children)


#ifelse 0:28: cat1 29:45 cat2 above 45 cat3
insurance$age1<-ifelse(insurance$age<28,"cat1",ifelse(insurance$age<45,"cat2","cat3"))

# using cut function 
insurance$age2<-cut(insurance$age,breaks = c(0,28,45,Inf),labels =c("cat1","cat2","cat3") )



# cross validation 
# hold out cross validation 

# 1. divide the data in training and testing 
# 2. run corrected model on training data set 
# 3. calculate RMSE for training data set 
# 4. predict results on testing data set 
# 5. calculate residules on testing dataset 
# 6. calculate RMSE on testing dataset 
# 7. check the difference of RMSE for training and testing;it must not exceed by 15%


index<-sample(nrow(insurance),0.75*nrow(insurance))
train<-insurance[index,]
test<-insurance[-index,]
# RUN MODEL ON TRAINING DATA 

insurance_model<-lm(charges~.,data = train)
train$pred_charges<-predict(insurance_model)
train$res<-residuals(insurance_model)

# RMSE FOR TRAIN DATA 
train_rmse<-sqrt(mean(train$res^2))
train_rmse

#TESTING DATA 
test$pred_charges<-predict(insurance_model,test)
test$res<-test$charges - test$pred_charges
test_rmse<-sqrt(mean(test$res^2))
test_rmse


# K FOLD CROSS VALIDATION 
library(caret)
library(lattice)
library(ggplot2)



library(caret)
folds<-trainControl(method="cv",number=4)
model<- train(charges~.,data=insurance,method="lm", trControl=folds)
model
full_model<-lm(charges~.,data=insurance)
pred<-predict(full_model)
RMSE(pred,insurance$charges)
RMSE(full_model$fitted.values,insurance$charges)


# REPEATED K FOLD CV
kfolds<-trainControl(method="repeatedcv",number=4,repeats=5) 
model<- train(charges~.,data=insurance,method="lm",trControl=kfolds)
model
#LOOCV : LEAVE ONE OUT CROSS VALIDATION 
library(caret)
kfolds<-trainControl(method="LOOCV")
model<- train(charges~.,data=insurance,method="lm",  trControl=kfolds)
model