#Importing the dataset
insurance_example<-read.csv("C:/Users/asif/Downloads/insurance (1).csv",header=T)
head(insurance_example)
#1 to check null values
colSums(is.na(insurance_example))
#2 check for the outliers in dependent variable
boxplot(insurance_example$charges)
IQR(insurance_example$charges)
summary(insurance_example)
#3take a subset of non-outlier observation
insurance_outremovable<-subset(insurance_example,insurance_example$charges<=16640)
boxplot(insurance_outremovable$charges)
str(insurance_example)
summary(insurance_example)
#4 divide the data in training and testing 
index<-sample(nrow(insurance_example),0.75*nrow(insurance_example))
train<-insurance_example[index,]
test<-insurance_example[-index,]
#5 run corrected model on training data set 
insurance_example_model<-lm(charges~.,data = train)
train$pred_charges<-predict(insurance_example_model)
#6 calculate residules on training dataset 
train$res<-residuals(insurance_example_model)
# 7 calculate RMSE for training data set 
train_rmse<-sqrt(mean(train$res^2))
train_rmse
#8 run model on testing dataset
test$pred_charges<-predict(insurance_example_model,test)
#9 calculate residuals on testing dataset
test$res<-test$charges - test$pred_charges
# 10 calculate residuals on testing dataset
test_rmse<-sqrt(mean(test$res^2))
test_rmse
