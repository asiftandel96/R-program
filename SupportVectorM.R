x=1:20
y=c(3,4,5,4,8,10,10,11,14,20,23,24,32,34,35,37,42,48,53,60)
train=data.frame(x,y)
plot.new()
plot(train,pch=16)
model<-lm(y ~ x, train)
abline(model)
library(e1071)
# fit a model.This Function Syntax is similar to lm function
model_svm<-svm(y ~ x , train)
model_svm
# Use the prediction for the data
pred<-predict(model_svm,train)
pred
#Plot the predictions and the plot to see our model fit
points(train$x,pred,col="blue",pch=4)
# Calculate RMSE For the svm model

train$pred<-fitted(model_svm) # prediction of price
train$res<-residuals(model_svm) # y-yhat

# calculate Root Mean Square Error
rmsetrain<-sqrt(mean(train$res**2))
rmsetrain
# Lm model
train$pred<-fitted(model) # prediction of price
train$res<-residuals(model) # y-yhat

# calculate Root Mean Square Error
rmsetrain_lm<-sqrt(mean(train$res**2))
rmsetrain_lm
# SVM Classification
library(e1071)
plot(iris)
plot(iris$Petal.Length,iris$Petal.Width,col=iris$Species)
plot(iris$Sepal.Length,iris$Sepal.Width,col=iris$Species)
set.seed(123)
s<-sample(150,100)
col<-c("Petal.Length","Petal.Width","Species")
iris_train<-iris[s,col]
iris_test<-iris[-s,col]
svmfit<-svm(Species~.,data=iris_train,kernel="linear")
print(svmfit)
plot(svmfit,iris_train)
p<-predict(svmfit,iris_train,type="class") # svm 
p
table(actual=iris_train$Species,pred=p)

plot.new()
svmfit1<-svm(Species~.,data=iris_train,kernel="radial")
plot(svmfit1,iris_train)

plot.new()
svmfit2<-svm(Species~.,data=iris_train,kernel="radial",gamma=10^-1)
plot(svmfit2,iris_train)

plot.new()
svmfit3<-svm(Species~.,data=iris_train,kernel="radial",gamma=1)
plot(svmfit3,iris_train)
plot.new()
svmfit4<-svm(Species~.,data=iris_train,kernel="radial",gamma=10)
plot(svmfit4,iris_train)
plot.new()
svmfit5<-svm(Species~.,data=iris_train,kernel="radial",ranges=list(cost=10^(-1:2)))
plot(svmfit5,iris_train)
svmfit6<-svm(Species~.,data=iris_train,kernel="radial",cost=1,gamma=1)
plot(svmfit6,iris_train)
svmfit7<-svm(Species~.,data=iris_train,kernel="radial",cost=0.1,gamma=1)
plot(svmfit7,iris_train)
svmfit8<-svm(Species~.,data=iris_train,kernel="radial",cost=10,gamma=1)
plot(svmfit8,iris_train)
