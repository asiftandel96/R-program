# Support Vector Machines-
mobile_data<-read.csv("C:/Users/asif/Downloads/mobile.csv",header=T)
str(mobile_data)
mobile_data$price_range<-as.factor(mobile_data$price_range)
mobile_data$touch_screen<-as.factor(mobile_data$touch_screen)
mobile_data$wifi<-as.factor(mobile_data$wifi)
mobile_data$dual_sim<-as.factor(mobile_data$dual_sim)
mobile_data$talk_time<-as.factor(mobile_data$talk_time)
mobile_data$four_g<-as.factor(mobile_data$four_g)
mobile_data$three_g<-as.factor(mobile_data$three_g)
mobile_data$blue<-as.factor(mobile_data$blue)
str(mobile_data)
library(caret)
library(lattice)
library(ggplot2)
ind<-createDataPartition(mobile_data$price_range,p=0.75,list=F)
set.seed(123)
training_mobile<-mobile_data[ind,]
head(training_mobile)
testing_mobile<-mobile_data[-ind,]
head(testing_mobile)
library(e1071)
svmfit_mobile<-svm(price_range~.,data=training_mobile,kernel="radial",gamma=1,cost=0.1)
print(svmfit_mobile)
plot(svmfit_mobile,training_mobile)
p<-predict(svmfit_mobile,training_mobile,type="class") # svm 
table(actual=training_mobile$price_range,pred=p)
1500/2000 # acc
confusionMatrix(mobile_data$price_range, sample(mobile_data$price_range)) # confusion matrix
# Decision Trees
mobile_data1<-mobile_data
str(mobile_data1)
library(rpart)
library(rpart.plot)
set.seed(123)
mobile_trees<-rpart(price_range~.,data=mobile_data1)
plot.new()
rpart.plot(mobile_trees,cex =0.5)
salary_mobile<-predict(mobile_trees,mobile_data1,type="class")
table(predicted=salary_mobile,actual=mobile_data1$price_range)
(477+365+354+432)/2000 # accuracy
confusionMatrix(mobile_data1$price_range, sample(mobile_data1$price_range)) 
# KNN Algorithms
mobile_data_knn<-mobile_data1[,-21]
str(mobile_data_knn)
mobile_data_knn$touch_screen<-as.numeric(mobile_data$touch_screen)
mobile_data_knn$wifi<-as.numeric(mobile_data$wifi)
mobile_data_knn$dual_sim<-as.numeric(mobile_data$dual_sim)
mobile_data_knn$talk_time<-as.numeric(mobile_data$talk_time)
mobile_data_knn$four_g<-as.numeric(mobile_data$four_g)
mobile_data_knn$three_g<-as.numeric(mobile_data$three_g)
mobile_data_knn$blue<-as.numeric(mobile_data$blue)
str(mobile_data_knn)
set.seed(123)
ind1<-sample(nrow(mobile_data_knn),0.75*nrow(mobile_data_knn))
training_knn_mobile<-mobile_data_knn[ind1,]
testing_knn_mobile<-mobile_data_knn[-ind1,]
dim(testing_knn_mobile)
dim(training_knn_mobile)

ytrain_knn<-mobile_data1$price_range[ind1] # vector of only dependent variables
ytest_knn<-mobile_data1$price_range[-ind1]
length(ytest_knn)
length(ytrain_knn)
library(class)


knn_mobile_model<-knn(training_knn_mobile,testing_knn_mobile,k=sqrt(nrow(training_knn_mobile)),cl=ytrain_knn)
table(predict=knn_mobile_model,actual=ytest_knn)
(131+114+106+108)/500 # accuracy
confusionMatrix(mobile_data1$price_range,sample(mobile_data1$price_range))
install.packages("plot3D")
library(plot3D)                
