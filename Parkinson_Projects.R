
#Getting the data of Parkinson's Disease and loading into RStudio paltform
parkinson_data<-read.csv("C:/Users/asif/Downloads/parkinsons.data",header=T)
#checking the dimensions of the dataset
dim(parkinson_data)
#checking structure of the data
str(parkinson_data)
#Checking if the dataset has any missing values
colSums(is.na(parkinson_data))
summary(parkinson_data)
summary(parkinson_data$status)
# Clenaing the data: Data Collection
parkinson_data$name<-NULL
parkinson_data$status<-as.factor(parkinson_data$status)

parkinson_data1<-parkinson_data[,-17]
str(parkinson_data1)
# model building and dividing it into training and testing data
set.seed(123)
index3<-sample(nrow(parkinson_data1),0.75*nrow(parkinson_data1))
training_parkinson<-parkinson_data1[index3,]
testing_parkinson<-parkinson_data1[-index3,]
dim(training_parkinson)
dim(testing_parkinson)
ytrain<-parkinson_data$status[index3] # vector of only dependent variables
ytest<-parkinson_data$status[-index3]
length(ytest)
length(ytrain)
library(class)
#Now the data is cleaned and prepared for model building,so it is time to choose the best ML Algorithms
#for the dataset
#As the dataset has almost numeric so it is advisable to go for KNN Algorithms as it support numeric value
#Here the depedent variable is status[0 for healthy and 1 for Parkison's Disease]
knn_parkinson_model<-knn(training_parkinson,testing_parkinson,k=sqrt(nrow(training_parkinson)),cl=ytrain)
table(predict=knn_parkinson_model,actual=ytest)
(6+34)/49 # accuracy
34/(34+4) # sensitivity
6/(6+5) # specificity
# Decision Trees
library(rpart)
library(rpart.plot)
#Building a Decision Trees
parkison_tree<-rpart(status~.,data=parkinson_data)
plot.new()
rpart.plot(parkison_tree)
parkinson_pred<-predict(parkison_tree,parkinson_data,type="class")
table(predicted=parkinson_pred,actual=parkinson_data$status)
(141+38)/195 # accuracy
141/(141+6) # sensitivity
38/(38+10) # specificity


