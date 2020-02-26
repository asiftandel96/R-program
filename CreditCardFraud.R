#Getting the data and loading the data into RStudio Platform 
creditfraud_data<-read.csv("C:/Users/asif/Downloads/creditcard.csv",header=T)
dim(creditfraud_data)
str(creditfraud_data)
# Checking for the null values
colSums(is.na(creditfraud_data))
summary(creditfraud_data$Class)
# Converting some necessary variable into factors
creditfraud_data$Class<-as.factor(creditfraud_data$Class)
str(creditfraud_data$Time)
# Handling imbalance dataset
install.packages("smotefamily")
library(smotefamily)
library(DMwR)
newCreditFraud_Data<- SMOTE(Class~.,creditfraud_data, perc.over=300, perc.under=300,k=3,learner=NULL)
table(newCreditFraud_Data$Class)
dim(newCreditFraud_Data)
str(newCreditFraud_Data)
newCreditFraud_Data1<-newCreditFraud_Data[,-31]
str(newCreditFraud_Data1)
# Preparing the data for model building and dividing it into training and testing data
set.seed(123)
index4<-sample(nrow(newCreditFraud_Data1),0.75*nrow(newCreditFraud_Data1))
training_fraud<-newCreditFraud_Data1[index4,]
testing_fraud<-newCreditFraud_Data1[-index4,]
dim(training_fraud)
dim(testing_fraud)
ytrain<-newCreditFraud_Data$Class[index4] # vector of only dependent variables
ytest<-newCreditFraud_Data$Class[-index4]
length(ytest)
length(ytrain)
library(class)
#Now the data is cleaned and prepared for model building,so it is time to choose the best ML Algorithms
#for the dataset
#As the datset has almost numeric so it is advisable to go for KNN Algorithms as it support numeric value
#Here the depedent variable is status[0 for non-fraud and 1 for fraud]
knn_fraud_model<-knn(training_fraud,testing_fraud,k=sqrt(nrow(training_fraud)),cl=ytrain)
table(predict=knn_fraud_model,actual=ytest)
(996+167)/1599 # accuracy
167/(167+344) # sensitivity
92/(92+966) # specificity
# Decision Trees-It is the best algorithm for this dataset
library(rpart)
library(rpart.plot)
creditcardFraud_tree<-rpart(Class~.,data=newCreditFraud_Data)
plot.new()
rpart.plot(creditcardFraud_tree)
creditFraud_pred<-predict(creditcardFraud_tree,newCreditFraud_Data,type="class")
table(predicted=creditFraud_pred,actual=newCreditFraud_Data$Class)
(4388+1675)/6396 # accuracy
1675/(1675+293) # sensitivity
4388/(40+4388) # specificity
