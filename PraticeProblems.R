train_house2<-read.csv("C:/Users/asif/Downloads/train (1).csv",header=T)
testing_house2<-read.csv("C:/Users/asif/Downloads/test.csv",header=T)
dim(testing_house2)
colSums(is.na(testing_house2))
dim(train_house2)
str(train_house2)
colSums(is.na(train_house2))
colSums(is.na(testing_house2))
train_house2$PoolQC<-NULL
train_house2$Fence<-NULL
train_house2$MiscFeature<-NULL
testing_house2$PoolQC<-NULL
testing_house2$Fence<-NULL
testing_house2$MiscFeature<-NULL
library(DMwR)
library(lattice)
library(grid)
train_house2<-knnImputation(train_house2[,names(train_house2)])
testing_house2<-knnImputation(testing_house2[,names(testing_house2)])
head(train_house2)
summary(train_house2)
train_house2<-train_house2[,c("OverallQual","GrLivArea","TotalBsmtSF","GarageCars","FullBath","SalePrice")]
testing_house2<-testing_house2[,c("OverallQual","GrLivArea","TotalBsmtSF","GarageCars","FullBath")]
head(testing_house2)
summary(testing_house2)
train_house2$OverallQual_grp<-cut(train_house2$OverallQual,breaks=c(0,3,7,Inf),labels=c("c1","c2","c3"))
head(train_house2)
library(ggplot2)
# plot saleprice-y,x-overallqual-grp
ggplot(train_house2,aes(x=OverallQual_grp,y=SalePrice))+
  geom_bar(stat="identity",fill="green")+
  labs(x="OverallQual_grp",y="SalePrice",title="bar diagram")
train_o<-train_house2[,c("OverallQual","GrLivArea","TotalBsmtSF","GarageCars","FullBath","SalePrice")]
head(train_o)
UDF<-function(x){
  (x-min(x))/((max(x))-(min(x)))
}
train_o<-as.data.frame(apply(train_o,2,UDF))
testing_house2<-as.data.frame(apply,testing_house2,2,UDF)
set.seed(123)
index3<-createDataPartition(train_o$SalePrice,p=0.7,list=F)
train_house3<-train_o[index3,]
library(neuralnet)
set.seed(100)
NN = neuralnet(SalePrice~.,data=train_o,linear.output = T ) 
plot(NN)
NN$net.result
prediction_train<-compute(NN,train_o)
str(prediction_train)
ActualPrediction<-prediction_train$net.result*(max(train_o$SalePrice)) + min(train_o$SalePrice)
ActualPrediction<-data.frame(ActualPrediction)
#write.csv(submit.df,file="Submission_20171130_4.csv")
dim(train_o)
library(caret)
head(a)
new<-data.frame(train_o$SalePrice,ActualPrediction)
head(new)
RMSE(new$ActualPrediction,new$train_o.SalePrice)
lm1<-lm(SalePrice~.,data=train_o)
RMSE(fitted(lm1),new$train_o.SalePrice)
summary(lm1)
