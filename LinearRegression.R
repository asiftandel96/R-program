house<-read.csv("C:/Users/asif/Downloads/house (1).csv",header = T)
dim(house) # row and columns in the dataset
str(house) # structure of the dataset
# to check null values 
colSums(is.na(house))

# check for the outliers in dependent variable
summary(house$price)
# checking Inter Quartile Range
IQR(house$price)*1.5 + 645000 # 1.5(IQR)+Q3
boxplot(house$price)
library(e1071)
skewness(house$price)
# take a subset of non outlier observations 
house1<-subset(house,house$price<=1129575)
dim(house1)
plot.new()
boxplot(house1$price)
# remove not required variables 
house1$id<-NULL
house1$date<-NULL
house1$zipcode<-NULL
summary(house1)
skewness(house1$price)
head(house1)
house1$bedrooms<-factor(house1$bedrooms)
summary(house1$bedrooms)
house1$bedrooms<-as.numeric(house1$bedrooms)
house2<-subset(house1,house1$bedrooms<7)

# to convert int var in factor wherever needed : floors,bedrooms,waterfront,view,condition,grade
house2$bedrooms<-factor(house2$bedrooms) 

vect1<-c("floors","bedrooms","waterfront","view","condition","grade")
house2[,vect1]<-lapply(house2[,vect1],factor)
str(house2)
dim(house2)
# convert year into age 
house2$yr_built<-2015-house2$yr_built
write.csv(house2,file="housefile.csv")
write.csv(house2,file="E:/All_Data/house2.csv")

house2<-read.csv("housefile.csv",header = T)
str(house2)

house2$yr_renovated[house2$yr_renovated==0]<-2015
house2$yr_renovated<-2015-house2$yr_renovated

#head(house2$yr_renovated,20)

# to build model on the given house2 data 
# cross validation :
# hold out cross validation 
# k fold cross validation 
# rep. k fold cross validation 
# Leave One Out Cross Validation [LOOCV]

install.packages("caret")
library(caret)
library(lattice)
library(ggplot2)
library(caret)
str(house2)
set.seed(123)
index<-createDataPartition(house2$price,p=0.7,list = F)
train_house<-house2[index,]
test_house<-house2[-index,]
head(train_house)
head(test_house)
length(index)
head(index,20)

set.seed(123)
index1<-sample(nrow(house2),0.75*nrow(house2))
nrow(house2)
index1<-sample(20190,0.75*20190)
train_house_sample<-house2[index1,]
test_house_sample<-house2[-index1,]




# model MUST BE built on training data : RMSE_training 
# validate model on testing data : RMSE_training
# difference of RMSE_train and RMSE_test should not exceed by 15%

house_model<-lm(price~.,data=train_house)
train_house_new<-train_house
train_house_new$sqft_living<-NULL
house_model_new<-lm(price~.,data=train_house_new)
library(car)
vif(house_model_new)
train_house_new$pred_price<-predict(house_model_new)





summary(house_model)

summary(train_house$condition)
set.seed(123)
house_model2<-lm(price~.-(bedrooms),data = train_house)
summary(house_model2)

# predict prices on training data and calculate residuals
train_house$pred<-fitted(house_model2) # prediction of price
train_house$res<-residuals(house_model2) # y-yhat

# calculate Root Mean Square Error
rmsetrain<-sqrt(mean(train_house$res**2))
rmsetrain<-sqrt(mean(train_house$res^2))

rmsetrain

test_house$pred_price<-predict(house_model2,test_house)
head(test_house)

#factor grade has new level-1 
summary(train_house$grade)
summary(test_house$grade)
test_house$pred_price<-predict(house_model2,test_house)
test_house<-subset(test_house,test_house$grade!=1)
test_house$res<-test_house$price - test_house$pred_price
rmsetest<-sqrt(mean(test_house$res^2))
#test_house$pred_price<-predict(house_model2,test_house)
length(house_model2$residuals)
summary(house_model2)
rmsetrain #113540.6
rmsetest  #114906.7

library(car)
library(carData)
vif(house_model2)

# 
str(train_house)
train_house1<-train_house[,c(1,3,4,5,11,12,13,14,15,16,17,18)]
dim(train_house1)
library(corrplot)
mat1<-cor(train_house1)
corrplot(mat1,method = "circle",type="lower")
names(train_house)

