train <- read.csv("C:/Users/asif/Downloads/train (1).csv", header = TRUE)
test <- read.csv("C:/Users/asif/Downloads/test.csv", header = TRUE)



train <- train[,c("OverallQual", "GrLivArea", "TotalBsmtSF", "GarageCars",
                  "FullBath", "SalePrice")]

test <- test[,c("OverallQual", "GrLivArea", "TotalBsmtSF", "GarageCars",
                "FullBath")]

summary(train)
summary(test)

train_o <- train

summary(train$SalePrice) # CLEAN
summary(train$OverallQual) # CLEAN
summary(train$GrLivArea)# CLEAN
summary(train$TotalBsmtSF)# CLEAN
summary(train$GarageCars)# CLEAN
summary(train$FullBath)


summary(test$SalePrice) # CLEAN
summary(test$OverallQual) # CLEAN
summary(test$GrLivArea)# CLEAN
summary(test$TotalBsmtSF)
summary(test$GarageCars)
summary(test$FullBath)# CLEAN


#Replace missing value with median
summary(test$TotalBsmtSF)
test$TotalBsmtSF[which(is.na(test$TotalBsmtSF))] <- 988.0

summary(test$GarageCars)
test$GarageCars[which(is.na(test$GarageCars))] <- 2.0
train_o <- train

train_o$OverallQual_grp<-cut(train_o$OverallQual,
                             breaks = c(0,3,7,Inf),labels = c("c1","c2","c3"))
library(ggplot2)

ggplot(train_o,aes(x=OverallQual_grp,y=SalePrice))+
  geom_bar(stat="identity",fill="green")+
  labs(x="OverallQual_grp",y="SalePrice",title="Bar Diagram")
summary(train_o$OverallQual_grp)




UDF <- function(x) {
  (x -min(x))/ (max(x)- min(x))
}


train <- as.data.frame(apply(train, 2, UDF))
test <- as.data.frame(apply(test, 2, UDF))
# SPLItting the data.


set.seed(100)
str(train)

index <- sample(nrow (train), round(0.6 * nrow(train)))
train.wp <- train[index,]
test.wp <- train[-index,]

dim(train.wp)
str(train.wp)
install.packages("neuralnet")
library(neuralnet)
nn_model <- neuralnet(SalePrice~., data=train.wp,linear.output = TRUE,hidden = 3)
str(train.wp)
nn_model$net.result
plot(nn_model)
prediction1 <- compute(nn_model, test)
str(prediction1)
ActualPrediction <-  prediction1$net.result * 
  (max(train_o$SalePrice)-min(train_o$SalePrice)) + min(train_o$SalePrice)

submit.df <- data.frame(Id = rep(1461:2919), SalePrice= ActualPrediction)
write.csv(submit.df, file = "Submission_20171130_4.csv")
dim(train)
dim(test)

str(train_o)
train_o$OverallQual_grp<-NULL
prediction_train <- compute(nn_model, train)
str(prediction1)
ActualPrediction <-  prediction_train$net.result * 
  (max(train_o$SalePrice)-min(train_o$SalePrice)) + min(train_o$SalePrice)
ActualPrediction<-data.frame(ActualPrediction)

library(caret)
head(a)
new<-data.frame(train_o$SalePrice,ActualPrediction)
head(new)
RMSE(new$ActualPrediction,new$train_o.SalePrice)

lm1<-lm(SalePrice~.,data=train_o)
RMSE(fitted(lm1),new$train_o.SalePrice)
summary(lm1)
