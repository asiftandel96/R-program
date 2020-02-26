# 1) BOOTSTRAP-MULTIPLE TREES
# 2) BAGGING
# 3) CALCULATION OF OOB-OUT OF BAG ERROR

 Employee_Churn <- read.csv("C:/Users/asif/Downloads/Employee Churn.csv")
summary(Employee_Churn)
str(Employee_Churn)
Employee_Churn$sn<-NULL
Employee_Churn$status<-as.factor(Employee_Churn$status)
summary(Employee_Churn$exp)
library(CHAID)
library(partykit)
library(grid)
library(libcoin)
library(mvtnorm)
treemodel<-chaid(status~.,data=Employee_Churn)
pred<-predict(treemodel,Employee_Churn,method="class")
table(predicted=pred,actual=Employee_Churn$status)
(35+31)/83
31/33
35/50
plot(treemodel,type="simple")
library(rpart.plot)
treemodel_rpart<-rpart(status~.,data=Employee_Churn)
rpart.plot(treemodel_rpart)
install.packages("randomForest")
library(randomForest)
rfmodel<-randomForest(status~.,data=Employee_Churn,importance=T,ntree=20)
set.seed(123)
rfmodel1<-randomForest(status~.,data=Employee_Churn,importance=T,mtry=3)
plot(rfmodel)
rfmodel$importance
#randomization as per rows,variable,mtry
# tuning using grid search ***
library(caret)
library(lattice)
library(ggplot2)
control<-trainControl(method = "repeatedcv",number = 10,repeats = 3,search="grid")
tunegrid<-expand.grid(mtry=c(1:4))
rf_gridsearch<-train(status~function.+exp+gender+source,data=Employee_Churn,method="rf",tuneGrid=tunegrid,trControl=control)
# Decision Trees
adult_salary_tree<-read.csv("C:/Users/asif/Downloads/adult_salary2.csv")
str(adult_salary_tree)
adult_salary_tree$X<-NULL
adult_salary_tree$income<-as.factor(adult_salary_tree$income)
library(rpart)
library(rpart.plot)
treemodel_rpart<-rpart(income~.,data=adult_salary_tree)
rpart.plot(treemodel_rpart)
pred_income1<-predict(treemodel_rpart,adult_salary2,type="class")
table(predicted=pred_income1,actual=adult_salary2$income)
(23495+3207)/32561
