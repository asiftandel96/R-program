titanicdata <- read.csv("C:/Users/asif/Downloads/titanic_train.csv",header = T,na.strings = c("","","na"))
summary(titanicdata)
colSums(is.na(titanicdata))
str(titanicdata)
# if there are more than 50% na present in the column drop the column
#else check the structure of the data 
#if factor replace with mode
# if num and sckewed then replace with median else replaced with mean
titanicdata$Cabin<-NULL# no of na are greater than 50%
summary(titanicdata$Age)
titanicdata$Age[is.na(titanicdata$Age)]<-median(titanicdata$Age,na.rm=T)
summary(titanicdata$Embarked)
titanicdata$Embarked[is.na(titanicdata$Embarked)]<-"S"
titanicdata$PassengerId<-NULL
titanicdata$Survived<-as.factor(titanicdata$Survived)
titanicdata$Name<-NULL
titanicdata$Ticket<-NULL
#preparing predictors
#Merging categories
#Selecting the split variable
#CHAID Algorithm- Chi-sq Automatic Interaction Detection Algorithm - with factor variable
#classification and factor-independent variable
#r part- anova
#Classification DT dependent var:cat with n categories
#Regression DT: dependent var:continous
#dep_var:categorical
#ind_var:categorical
# Decision Tree -non-binary trees
titanicdata$Age<-cut(titanicdata$Age,breaks =c(0,22,30,35,Inf),labels=c("c1","c2","c3","c4"))
summary(titanicdata$Fare)
titanicdata$Fare<-cut(titanicdata$Fare,breaks =c(0,8,15,33,Inf),labels=c("a1","a2","a3","a4"))
titanicdata$SibSp<-as.factor(titanicdata$SibSp)
titanicdata$Parch<-as.factor(titanicdata$Parch)
titanicdata$Fare[is.na(titanicdata$Fare)]<-"a4"
titanicdata_1<-subset(titanicdata,!(titanicdata$SibSp=="5"|titanicdata$SibSp=="8"))
titanicdata_1<-subset(titanicdata_1,!(titanicdata_1$Parch=="5"|titanicdata_1$Parch=="6"))
summary(titanicdata_1)
install.packages("partykit")
library(partykit)
library(grid)
library(libcoin)
library(mvtnorm)
install.packages("CHAID",repos="http://R-Forge.R-project.org",type="source")
library(CHAID)
# Decision Tree implementation

treemodel<-chaid(Survived~.,data = titanicdata_1)
str(titanicdata_1)
titanicdata_1$Pclass<-as.factor(titanicdata_1$Pclass)
plot(treemodel,type="simple")
pred_survived<-predict(treemodel,titanicdata_1)
table(predicted=pred_survived,actual=titanicdata_1$Survived)
accuracy<-(502+215)/873
502/(30+502)
215/(215+126)
str(titanicdata_1)
# comparing it with binary logistic regression
bin_titanic_model<-glm(Survived~.,data=titanicdata_1,family="binomial")
pred_prob<-predict(bin_titanic_model,titanicdata_1,type="response")
library(ROCR)
library(gplots)
pred<-prediction(pred_prob,titanicdata_1$Survived)
perf<-performance(pred,"tpr","fpr")
plot(perf,colorize=T,print.cutoffs.at=seq(0.1,by=0.1))
pred_survived1<-ifelse(pred_prob<=0.35,0,1)
table(predicted=pred_survived1,actual=titanicdata_1$Survived)
accuracy<-(412+271)/873
412/(412+120)
271/(271+70)
# using partykit package

tree1<-partykit::ctree(Survived~.,data=titanicdata_1)
plot(tree1)
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
titanicdata<-read.csv("C:/Users/asif/Downloads/titanic_train.csv",header = T,na.strings = c("","","na"))
titanicdata$Age[is.na(titanicdata$Age)]<-median(titanicdata$Age,na.rm = T)
titanicdata$Embarked[is.na(titanicdata$Embarked)]<-"S"
str(titanicdata)
titanicdata$PassengerId<-NULL
titanicdata$Cabin<-NULL
titanicdata$Ticket<-NULL
titanicdata$Name<-NULL

titanicdata$Pclass<-as.factor(titanicdata$Pclass)
titanicdata$SibSp<-as.factor(titanicdata$SibSp)
titanicdata$Parch<-as.factor(titanicdata$Parch)
titanicdata$Survived<-as.factor(titanicdata$Survived)

library(CHAID)
titanic_rpartmodel<-rpart(Survived~.,data = titanicdata)
rpart.plot(titanic_rpartmodel,cex=0.6)

#titanic_model<-chaid(Survived~.,data = titanicdata)
pred_survived_rpart<-predict(titanic_rpartmodel,titanicdata)

pred_survived_rpart<-predict(titanic_rpartmodel,titanicdata,type = "class")
table(predicted=pred_survived_rpart, actual=titanicdata$Survived)
accuracy<-(498+244)/873
498/(498+51)
244/(244+98)
#
HR_data<- read.csv("C:/Users/asif/Downloads/HR_comma_sep (1).csv", header=T)
str(HR_data)
summary(HR_data$sales)
#IT+TECHNICAL+PRODUCT_MG+MANAGEMENT<-DEVELOPMENT
#marketing+sales<-marketing
#Rando+support<-Support
HR_data$sales<-as.character(HR_data$sales)
HR_data$sales[HR_data$sales=="IT" | HR_data$sales=="product_mng" | HR_data$sales=="technical"]<-"development"
HR_data$sales[HR_data$sales=="marketing" | HR_data$sales=="sales"]<-"Marketing"
HR_data$sales[HR_data$sales=="RandD" | HR_data$sales=="support"]<-"support"
HR_data$sales[HR_data$sales=="accounting" | HR_data$sales=="hr"]<-"HR"
View(HR_data$sales)
HR_data$sales<-as.factor(HR_data$sales)
str(HR_data$sales)
summary(HR_data$sales)
str(HR_data)
names<-c("number_project","time_spend_company","Work_accident","left","promotion_last_5years")
HR_data[,names]<-lapply(HR_data[,names],as.factor)
summary(HR_data$time_spend_company)
#"2-3, >3 to 6, above 6"
HR_data$time_spend_company<-as.numeric(HR_data$time_spend_company)
HR_data$time_spend_company<-ifelse(HR_data$time_spend_company,"level1",ifelse(HR_data$time_spend_company))
HR_data$time_spend_company<-as.factor(HR_data$time_spend_company)
summary(HR_data$time_spend_company)
HR_data1<-read.csv("C:/Users/asif/Downloads/HR_comma_sep (1).csv",header = T)
HR_data$time_spend_company<-HR_data1$time_spend_company
HR_data$time_spend_company<-cut(HR_data$time_spend_company,breaks=c(0,4,7,Inf),labels = c("level1","level2","level3"))
summary(HR_data1$time_spend_company)
str(HR_data)
summary(HR_data)
library(rpart)
library(rpart.plot)
salary_tree<-rpart(salary~.,data=HR_data)
rpart.plot(salary_tree)
salary_pred<-predict(salary_tree,HR_data,type="class")
table(predicted=salary_pred,actual=HR_data$salary)
# tree regression
# dep_var: numeric
#ind_varnumeric/factor
# classification regression-Decision Tree
iris1<-iris
str(iris1)
tree1<-rpart(Species~.,data=iris1)
rpart.plot(tree1)
tree2<-rpart(Sepal.Length~.,data=iris1)
rpart.plot(tree2)
summary(iris1$Sepal.Length)
summary(iris1$Sepal.Width)
#insurance
insurancedata<- read.csv("C:/Users/asif/Downloads/insurance2.csv")
str(insurancedata)
insurance$X<-NULL
insurance_tree<-rpart(charges~.,data=insurancedata)
rpart.plot(insurance_tree)
insurance_pred_charges<-predict(insurance_tree,insurancedata)
head(insurance_pred_charges)
res1<-insurancedata$charges-insurance_pred_charges
rmse1<-sqrt(mean(res1**2))# 4586.77
library(e1071)
skewness(insurance$charges)
shapiro.test(insurance$charges)
insurance_lm<-lm(charges~.,data=insurancedata)
pred_lm<-predict(insurance_lm,insurancedata)
res2<-insurancedata$charges-pred_lm
rmse2<-sqrt(mean(res**2))# 4807.282
# 1) BOOTSTRAP-MULTIPLE TREES
# 2) BAGGING
# 3) CALCULATION OF OOB-OUT OF BAG ERROR
