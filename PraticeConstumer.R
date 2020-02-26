churn_bankdataK<-read.csv("C:/Users/asif/Downloads/Churn_Modelling (1).csv",header=T)
dim(churn_bankdataK)
str(churn_bankdataK)
colSums(is.na(churn_bankdataK))
churn_bankdataK$RowNumber<-NULL
churn_bankdataK$Surname<-NULL
churn_bankdataK$Geography<-NULL
vect_fact<-c("HasCrCard","IsActiveMember","Exited")
churn_bankdataK[,vect_fact]<-lapply(churn_bankdataK[,vect_fact],factor)
summary(churn_bankdataK$Age)
#churn_bankdataK$Age<-cut(churn_bankdataK$Age,
                             #breaks = c(0,18,44,Inf),labels = c("c1","c2","c3"))

# check if there exists a corelation between Tenure and Total Amount
cor(churn_bankdataK$Tenure,churn_bankdataK$Balance)
#recode 1 as churner and 0 as non churner
churn_bankdataK$Exited<-as.character(churn_bankdataK$Exited)
churn_bankdataK$Exited[churn_bankdataK$Exited=="1"]<-"churner"
churn_bankdataK$Exited[churn_bankdataK$Exited=="0"]<-"non-churner"
summary(churn_bankdataK$Exited)
churn_bankdataK$Exited<-as.factor(churn_bankdataK$Exited)
str(churn_bankdataK$Exited)
str(churn_bankdataK)
library(dplyr)
# perportional destribution of churner/ non churner wrt gender 
prop.table(table(churn_bankdataK$Exited, churn_bankdataK$Gender), margin=2)
#CorPlot
library(corrplot)
churn_numeric<-churn_bankdataK
str(churn_numeric)
vect_numeric<-c("HasCrCard","IsActiveMember","Age","Gender","Exited")
summary(churn_numeric$Exited)
churn_numeric$Geography<-NULL
str(churn_numeric)
churn_numeric[,vect_numeric]<-lapply(churn_numeric[,vect_numeric],as.numeric)
mat1<-cor(churn_numeric)
corrplot(mat1,method = "circle",type="lower")
summary(churn_bankdataK)
# Performing Regression Analysis
library(caret)
library(lattice)
library(ggplot2)
ind2<-createDataPartition(churn_bankdataK$Exited,p=0.75,list=F)
set.seed(123)
training_churnbank<-churn_bankdataK[ind2,]
testing_churnbank<-churn_bankdataK[-ind2,]
churn_model<-glm(Exited~.,data=training_churnbank,family = "binomial")
summary(churn_model)
#Null deviance should be greater than residual deviance and calculate by maximum likelihood estimator-improving correct 0 and 1
#LS<- the value of coefficients when used reduced the difference between actual value and predicted of dep variable w.r.t linear regression
#MLE<- the value of coef when used increases the likelihood of getting correct identification of "1" and "0"
training_churnbank$pred_prob<-predict(churn_model,type="response")# with type= "response" generates result as y=1/1-e^(bo+b1x1+b2x2+...bnxn[lm]
head(training_churnbank)
#threshold
#pred_income

training_churnbank$pred_churn<-ifelse(training_churnbank$pred_prob>=0.3,"1","0")
head(training_churnbank)
table(predicted=training_churnbank$Exited,actual=training_churnbank$pred_churn)
(5788+254)/7501 # accuracy
library(ROCR)
pred<-prediction(training_churnbank$pred_prob,training_churnbank$Exited)# prediction-probability value
perf<-performance(pred,"tpr","fpr")#perf<-ROCR::performance
plot(perf,colorize=T,print.cutoffs.at=seq(0.1,by=0.1))
auc1<-performance(pred,"auc")
auc1@y.values
training_churnbank$pred_churn<-ifelse(training_churnbank$pred_prob>=0.6,"1","0")
table(predicted=training_churnbank$pred_churn,actual=training_churnbank$Exited)
(469+5550)/7501 # accuracy
469/469+1059 # sensitivity
5550/(5550+423) # specificity
# Decision Tree
library(rpart)
library(rpart.plot)
decision_treebank<-rpart(Exited~.,data = churn_bankdataK)
rpart.plot(decision_treebank,cex=0.6)
pred_survived_rpartbank<-predict(decision_treebank,churn_bankdataK,type = "class")
table(predicted=pred_survived_rpartbank, actual=churn_bankdataK$Exited)
(7641+905)/10000 # accuracy
905/(905+1132)   # sensitivity
7641/(7641+322)  # specificity
# Random Forest
library(randomForest)
rfmodel_bank<-randomForest(Exited~.,data=churn_bankdataK,importance=T,ntree=30)
plot(rfmodel_bank)
rfmodel_bank$importance
#knn algoritm
str(churn_numeric)
churn_bankdataK1<-churn_numeric[,-11]
str(churn_bankdataK1)
dim(churn_bankdataK)
data("UKgas")
str(UKgas)
summary(UKgas)
# check if the average of EstimatedSalary is significant different w.r.t churner and non-churner
t.test(EstimatedSalary~Exited,data=churn_bankdataK,alternative="two.sided",var.equal=T)
# 2.
churn_bankdataK1<-aov(CreditScore~Exited,data=churn_bankdataK)
summary(churn_bankdataK1)
# Credit Score different w.r.t Geography
#anova2<-aov(CreditScore~Geography,data=churn_bankdataK)
#summary(anova2)
#CreditScore is significant Different w.r.t Geography & Exited
#twowayanova(CreditScore~Geography+Exited+Geography*Exited,data=churn_bankdataK)
#aggregate(CreditScore~Exited,data=churn_bankdataK,FUN=mean)
#aggregate(CreditScore~Geography,data=churn_bankdataK,FUN=mean)
#cor balance with creditscore
cor.test(churn_bankdataK$CreditScore,churn_bankdataK$Balance)
cor.test(churn_bankdataK$Age,churn_bankdataK$CreditScore)
# avg CreditSCore for France is 650
# avg CreditScore for France >650
#t.test(churn_bankdataK$Geography,alternative = "greater",mu=650)
#s1<-subset(churn_bankdataK,churn_bankdataK$Geography=="France")
#To check data is Normally Distributed
#s2<-churn_bankdataK[1:5000,]
#shapiro.test(s2$CreditScore)
