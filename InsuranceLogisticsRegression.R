
#binary logistic regression 

adult_salary<-read.csv("C:/Users/asif/Downloads/adult.csv",header = T)
dim(adult_salary)

str(adult_salary)

# remove id : x
adult_salary$X<-NULL
# recode income : >50k = 1 , <=50k =0 [Hint: ifelse]
adult_salary$income<-ifelse(adult_salary$income=="<=50K","0","1")
adult_salary$income<-as.factor(adult_salary$income)
str(adult_salary)

# age 0,28,40,55,Inf [ Hint: cut]
adult_salary$age<-cut(adult_salary$age,breaks = c(0,28,40,55,Inf),labels = c("a1","a2","a3","a4"))

#workclass
#Federal-gov+Local-gov+State-gov = gov_emp
#Self-emp-inc+Self-emp-not-inc = self_emp
#Never-worked+Without-pay= not_working 
# convert factor into charecter 
adult_salary$workclass<-as.character(adult_salary$workclass)
adult_salary$workclass[adult_salary$workclass=="Federal-gov" | adult_salary$workclass=="Local-gov"| adult_salary$workclass=="State-gov"]<-"gov_emp"
adult_salary$workclass[adult_salary$workclass=="Self-emp-inc" | adult_salary$workclass=="Self-emp"| adult_salary$workclass=="not-inc"]<-"self_emp"
adult_salary$workclass[adult_salary$workclass=="never-worked" | adult_salary$workclass=="Working"| adult_salary$workclass=="Without pay"]<-"not_working"
summary(adult_salary$workclass)
str(adult_salary)
summary(adult_salary$education)
#preschool+1st-4th+5th-6th+7th-8th+9yh=10th
#11th-12th-"hs"

adult_salary$education<-as.character(adult_salary$education)
adult_salary$education[adult_salary$education=="preschool" | adult_salary$education=="1st-4th"| adult_salary$education=="5th-6th" | adult_salary$education=="7th-8th" | adult_salary$education=="9th"| adult_salary$education=="10th"]<-"school"
adult_salary$education[adult_salary$education=="11th" | adult_salary$education=="12th" | adult_salary$education=="HS-grad"]<-"HS"
adult_salary$education[adult_salary$education=="Assoc-acdm" | adult_salary$education=="Assoc-voc" | adult_salary$education=="Prof-school" |adult_salary$education=="Some-college"]<-"voc_edu"
View(adult_salary$education)
summary(adult_salary$marital.status)
#Divorced+Seperated+widowed+Married-spouse-absent<-"single"
#Married-AF-spouse+Married-civ-spouse<-"married"
adult_salary$marital.status<-as.character(adult_salary$marital.status)
adult_salary$marital.status[adult_salary$marital.status=="Divorced" | adult_salary$education=="Separated"| adult_salary$education=="Married-spouse-absent"]<-"single"
adult_salary$marital.status[adult_salary$marital.status=="Married-AF-spouse" | adult_salary$education=="Married-civ-spouse"]<-"married"
View(adult_salary$marital.status)
summary(adult_salary$occupation)
#Armed Forces+Protective-serv<-"security"
#Adm-clerical+Exec-managerial+Tech-support+Sales<-"management"
#craft_repair+machine-op-insec+handler-cleaners<-"machine_maintenance"
#other-service+farming-fish+transport_moving<-"others"
adult_salary$occupation<-as.character(adult_salary$occupation)
adult_salary$occupation[adult_salary$occupation=="Armed Forces" | adult_salary$occupation=="Protective-serv"| adult_salary$occupation=="private-house-serv"]<-"security"
adult_salary$occupation[adult_salary$occupation=="Adm-clerical" | adult_salary$occupation=="Exec-managerial" | adult_salary$occupation=="Tech-support"]<-"management"
adult_salary$occupation[adult_salary$occupation=="Craft-repair" | adult_salary$occupation=="Machine-op-inspct"| adult_salary$occupation=="Handlers-cleaners"]<-"machine_maintenance"
adult_salary$occupation[adult_salary$occupation=="other-service" | adult_salary$occupation=="farming-fishing" | adult_salary$occupation=="Transport-moving"]<-"Other-service"
View(adult_salary$occupation)
summary(adult_salary$race)
adult_salary$race<-as.character(adult_salary$race)
adult_salary$race[adult_salary$race=="Amer-Indian-Eskimo-Eskimo" | adult_salary$race=="Asian-Pac-Islander"]<-"Other"
View(adult_salary$race)
adult_salary$native.country<-as.character(adult_salary$native.country)
adult_salary$native.country[adult_salary$native.country!="United-States"]<-"non-us"
View(adult_salary$native.country)
str(adult_salary)
names<-c("workclass","education","marital.status","occupation","race","native.country")
adult_salary[,names]<-lapply(adult_salary[,names],as.factor)
write.csv(adult_salary,file="adult_salary.csv")
#LoGISTIC REgression
#PROBABILITY
#ODD
#ODD'S RATIO
#EQUATION FOR BLR
#glm function
library(caret)
library(lattice)
library(ggplot2)
ind<-createDataPartition(adult_salary$age,p=0.75,list=F)
set.seed(123)
training_income<-adult_salary[ind,]
testing_income<-adult_salary[-ind,]
income_model<-glm(income~.,data=training_income,family = "binomial")
str(adult_salary)
adult_salary$native.country<-NULL
summary(income_model)
#Null deviance should be greater than residual deviance and calculate by maximum likelihood estimator-improving correct 0 and 1
#LS<- the value of coefficients when used reduced the difference between actual value and predicted of dep variable w.r.t linear regression
#MLE<- the value of coef when used increases the likelihood of getting correct identification of "1" and "0"
dim(adult_salary)
training_income$pred_prob<-predict(income_model,type="response")# with type= "response" generates result as y=1/1-e^(bo+b1x1+b2x2+...bnxn[lm]
head(training_income)
#threshold
#pred_income
training_income$pred_income<-ifelse(training_income$pred_prob>=0.5,"1","0")
head(training_income)
table(predicted=training_income$income,actual=training_income$pred_income)
(17208+3336)/32561# accuracy
3336/(3336+1407) # sensitivity
17208/(17208+2471) # specificity
dim(adult_salary)
misclassification<-1-accuracy
#take threshold as 0.1 and generate table 1 is high
# take threshold as 0.9 and generate table 0 is high
training_income$pred_income<-ifelse(training_income$pred_prob>=0.1,"1","0")
table(actual=training_income$income,predict=training_income$pred_income)
training_income$pred_income<-ifelse(training_income$pred_prob>=0.9,"1","0")
table(actual=training_income$income,predict=training_income$pred_income)
#ROC-R : Receiver operating characteristic Curve in R
install.packages("ROCR")
library(ROCR)
library(gplots)
# The cut off that gives high and similar sensitivity and specificity is the best cutoff
pred<-prediction(training_income$pred_prob,training_income$income)# prediction-probability value
perf<-performance(pred,"tpr","fpr")#perf<-ROCR::performance
plot(perf,colorize=T,print.cutoffs.at=seq(0.1,by=0.1))
auc1<-performance(pred,"auc")
auc1@y.values
training_income$pred_income<-ifelse(training_income$pred_prob>=0.3,"1","0")
table(predicted=training_income$income,actual=training_income$pred_income)
#sen<-TP/TP+FN
#spe<-TN/TN+FP

4530/(4646+3232)# sensitivity
15383/(15383+4530)#specificity
(15383+4530)/32561 # accuracy
testing_income$pred_prob<-predict(income_model,testing_income,type="response")
testing_income$pred_income<-ifelse(testing_income$pred_prob>0.3,"1","0")
table(actual=testing_income$income,predict=testing_income$pred_income)
acc<-(5071+1529)/nrow(testing_income)
sen1<-1529/(426+1529)
spe1<-5071/(1113+5071)
#comparisan w.r.t auc
pred_test<-prediction(testing_income$pred_prob,testing_income$income)
perf_test_auc<-performance(pred_test,"auc")
perf_test_auc@y.values
auc1@y.values
str(adult_salary)
#Homework : Smote package : Undersample,oversample,balanced sample
# Decision Trees- comparing model with Binary Model
library(rpart)
library(rpart.plot)
tree_adult_salary<-rpart(income~.,data=training_income)
rpart.plot(tree_adult_salary)
pred_tree<-predict(tree_adult_salary,training_income,type="class")
table(predicted=pred_tree,actual=training_income$income)
(17472+2961)/32561 #accuracy
2961/(2961+2925) # sensitivity
17472/(17472+1064) # specificity
