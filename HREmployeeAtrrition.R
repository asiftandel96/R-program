hrdata<-read.csv("C:/Users/asif/Downloads/WA_Fn-UseC_-HR-Employee-Attrition.csv", header=T)

str(hrdata)

summary(hrdata$EmployeeCount)

#--------------------

# all values for employeecount is 1 so it has no use, we remove that variable
hrdata$EmployeeCount<-NULL

#-------------------

# all values for over18 is Y for all so  it has no use, we remove that variable
hrdata$Over18<-NULL

#-------------------
unique(hrdata$StandardHours)

hrdata$StandardHours<-NULL

#--------------------

hrdata$EmployeeNumber<-NULL

#--------------------

hrdata$EnvironmentSatisfaction<-ifelse(hrdata$EnvironmentSatisfaction==1,"Low",ifelse(hrdata$EnvironmentSatisfaction==2,"Medium",ifelse(hrdata$EnvironmentSatisfaction==3,"Hgh","Very High")))

hrdata$EnvironmentSatisfaction<-as.factor(hrdata$EnvironmentSatisfaction)

str(hrdata$EnvironmentSatisfaction)

unique(hrdata$EnvironmentSatisfaction)

#-------------------

unique(hrdata$Education)

hrdata$Education<-ifelse(hrdata$Education==1,"Below College",ifelse(hrdata$Education==2,"College",ifelse(hrdata$Education==3,"Bachelor",ifelse(hrdata$Education==4,"Master","Doctor"))))
hrdata$Education<-as.factor(hrdata$Education)

unique(hrdata$Education)

#_------------------

unique(hrdata$JobInvolvement)

hrdata$JobInvolvement<-ifelse(hrdata$JobInvolvement==1,"Low",ifelse(hrdata$JobInvolvement==2,"Medium",ifelse(hrdata$JobInvolvement==3,"High","Very High")))
hrdata$JobInvolvement<-as.factor(hrdata$JobInvolvement)

unique(hrdata$JobInvolvement)

#-------------------

unique(hrdata$JobSatisfaction)

hrdata$JobSatisfaction<-ifelse(hrdata$JobSatisfaction==1,"Low",ifelse(hrdata$JobSatisfaction==2,"Medium",ifelse(hrdata$JobSatisfaction==3,"High","Very High")))
hrdata$JobSatisfaction<-as.factor(hrdata$JobSatisfaction)

unique(hrdata$JobSatisfaction)

#--------------------

unique(hrdata$PerformanceRating)

hrdata$PerformanceRating<-ifelse(hrdata$PerformanceRating==3,"Excellent","Outstanding")
hrdata$PerformanceRating<-as.factor(hrdata$PerformanceRating)

unique(hrdata$PerformanceRating)

#-------------------

unique(hrdata$RelationshipSatisfaction)

hrdata$RelationshipSatisfaction<-ifelse(hrdata$RelationshipSatisfaction==1,"Low",ifelse(hrdata$RelationshipSatisfaction==2,"Medium",ifelse(hrdata$RelationshipSatisfaction==3,"High","Very High")))
hrdata$RelationshipSatisfaction<-as.factor(hrdata$RelationshipSatisfaction)

unique(hrdata$RelationshipSatisfaction)

#-------------------

unique(hrdata$WorkLifeBalance)

hrdata$WorkLifeBalance<-ifelse(hrdata$WorkLifeBalance==1,"Bad",ifelse(hrdata$WorkLifeBalance==2,"Good",ifelse(hrdata$WorkLifeBalance==3,"Better","Best")))
hrdata$WorkLifeBalance<-as.factor(hrdata$WorkLifeBalance)

unique(hrdata$WorkLifeBalance)

#--------------------

unique(hrdata$JobLevel)

hrdata$JobLevel<-as.factor(hrdata$JobLevel)

#--------------------------------------------------------------

str(hrdata)

#==============================================================
write.csv(adult,file="hremployeeatrritionnew.csv")
#hrdata<-read.csv("C:/Users/utkarsha/Documents/R Programs/hrdata_modified.csv",header=T)
#==============================================================

# training : testing 75 : 25
# run the model on training data
# select only required variables
# regenerate the model with significant variables
# predict probabilities on training data
# get cutoff
# calculate accuracy,sensitivity,specificity
# evaluate the model on testdata
# calculate accuracy,sensitivity and specificity for the data

#============================================================

summary(hrdata$Attrition)
hrdata$Attrition<-ifelse(hrdata$Attrition=="No",0,1)
hrdata$Attrition<-as.factor(hrdata$Attrition)
unique(hrdata$Attrition)
#------------------

library(caret)
library(ggplot2)
library(lattice)
set.seed(123)
ind<-createDataPartition(hrdata$Attrition,p=0.75,list = F) 

training_Attrition<-hrdata[ind,]

testing_Attrition<-hrdata[-ind,]

str(training_Attrition)

#-----------------------------------------

attrition_model<-glm(Attrition~.-(ï..Age+Department+Education+JobRole+HourlyRate+MonthlyRate+PercentSalaryHike+StockOptionLevel+TotalWorkingYears+PerformanceRating),data = training_Attrition ,family = "binomial")

summary(attrition_model)

#-----------------------------------------

training_Attrition$pred_prob<-predict(attrition_model,type = "response")

training_Attrition$pred_attrition<-ifelse(training_Attrition$pred_prob>=0.2,"1","0")
training_Attrition$pred_attrition<-as.factor(training_Attrition$pred_attrition)
head(training_income)

#-----------------------------------------
library(ROCR)
pred<-prediction(training_Attrition$pred_prob,training_Attrition$Attrition)
pref<-performance(pred,"tpr","fpr")
plot(pref, colorize = T , print.cutoffs.at = seq(0.1,by=0.1))

#-----------------------------------------
auc_attrition_model<-performance(pred,"auc")
auc_attrition_model@y.values 
testing_Attrition$pred_prob<-predict(attrition_model,testing_Attrition,type="response")
testing_Attrition$pred_attrition<-ifelse(testing_Attrition$pred_prob>=0.2,"1","0")
table(actual=testing_Attrition$Attrition,predict=testing_Attrition$pred_attrition)
pred_attrition<-prediction(testing_Attrition$pred_prob,testing_Attrition$Attrition)
perf_attrition_auc<-performance(pred_attrition,"auc")
perf_attrition_auc@y.values
