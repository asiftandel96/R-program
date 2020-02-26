german_credit <- read.csv("C:/Users/asif/Downloads/german_credit.csv",header = T)
str(german_credit)
summary(german_credit$installment_as_income_perc)
german_credit$default<-as.factor(german_credit$default)
str(german_credit)
summary(german_credit$credit_history)
# installment_as_income perc,present_res_since,credit_this_bank,job as factors
german_credit$installment_as_income_perc<-as.factor(german_credit$installment_as_income_perc)
german_credit$present_res_since<-as.factor(german_credit$present_res_since)
german_credit$credits_this_bank<-as.factor(german_credit$credits_this_bank)
german_credit$job<-as.factor(german_credit$job)
german_credit$people_under_maintenance<-as.factor(german_credit$people_under_maintenance)
str(german_credit)
library(caret)
library(lattice)
library(ggplot2)
ind<-createDataPartition(german_credit$default,p=0.75,list=F)
set.seed(123)
training_default<-german_credit[ind,]
testing_default<-german_credit[-ind,]
default_model<-glm(default~.,data=training_default,family = "binomial")
default_null<-glm(default~1,data=training_default,family = "binomial")
summary(default_model)
# run stepwise regression
step(default_null,direction = "forward",scope=list(lower=default_null,upper=default_model))
step(default_null,direction="backward",scope=list(lower=default_null,upper=default_model))
bin_model<-glm(formula = default ~ account_check_status + credit_history + 
      duration_in_month + housing + purpose + foreign_worker + 
      present_emp_since + personal_status_sex + installment_as_income_perc + 
      credit_amount + other_installment_plans + age, family = "binomial", 
    data = training_default)
training_default$pred_prob<-predict(bin_model,type="response")
head(training_default)
pred<-prediction(training_default$pred_prob,training_default$default)# prediction-probability value
training_default$pred_default<-ifelse(training_default$pred_prob>0.35,"1","0")
table(predicted=training_default$default,actual=training_default$pred_default)
perf<-performance(pred,"tpr","fpr")#perf<-ROCR::performance
plot(perf,colorize=T,print.cutoffs.at=seq(0.1,by=0.1))
(410+152)/750 # accuracy
152/(152+115) # sensitivity
410/(410+73)  # specificity
confusionMatrix(table(predicted=training_default$default,actual=training_default$pred_default))
# Naive Bayes- mutually exclusive
