install.packages("neuralnet")
library(neuralnet)
data = read.csv("C:/Users/asif/Downloads/cereals.csv", header=T)
dim(data)
samplesize = 0.60 * nrow(data)
set.seed(80)
index = sample(seq_len ( nrow ( data ) ), size = samplesize )


# Create training and test set
datatrain = data[ index, ]
datatest = data[ -index, ]

normalize<-function(x){
  (x-min(x))/((max(x))-(min(x)))
}

train<-normalize(datatrain)
test<-normalize(datatest)
set.seed(100)
NN = neuralnet(rating ~ calories + protein + fat + sodium + fiber, train, 
               hidden = 1 ,linear.output = T,err.fct = "sse" )    
head(datatrain)
head(train)
plot(NN)

predict_testNN = compute(NN, train[,c(1:5)])
pred_ann<-predict_testNN$net.result
library(caret)


RMSE(pred_ann,train$rating)
head(train)

lm1<-lm(rating~.,data = train)
RMSE(fitted(lm1),train$rating)
predict_testNN = compute(NN, test[,c(1:5)])
predict_testNN$net.result


#1.51


diab<-read.csv("C:/Users/asif/Downloads/diabetes (1).csv",header = T)
dim(diab)

str(diab)
diab$Outcome<-factor(diab$Outcome)
diab$Glucose[diab$Glucose==0]<-median(diab$Glucose)
diab$BloodPressure[diab$BloodPressure==0]<-median(diab$BloodPressure)
diab$SkinThickness[diab$SkinThickness==0]<-median(diab$SkinThickness)
diab$Insulin[diab$Insulin==0]<-median(diab$Insulin)
diab$BMI[diab$BMI==0]<-median(diab$BMI)
diab$DiabetesPedigreeFunction[diab$DiabetesPedigreeFunction==0]<-median(diab$DiabetesPedigreeFunction)
str(diab)
dim(diab)
norm_data<-normalize(diab[,1:8])

summary(norm_data)
norm_data$outcome<-diab$Outcome
# ANN with BIN 
norm_data$outcome<-factor(norm_data$outcome)
str(norm_data)

ann_diab<-neuralnet(outcome~.,norm_data,hidden = 1,err.fct = "ce",
                    linear.output = F,act.fct = "logistic")


plot(ann_diab)
prob<-compute(ann_diab,norm_data)
prob1<-prob$net.result
class(prob1)
prob1<-prob1[,2]
prob1
pred_diab<-ifelse(prob1>0.5,"1","0")
table(actual=diab$Outcome,predicted=pred_diab)

glm_diab<-glm(Outcome~.,data=diab,family = "binomial")
pred_log<-predict(glm_diab,diab,type="response")

pred_diab_glm<-ifelse(pred_log>0.5,"1","0")
table(actual=diab$Outcome,predicted=pred_diab_glm)


library(ROCR)

pred<-prediction(pred_log,diab$Outcome)
perf<-performance(pred,"tpr","fpr")
ann_diab_h2<-neuralnet(outcome~.,norm_data,hidden = 2,err.fct = "ce",
                       linear.output = F,act.fct = "logistic")


plot(ann_diab_h2)
prob_h2<-compute(ann_diab_h2,norm_data)
prob2<-prob_h2$net.result
class(prob2)
prob1<-prob2[,2]
prob
pred_diab_h2<-ifelse(prob1>0.5,"1","0")
table(actual=diab$Outcome,predicted=pred_diab_h2)

ann_diab_h2<-neuralnet(outcome~.,norm_data,hidden = 3,err.fct = "ce",
                       linear.output = F,act.fct = "logistic")


prob_h2<-compute(ann_diab_h2,norm_data)
prob2<-prob_h2$net.result
class(prob2)
prob1<-prob2[,2]
prob
pred_diab_h2<-ifelse(prob1>0.5,"1","0")
table(actual=diab$Outcome,predicted=pred_diab_h2)


