bankdata <-read.csv("C:/Users/asif/Downloads/bank-full.csv", header=T,sep=";")
str(bankdata)
summary(bankdata)
View(bankdata)
colSums(is.na(bankdata))
summary(bankdata$campaign)
bankdata<-subset(bankdata,bankdata$campaign<=10)
quantile(bankdata$previous,probs=c(0.9,0.95,0.97))
bankdata<-subset(bankdata,bankdata$previous<=5)
bankdata$duration<-cut(bankdata$duration,breaks= c(0,106,182,322,Inf),labels= c("Very_short","short","medium","long"))
bankdata$duration[is.na(bankdata$duration)]<-"Very_short"
bankdata$pdays<-ifelse(bankdata$pdays<=1,"Not-Contacted","Contacted")
bankdata$pdays<-as.factor(bankdata$pdays)
summary(bankdata)
table(bankdata$pdays,bankdata$y)
set.seed(123)
ind<-sample(nrow(bankdata),0.75*nrow(bankdata))
train_bank<-bankdata[ind,]
test_bank<-bankdata[-ind,]
bin_model<-glm(y~.,data=train_bank,family="binomial")
summary(bin_model)
# stepwise regression
null_bankmodel<-glm(y~1,data=train_bank,family=binomial)
# forward step regression- limited no of variable
step(null_bankmodel,direction = "forward",scope=list(lower=null_bankmodel,upper=bin_model))
# finalised model after forward regression
bankmodel_step<-glm(formula = y ~ duration + month + poutcome + contact + housing + job + marital + 
                      loan + education + campaign + day + balance + previous,family="binomial",data=train_bank)
# generating probability
train_bank$pred_prob<-predict(bankmodel_step,train_bank,type="response")
head(train_bank)
library(ROCR)
pred<-prediction(train_bank$pred_prob,train_bank$y)
perf<-performance(pred,"tpr","fpr")
auc<-performance(pred,"auc")
auc@y.values
plot(perf,colorize=T,print.cutoffs.at=seq(0.1,by=0.05))
pred_y<-ifelse(train_bank$pred_prob>0.15,"yes","no")
table(predicted=pred_y,actual=train_bank$y)
(23547+2908)/32217 #accuracy
2908/(2908+864) # sensitivity
23547/(23458+4898) # specificity
# Decision Trees
library(rpart)
library(rpart.plot)
library(randomForest)
train_bank$pred_prob<-NULL
tree_bank<-rpart(y~.,data=train_bank)
rpart.plot(tree_bank)
pred_tree<-predict(tree_bank,train_bank,type="class")
table(predicted=pred_tree,actual=train_bank$y)
(28216+812)/32217 # accuracy
812/(812+2960)  # sensitivity
28126/(28126+319) # specifivity
# random forest
rfbank_model<-randomForest(y~.,data=train_bank,importance=T,ntree=150)
plot(rfbank_model)
t<-table(train_bank$job,train_bank$y)
prop.table(t)
