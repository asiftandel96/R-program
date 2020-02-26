house_price<- read.csv("C:/Users/asif/Downloads/House Price Data.csv",header = T)
str(house_price)
house_price$X<-NULL
house_price$X.1<-NULL
house_price$Houseid<-NULL
housela<-house_price[1:90,]
str(housela)
housela$X<-NULL
housela$X.1<-NULL
housela$Houseid<-NULL
house_model<-lm(Price~.,data=housela)
summary(house_model)# all var are significant
# using above model,predict sales for house id's 91-99[test] and obtain Root Mean Squared Error
house2<-house_price[91:99,]
house2$pred_price<-predict(house_model,house2)
rmse_train<-sqrt(mean(house_model$residuals^2))
rmse_test<-sqrt(mean(house2$price-house2$pred_price)^2)
house3<-normalise_value(house2)
summary(house3)
house1_st_lm<-lm(Price~.,data=house3)
summary(house1_st_lm)
"Define Price_bin=1 if Price>median(Price): else Price_bin=0 Obtain area under ROC
curve by modelling Price_bin.
Write R code and are under ROC curve as your answer."
housela$Price_bin<-ifelse(housela$Price<-median(housela$Price),1,0)
head(housela)
housela$Price<-NULL
library(ROCR)
Price_model<-glm(Price_bin~.,data=housela,family = "binomial")
prob<-predict(Price_model,housela,type="response")
str(housela)
housela$Price_bin<-as.factor(housela$Price_bin)
pred<-prediction(prob,housela$Price_bin)
perf<-performance()
