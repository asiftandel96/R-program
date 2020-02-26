titanicdata<- read.csv("C:/Users/asif/Downloads/titanic.csv",header=T)
summary(titanicdata)
str(titanicdata)
colSums(is.na(titanicdata))
titanicdata$PassengerId<-NULL
titanicdata$Pclass<-as.factor(titanicdata$Pclass)
titanicdata$Survived<-as.factor(titanicdata$Survived)
titanicdata$Ticket<-as.numeric(titanicdata$Ticket)
summary(titanicdata$Name)
View(titanicdata$Survived)
titanicdata$Age<-NULL
summary(titanicdata$Pclass)
titanicdata$Pclass[titanicdata$Pclass==1]<-"FirstClass"
sum(is.na(titanicdata$Survived))

