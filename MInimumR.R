Telecom_CustDemo_Data <- read.csv("C:/Users/asif/Downloads/Telecom_CustDemo.csv",header = T)
str(Telecom_CustDemo_Data)
Telecom_WeeklyData <- read.csv("C:/Users/asif/Downloads/Telecom_WeeklyData.csv",header=T)
str(Telecom_WeeklyData)
dim(Telecom_CustDemo_Data)
dim(Telecom_WeeklyData)
head(Telecom_CustDemo_Data)
head(Telecom_WeeklyData)
tail(Telecom_CustDemo_Data)
tail(Telecom_WeeklyData)
summary(Telecom_CustDemo_Data)
summary(Telecom_WeeklyData$Minutes)
Telecom_WeeklyData$Minutes<-Telecom_WeeklyData$Minutes/60
Telecom_WeeklyData$Minutes<-NULL
write.csv(Telecom_WeeklyData,file="Telecom.csv")
head(Telecom_CustDemo_Data$Gender)
# find usage(total mimutes) over 24 weeks for each customer
head(Telecom_WeeklyData,25)
usage<-aggregate(Minutes~CustID,data=Telecom_WeeklyData,FUN=sum)
# create a subset of Gender=Female and Age<=30
Telecom8<-subset(Telecom_CustDemo_Data,Telecom_CustDemo_Data$Gender=="F" & Telecom_CustDemo_Data$Age<=30)
head(Telecom8)
Telecom_CustDemo_Data$Age<-cut(Telecom_CustDemo_Data$Age,breaks = c(0,30,45,Inf),labels = c("cat1","cat2","cat3"))
#Find total usage for each age group
total_usage<-merge(usage,Telecom_CustDemo_Data,by="CustID")
Telecom_CustDemo_Data$Age<-as.numeric(Telecom_CustDemo_Data$Age)
usagegroup<-aggregate(Minutes~Age,data=total_usage,FUN=sum)
sub2<-merge(usage,Telecom8,by="CustID")
head(sub2)
sub3<-sub2[,c(1:4)]
head(sub3)
working<-sub3[order(sub3$Minutes),]
head(working)
bottom5<-head(working,5)
top5<-tail(working,5)
working1<-sub3[order((sub3$Minutes),decreasing = T),]
head(working1)
top5_new<-head(working1)
bottom5_new<-tail(working1)
# Customer Data
Consumer_gender <- read.csv("C:/Users/asif/Downloads/Consumer Pref Gender.csv",header = T)
 Consumer_Responses <- read.csv("C:/Users/asif/Downloads/Consumer Pref Responses.csv",header=T)
head(Consumer_gender)
head(Consumer_Responses)
lookfeel<-Consumer_Responses$Color+Consumer_Responses$Weight+Consumer_Responses$shape
tech<-Consumer_Responses$Camera+Consumer_Responses$Ram+Consumer_Responses$Processor+Consumer_Responses$Internet
mean(lookfeel)
median(tech)
mean(tech)
t.test(lookfeel,alternative = "greater",mu=9)
t.test(tech,alternative = "greater",mu=12)
Consumer_Responses$lookfeel<-Consumer_Responses$Color+Consumer_Responses$Weight+Consumer_Responses$shape
Consumer_Responses$tech<-Consumer_Responses$Camera+Consumer_Responses$Ram+Consumer_Responses$Processor+Consumer_Responses$Internet
head(Consumer_Responses)
Consumer_Responses$gender<-Consumer_gender$Gender
head(Consumer_Responses)
t.test(lookfeel~gender,data=Consumer_Responses,alternative="two.sided",var.equal=T)
t.test(tech~gender,data=Consumer_Responses,alternative="two.sided",var.equal=T)
sub1<-subset(Consumer_Responses,Consumer_Responses$gender=="M",select = c(lookfeel,tech))
sub2<-subset(Consumer_Responses,Consumer_Responses$gender=="F",select = c(lookfeel,tech))
t.test(sub1$lookfeel,sub2$tech)
cor(lookfeel,tech)
# what percentage of respondents have color rating less than or equal to 3
Col_rating<-ifelse(Consumer_Responses$Color<=3,0,1)
a<-table(Col_rating)
prop.table(a)
str(Consumer_Responses)
