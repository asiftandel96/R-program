# Basic Implementation Of R for Data Analysis
# We are primarily focusing in this session on list,matrix,some statistical function such as anova,normality
#and other as follows 
#list : one dimensional : hetrogeneous
list1<-list("data",12,12L,4+7i,F)

vect<-c("data",12,12L,4+7i,F)
vect
class(vect)
list1
class(list1)

# matrix : two dim.
vect1<-c(12,23,45,13,24,36,76,87,98)
vect1
mat<-matrix(1:9,nrow = 3,ncol = 3)
mat
mat1<-matrix(vect1,nrow = 3,ncol = 3)
mat1
mat2<-matrix(vect1,nrow = 3,ncol = 3,byrow = T)
mat2
mat3<-matrix(vect1,nrow = 3,ncol = 4,byrow = T)
mat3
mat4<-matrix(vect1,nrow = 2,ncol = 2,byrow = T)
mat4
class(mat)
t(mat4)

# array

array1<-array(1:9,dim=c(3,3,1))
array1
array2<-array(1:9,dim=c(3,3,2)) 
array2
array2<-array(1:13,dim=c(4,4,2))
array2
array3<-array(1:17,dim=c(2,2,2))
array3
array3[1,1,1]
array3[,1,]
array2[,,] # Doubt
array3[1,,]# Doubt

#hypothesis testing 
#Ho : null hypothesis
#H1 : alternative hypothesis
#test of hypothesis
#confidence interval 
#level of significance [alpha{ prob of type1 err}]
#types of error [ I , II]
#p value [R/python / excel]


# type 1 : rejecting Ho when in reality Ho is true
# type 2 : accepting Ho when in reality Ho is false 

# t-test family 
# data is normally destributed 
# there is no variance inside data 

# t test for single sample / student's t test 

onesample<-read.csv(file.choose(),header = T)
onesample
#ho: avg time to complete MIS report is 90
#h1: avg time to complete MIS report is more than 90

#h1: avg time to complete MIS report is less thean 90
#h1: avg time to complete MIS report is not equal  90

# t statistics 
# formlula = mean(x)- mu /sd(x)*sqrt(n)
t.test(onesample$Time,alternative = "greater",mu=90)
t.test(onesample$Time,alternative = "less",mu=90)
t.test(onesample$Time,alternative = "two.sided",mu=90)

# independent sample t test
# comparison of two means of two samples  

INDEPENDENTSAMPLE<-read.csv("C:/Users/asif/Downloads/PGDDS DAY9 ACTIVITY DATA SETS/INDEPENDENT SAMPLES t TEST.csv", header=T)
INDEPENDENTSAMPLE

#ho: avg time taken to complete MIS report by gr1and gr2 is same 
#h1: avg time taken to complete MIS report by gr1 and gr2 is not same
# both samples are normal 
# both sample have similar variance 
t.test(INDEPENDENTSAMPLE$time_g1,INDEPENDENTSAMPLE$time_g2,alternative ="two.sided")
# wide data # Doubt

independentsample1<-read.csv("C:/Users/asif/Downloads/PGDDS DAY9 ACTIVITY DATA SETS/INDEPENDENT SAMPLES t TEST_2.csv",header = T)
independentsample1
# long data 
t.test(time ~ group,data = independentsample1,alternative="two.sided")
t.test(independentsample1$time ~ independentsample1$group,alternative="two.sided")

# t test for dependent sample/ paired t test 
paired<-read.csv("C:/Users/asif/Downloads/PGDDS DAY9 ACTIVITY DATA SETS/PAIRED t TEST.csv",header = T)
paired
# avg time to complete MIS before and after training is same 
# difference in time taken to complete MIS report b4 and after training is 0
# difference in time taken to complete MIS report b4 and after is greater 
t.test(paired$time_before,paired$time_after,alternative = "greater",paired = T)

# f-test : test of variance 
#ho: variance in two sample is similar 
#h1: variance in two sample is not similar
#f stat= sd(sample1)^2/sd(sample2)^2
# doubt
var.test(INDEPENDENTSAMPLE$time_g1,INDEPENDENTSAMPLE$time_g2)# cor test 
# ho: columns are NOT CORRELATED
# h1: columns are CORRELATED
jobproficiancy<-read.csv("C:/Users/asif/Downloads/PGDDS DAY9 ACTIVITY DATA SETS/JOBPROF.csv",header = T)
jobproficiancy
cor.test(jobproficiancy$aptitude,jobproficiancy$job_prof)
#anova test
# one way anova test,two way anova,multi-anova
one_way<-read.csv("C:/Users/asif/Downloads/One way anova.csv",header=T)
head(one_way)
summary(one_way)
#ho avg satisfication index of emp in all departments is same
#h1:!ho
#one factor and 2 or more than 2 levels
# anova for comparison
#is always implemented on LONG formatted data
# @reshape2
# aov<- to compare data
# anova<- to compare model
install.packages("reshape2")
library(reshape2)
oneway<-aov(satindex~dept,data=one_way)
summary(oneway)
# two way anova-Two Factors
#two factor with every can have n number of levels
twoway<-rea0d.csv("C:/Users/asif/Downloads/Two way anova.csv",header=T)
summary(twoway)
#ho avg satisfication index for all departments is similar
#ho avg satisfication index for  all experience is similar
#h1: !ho
twoway<-aov(satindex~dept+exp,data=twoway)
summary(twoway)
# Linear Regression
# dept var must be numeric
# ind var can be num/factor and not strong relation
# data should not vary with time component # test of autocorelation
#-normally distributed and continous and no relation in independent variable weak relation is acceptable
performanceindex<-read.csv("C:/Users/asif/Downloads/jpi.csv",header=T)
summary(performanceindex)
performanceindex$X<-NULL
performanceindex$X.1<-NULL
performanceindex$X.2<-NULL
head(performanceindex)
performanceindex$empid<-NULL
# Equation of linear regression
pairs(~.,data=performanceindex)
install.packages("corrplot")
library(corrplot)
mat<-cor(performanceindex)
mat
corrplot(mat,method="circle",type="lower")
#model generation
jpimodel<-lm(jpi~written+language+tech+gk,data=performanceindex)
jpimodel<-lm(jpi~.,data=performanceindex)
summary(jpimodel)
# F-statistic: 49.81 on 4 and 28 DF, p-value247e-12: global test
null_jpi<-lm(jpi~1,data=performanceindex) # Syntax of null model
#ho : avg of pred jpi is same for null model an dfor full model
#h1!=ho
anova(null_jpi,jpimodel) 
# non parametric htpothesis test
#post hoc anova
# basic of ggplot2
#normality test :shapiro test
# ho:data is normally distributed
# h1:data is not normally distributed
shapiro.test(performanceindex$jpi)
boxplot(performanceindex$jpi)
install.packages("e1071") # skewness packages
library(e1071)
skewness(performanceindex$jpi)
# local test
# prediction using model formula
#predict(modelname)
#fitted(modelname)
performanceindex$pred_jpi<-predict(jpimodel)
head(performanceindex)
performanceindex$pred_jpi_fitted<-fitted(jpimodel)
performanceindex$residules_jpi<-residuals(jpimodel)# residuals
sum(performanceindex$residules_jpi) # sum of res is 0
shapiro.test(performanceindex$residules_jpi)# res are normally distributed
head(performanceindex)
head(performanceindex)
jpimodel$coefficients
jpimodel$fitted.values

# fitted work for model is generated,pred work for static and dyanmic data
# multi-colinearity using VIF
install.packages("car")
library(car)
library(carData)
vif(jpimodel) # multi-colinearity vif(name_of_model)
# wr:1.17 lag 1.32 tech 2.07 gk 2.02
# the value of vif for every column should not exceed 5
# test of autocorelation :to decide linear model vs time series
# ho:data is not autocorelated
# h1:data is autocorelated
durbinWatsonTest(jpimodel)
# p>0.06 : Accept ho,no auto


