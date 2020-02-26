cancer_pratice_data<-read.csv("C:/Users/asif/Downloads/data.csv",header=T)
str(cancer_pratice_data)
cancer_pratice_data$X<-NULL
cancer_pratice_data$id<-NULL
cancer_pratice<-cancer_pratice_data[,-1]
str(cancer_pratice)
set.seed(123)
ind<-sample(nrow(cancer_pratice),0.75*nrow(cancer_pratice))
training_cancer_pratice<-cancer_pratice[ind,]
testing_cancer_pratice<-cancer_pratice[-ind,]
str(training_cancer_pratice)
str(testing_cancer_pratice)
dim(training_cancer_pratice)
dim(testing_cancer_pratice)
ytrain<-cancer_pratice_data$diagnosis[ind]
ytest<-cancer_pratice_data$diagnosis[-ind]
length(ytrain)
length(ytest)
library(class)
knn_love_model<-knn(training_cancer_pratice,testing_cancer_pratice,k=sqrt(nrow(training_cancer_pratice)),cl=ytrain)
table(predict=knn_love_model,actual=ytest)
(78+57)/143 # accuracy
78/(78+2) # sensitivity
57/(57+6) # specificity
# sonar data
sonar_pratice_data<-read.csv("C:/Users/asif/Downloads/sonar.all-data.csv", header=FALSE)
str(sonar_pratice_data)
sonar_pratice<-sonar_pratice_data[,-61]
str(sonar_pratice)
set.seed(123)
ind<-sample(nrow(sonar_pratice),0.75*nrow(sonar_pratice))
training_solar_p<-sonar_pratice[ind,]
str(training_solar_p)
str(training_solar_p)
testing_solar_p<-sonar_pratice[-ind,]
dim(training_solar_p)
dim(testing_solar_p)
ytrain_s<-sonar_pratice_data$V61[ind]
ytest_s<-sonar_pratice_data$V61[-ind]
length(ytrain_s)
length(ytest_s)
library(class)
knn_pratice<-knn(training_solar_p,testing_solar_p,k=sqrt(nrow(training_solar_p)),cl=ytrain_s)
knn_pratice
table(predited=knn_pratice,actual=ytest_s)
(25+12)/50 # accuracy
25/(25+6) # sensitivity
12/(12+9) # specificity
