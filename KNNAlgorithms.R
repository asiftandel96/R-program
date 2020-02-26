cancer_binary_data<-read.csv(file.choose(),header = T)
# KNN : K Nearest Neighbour
# supervised learning
# classification algorithm
# dependent var: cat[n levels]
# independent var cont
# adv: simple, no statistical concept
# disadv:complexity w.r.t running time of algorithm
#keyword: Maximum voting Concept
#KNNRegression:dependent continous,independent var:continous

str(cancer_binary_data)
cancer_binary_data$X<-NULL
cancer_binary_data$id<-NULL
#1. divide data into two parts[70:30] without dependent variables
#2. generate vectors for two data parts of only dependent variables
# training_Data,testing_data, training_diag, testing_test
cancer1<-cancer_binary_data[,-1]
str(cancer1)
set.seed(123)
ind<-sample(nrow(cancer1),0.75*nrow(cancer1))
training_cancer<-cancer1[ind,]
testing_cancer<-cancer1[-ind,]
dim(testing_cancer)
dim(training_cancer)
ytrain<-cancer_binary_data$diagnosis[ind] # vector of only dependent variables
ytest<-cancer_binary_data$diagnosis[-ind]
length(ytest)
length(ytrain)
library(class)

knn_cancer_model<-knn(training_cancer,testing_cancer,k=sqrt(nrow(training_cancer)),cl=ytrain)
str(cancer1)
str(training_cancer)
table(predict=knn_cancer_model,actual=ytest)
(78+57)/153 # accuracy
78/(78+2) # sensitivity
57/(57+6) # specificity
sonar_data <- read.csv("C:/Users/asif/Downloads/sonar.all-data.csv", header=F)
str(sonar_data)
set.seed(123)
sonar_data1<-sonar_data[,-61]
str(sonar_data1)
ind<-sample(nrow(sonar_data1),0.75*nrow(sonar_data1))
training_solar<-sonar_data1[ind,]
str(training_solar)
testing_solar<-sonar_data1[-ind,]
str(testing_solar)
dim(testing_solar)
dim(training_solar)
ytrain<-sonar_data$V61[ind]# making dependent variable as vectors
ytest<-sonar_data$V61[-ind]
length(ytest)
length(ytrain)
library(class)
knn_solar_model<-knn(training_solar,testing_solar,k=sqrt(nrow(training_solar)),cl=ytrain)
table(predict=knn_solar_model,actual=ytest)
(25+12)/50 #accuracy
25/(25+6)# sensitivity
12/(12+9) # specificity
# knn regression
#dependent var: numeric
#independent var : numeric

bostandata<-read.csv("bostondata.csv",header=T)
str(bostandata)
bostandata$X<-NULL
names(bostandata)
ind<-createDataPartition(bostandata$crim,p=0.75,list = F)
boston1<-bostandata[,-14]

train_boston<-boston1[ind,]
test_boston<-boston1[-ind,]

ytrain<-bostandata$medv[ind]
ytest<-bostandata$medv[-ind]

knn_boston_model<-knnreg(train_boston,ytrain,k=20)
predict(knn_boston_model,test_boston)

# lm model
# generate linear model on training data
train_boston$mdev<-ytrain
test_boston$mdev<-ytest
head(train_boston)
head(test_boston)
linear_boston<-lm(mdev~.,data=train_boston)
test_boston_model<-predict(linear_boston,test_boston)
RMSE(test_boston_pred_lm,test_boston$mdev)
#diabetes dataset
diabetes_data<-read.csv("C:/Users/asif/Downloads/diabetes.csv")
str(diabetes_data)
summary(diabetes_data)
diabetes_data$Outcome<-as.factor(diabetes_data$Outcome)
diabetes_data$Glucose[diabetes_data$Glucose==0]<-median(diabetes_data$Glucose)
diabetes_data$BloodPressure[diabetes_data$BloodPressure==0]<-median(diabetes_data$BloodPressure)
diabetes_data$Insulin[diabetes_data$Insulin==0]<-median(diabetes_data$Insulin)
diabetes_data$BMI[diabetes_data$BMI==0]<-median(diabetes_data$BMI)
diabetes_data$SkinThickness[diabetes_data$SkinThickness==0]<-median(diabetes_data$SkinThickness)
set.seed(123)
ind<-sample(nrow(diabetes_data),0.75*nrow(diabetes_data))
train_diabetesbi_data<-diabetes_data[ind,]
test_diabetesbi_data<-diabetes_data[-ind,]
train_knn_diab<-diabetes_data[ind,-9]#removing outcome column as it dependent factor variable
test_knn_diab<-diabetes_data[-ind,-9]# removing outcome column as its dependent factor variable
ytrain_diab<-diabetes_data$Outcome[ind]# vector of outcome values associated with train_knn_diab
ytest_diab<-diabetes_data$Outcome[-ind]# vector of outcome values associated with test_knn_diab

#glm
binary_diab_model<-glm(Outcome~.,data=train_diabetesbi_data,family = "binomial")
summary(binary_diab_model)
#glm using only signifiacant var
binary_diab_model_rev<-glm(Outcome~Pregnancies+Glucose+BloodPressure+BMI+DiabetesPedigreeFunction,data=train_diabetesbi_data,family="binomial")
# generate the probabilities on testing data
test_diabetesbi_data$pred_prob<-predict(binary_diab_model_rev,test_diabetesbi_data,type="response")
# generate cutoff using ROCR
library(ROCR)
train_diabetesbi_data$pred_prob<-predict(binary_diab_model_rev,train_diabetesbi_data,type="response")
pred<-prediction(train_diabetesbi_data$pred_prob,train_diabetesbi_data$Outcome)
perf<-performance(pred,"tpr","fpr")
plot(perf,colorize=T,print.cutoffs.at=seq(0.1,by=0.1))
# from graph 0.3 is the best cutoff
auc_binary<-performance(pred,"auc")
auc_binary@y.values
test_diabetesbi_data$pred_outcome<-ifelse(test_diabetesbi_data$pred_prob>0.3,"1","0")
table(predicted=test_diabetesbi_data$pred_outcome,actual=test_diabetesbi_data$Outcome)
acc<-(95+49)/192
se1<-49/(18+49)
se2<-95/(30+95)
# knn model
library(class)
knn_diab_model<-knn(train_knn_diab,test_knn_diab,cl=ytrain_diab,k=sqrt(nrow(train_knn_diab)))
table(predicted=knn_diab_model,actual=ytest_diab)
acc<-(109+27)/192
se1<-27/(40+27)
se2<-109/(16+109)
# K Means  Clustering
cancer_kmeans<-read.csv(file.choose(),header = T)
str(cancer_kmeans)
cancer_kmeans$X<-NULL
cancer_kmeans$id<-NULL
cancer_kmeans1<-cancer_kmeans
cancer_kmeans1$diagnosis<-NULL
str(cancer_kmeans1)
cancer3<-normalise_value(cancer_kmeans1)
summary(cancer3)
kmeans_cancer_model<-kmeans(cancer3,3)
kmeans_cancer_model$betweenss
kmeans_cancer_model$tot.withinss
bss_cancer<-numeric()
wss_cancer<-numeric()
for(i in 1:10){
  #for each k calculate between ss and total ss
  bss_cancer[i]<-kmeans(cancer3,center=i)$betweenss
  wss_cancer[i]<-kmeans(cancer3,center=i)$tot.withinss
}
library(ggplot2)
#Total within-cluster sum of squaresvs choice of k
p1_cancer<-qplot(1:10,wss_cancer,geom=c("point","line"),
          xlab="Number of clusters", ylab="Total within-cluster sum of squares")+
  scale_x_continuous(breaks=seq(0,10,1)) + scale_color_viridis(discrete = TRUE, option = "D")+
  scale_fill_viridis(discrete = TRUE) +
  theme_minimal() +
  theme(legend.position = "bottom")

p2_cancer<-qplot(1:10,bss_cancer,geom=c("point","line"),
          xlab="Number of clusters", ylab="within-cluster sum of squares")+
  scale_x_continuous(breaks=seq(0,10,1)) + scale_color_brewer(palette = "Dark2") +
  theme_bw()         
library(grid)
library(gridExtra)
grid.arrange(p1_cancer,p2_cancer,ncol=2)
install.packages("viridis")
library(viridis)
library(viridisLite)
