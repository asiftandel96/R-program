
computer<-read.csv("C:/Users/asif/Downloads/Computers.csv",header=T)
dim(computer)
str(computer)
computer$X<-NULL
normalise_value1<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
mean(computer$price)
computer$screen<-normalise_value1(computer$screen)
computer$ads<-normalise_value1(computer$ads)
computer$trend<-normalise_value1(computer$trend)# dataset normalization-scaling down to 0 and 1
summary(computer)
colSums(is.na(computer))
vect4<-c("cd","multi","premium")
computer$cd<-as.numeric(computer$cd)
computer$premium<-as.numeric(computer$premium)
computer$multi<-as.numeric(computer$multi)
computer_data<-computer
computer_pr<-normalise_value(computer_data)
summary(computer_pr)
# Model Building in K means Algorithms
kmeans_model<-kmeans(computer,2)
computer_pr$cl<-kmeans_model$cluster
head(computer_pr)
kmeans_model$betweenss
kmeans_model$tot.withinss
bss<-numeric()
wss<-numeric()
for(i in 1:10){
  #for each k calculate between ss and total ss
  bss[i]<-kmeans(computer_pr,center=i)$betweenss
  wss[i]<-kmeans(computer_pr,center=i)$tot.withinss
}
library(ggplot2)
#Total within-cluster sum of squaresvs choice of k
p4_computer<-qplot(1:10,wss,geom=c("point","line"),
          xlab="Number of clusters", ylab="Total within-cluster sum of squares")
scale_x_continuous(breaks=seq(0,10,1)) +
  theme_bw()
p4_computer
p3_computer<-qplot(1:10,bss,geom=c("point","line"),
          xlab="Number of clusters", ylab="within-cluster sum of squares")+
  scale_x_continuous(breaks=seq(0,10,1)) +
  theme_bw() 
p3_computer
