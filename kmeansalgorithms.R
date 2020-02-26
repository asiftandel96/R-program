wine<- read.csv("C:/Users/asif/Downloads/winequality.csv",header=T)
str(wine)
summary(wine)
wine1<-wine[-c(12)]
names(wine1)
str(wine1)
wine1$color<-ifelse(wine1$color=="red",1,0)
# user defined functions
normalise_value<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
mean(wine1$fixed.acidity)
area_circle<-function(r){
  return(3.14*r*r)
}
rad<-c(1:10)
area_circle(rad)
area_rectangle<-function(l,b){
  return(l*b)
}
length<-c(1)
breadth<-c(3)
area_rectangle(length,breadth)
wine1$fixed.acidity<-normalise_value(wine1$fixed.acidity)
summary(wine1$fixed.acidity)
summary(wine$fixed.acidity)
wine3<-normalise_value(wine1) # dataset normalization-scaling down to 0 and 1
summary(wine3)
floor(sqrt(ncol(wine3)))
# Model Building in K means Algorithms
kmeans_model<-kmeans(wine3,3)
wine3$cl<-kmeans_model$cluster
head(wine3)
kmeans_model$betweenss
kmeans_model$tot.withinss
bss<-numeric()
wss<-numeric()
for(i in 1:10){
  #for each k calculate between ss and total ss
  bss[i]<-kmeans(wine3,center=i)$betweenss
  wss[i]<-kmeans(wine3,center=i)$tot.withinss
}
library(ggplot2)
#Total within-cluster sum of squaresvs choice of k
p4<-qplot(1:10,wss,geom=c("point","line"),
          xlab="Number of clusters", ylab="Total within-cluster sum of squares")
          scale_x_continuous(breaks=seq(0,10,1)) +
          theme_bw()
p3<-qplot(1:10,bss,geom=c("point","line"),
          xlab="Number of clusters", ylab="within-cluster sum of squares")+
          scale_x_continuous(breaks=seq(0,10,1)) +
          theme_bw()         
library(grid)
install.packages("gridExtra")
library(gridExtra)
grid.arrange(p3,p4,ncol=2)
Mall_Customers <- read.csv("C:/Users/asif/Downloads/Mall_Customers.csv", header=FALSE)
str(Mall_Customers)
summary(Mall_Customers)
malldata<-Mall_Customers
malldata$V2<-ifelse(malldata$V2=="male",1,0)
str(malldata)
malldata1<-normalise_value(malldata)
summary(malldata1)
kmeans_mall_model<-kmeans(malldata1,3)
kmeans_mall_model$betweenss
kmeans_mall_model$tot.withinss
kmeans_mall_model$cluster
bss_mall<-numeric()
wss_mall<-numeric()
for(i in 1:10){
  #for each k calculate between ss and total ss
  bss_mall[i]<-kmeans(malldata1,center=i)$betweenss
  wss_mall[i]<-kmeans(malldata1,center=i)$tot.withinss
}

library(ggplot2)
#Total within-cluster sum of squaresvs choice of k
p1<-qplot(1:10,wss_mall,geom=c("point","line"),
          xlab="Number of clusters", ylab="Total within-cluster sum of squares")+
scale_x_continuous(breaks=seq(0,10,1)) +
  theme_bw()
p2<-qplot(1:10,bss_mall,geom=c("point","line"),
          xlab="Number of clusters", ylab="within-cluster sum of squares")+
  scale_x_continuous(breaks=seq(0,10,1)) +
  theme_bw()         
library(grid)
library(gridExtra)
grid.arrange(p1,p2,ncol=2)
# colorbrewer packages
install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all()
display.brewer.pal(n = 8, name = 'Dark2')
barplot(1:5, col=rainbow(5))
scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))
