cylinder_band<-read.csv("C:/Users/asif/Downloads/bands.data", header=FALSE)
summary(cylinder_band)
dim(cylinder_band)
str(cylinder_band)
colSums(is.na(cylinder_band))
summary(cylinder_band$V5)
cylinder_band$V2<-NULL
cylinder_band$V5[cylinder_band$V5=="?"]<-"45"
cylinder_band$V7[cylinder_band$V7=="?"]<-"17"
cylinder_band$V8[cylinder_band$V8=="?"]<-"84"
summary(cylinder_band$V8)
cylinder_band$V12[cylinder_band$V12=="?"]<-"1865"
summary(cylinder_band$V12)
summary(cylinder_band$V20)
cylinder_band$V20[cylinder_band$V20=="?"]<-"40"
summary(cylinder_band$V20)
dim(cylinder_band)
