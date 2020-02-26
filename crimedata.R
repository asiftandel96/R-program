crime_data1<-read.csv("C:/Users/asif/Downloads/crimedatacorrected.csv",header=T)
summary(crime_data1)
colSums(is.na(crime_data1))
summary(crime_data1$rapes)
# country code,lemasswornFT,LewasSwftPerPop,lemasSwftFieldOps,Lemasswft
crime_data1$countyCode<-NULL
crime_data$communityCode<-NULL
crime_data1$LemasSwornFT<-NULL
crime_data$LemasSwFTPerPop<-NULL
crime_data1$LemasSwFTFieldOps<-NULL
crime_data$LemasSwFTFieldPerPop<-NULL
crime_data1LemasTotalReq<-NULL
crime_data$LemasTotReqPerPop<-NULL
crime_data1PolicReqPerOffic<-NULL
crime_data$PolicPerPop<-NULL
crime_data1$RacialMatchCommPol<-NULL
crime_data1$PctPolicWhite<-NULL 
crime_data1$PctPolicBlack<-NULL
crime_data1$PctPolicHisp<-NULL
crime_data1$PctPolicAsian<-NULL
crime_data1$PctPolicMinor<-NULL
crime_data1$OfficAssgnDrugUnits<-NULL
crime_data1$NumKindsDrugsSeiz<-NULL
crime_data1$PolicAveOTWorked<-NULL 
crime_data1$PolicCars<-NULL
crime_data1$PolicOperBudg<-NULL 
crime_data1$LemasGangUnitDeploy<-NULL 
crime_data1$PolicBudgPerPop<-NULL
crime_data1$LemasPctPolicOnPatr<-NULL 
summary(crime_data1)
crime_data1$LemasTotReqPerPop<-NULL 
crime_data1$PolicReqPerOffic<-NULL  
crime_data1$PolicPerPop<-NULL 
summary(crime_data1)
crime_data1$communityCode<-NULL
crime_data1$LemasSwFTPerPop<-NULL
crime_data1$LemasSwFTFieldPerPop<-NULL
summary(crime_data1)
crime_data1$LemasSwFTFieldPerPop<-NULL
crime_data1$LemasTotalReq<-NULL
summary(crime_data1)
install.packages("DMwR")
library(DMwR)
library(lattice)
library(grid)
crime_data1<-knnImputation(crime_data1[,names(crime_data1)])# perform knn imputation
colSums(is.na(crime_data1))
dim(crime_data1)
str(crime_data1)
crime_data_plot<-crime_data1[,c(4,5,6,7,8,9,10,11,12,13,14)]
dim(crime_data_plot)
library(corrplot)
mat1<-cor(num_col)
corrplot(num_col,method = "circle",type="lower")
a<-unlist(lapply(crime_data1,is.numeric)) # fetching numerical columns
b<-unlist(lapply(crime_data1,is.factor))
num_col<-crime_data1[,a]
fact_col<-crime_data1[,b]
num_col1<-num_col[c(1:20,120,121)]
mat1<-cor(num_col1)
corrplot(mat1,method="circle",type="lower")
install.packages("tidyverse")
library(tidyverse)
names(num_col)
cor_mat_try<-cor(num_col)
cor_mat_try1<-data.frame(cor_mat_try)
cor_dep_var<-cor_mat_try1[,"ViolentCrimesPerPop"]
x<-data.frame(names(num_col),cor_dep_var)
imp_var<-subset(x,x$cor_dep_var>0.3 | x$cor_dep_var< -(0.3))
imp_var
dim(imp_var)
temp<-imp_var$names.num_col.
temp<-as.character(temp)
final_crime<-subset(crime_data1,select=temp)
dim(final_crime)
str(final_crime)
names(final_crime)
null_crime_model<-lm(ViolentCrimesPerPop~1,data=final_crime)
full_crime_model<-lm(ViolentCrimesPerPop~.,data=final_crime)
summary(full_crime_model)
step(full_crime_model,direction="backward",scope=list(lower=null_crime_model,upper=full_crime_model))
step_crime_model<-lm(formula = ViolentCrimesPerPop ~ racepctblack + pctWWage + 
     pctWInvInc + pctWPubAsst + PctPopUnderPov + PctLess9thGrade + 
     PctNotHSGrad + PctBSorMore + PctUnemployed + PctOccupMgmtProf + 
     MalePctDivorce + PctFam2Par + PctKids2Par + PctYoungKids2Par + 
     PctKidsBornNeverMar + PctPersOwnOccup + PctPersDenseHous + 
     MedNumBR + murdPerPop + rapesPerPop + robbbPerPop + assaultPerPop + 
     burglaries + burglPerPop, data = final_crime)
summary(step_crime_model)
library(caret)
RMSE(step_crime_model$fitted.values,final_crime$ViolentCrimesPerPop)
View(final_crime)
write.csv(final_crime,file="final_crime2.csv")
