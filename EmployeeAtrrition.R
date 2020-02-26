
Employee_Attrition <- read.csv("C:/Users/asif/Downloads/WA_Fn-UseC_-HR-Employee-Attrition.csv", header=T)
colSums(is.na(Employee_Attrition))
dim(Employee_Attrition)
str(Employee_Attrition)
Employee_Attrition$EmployeeCount<-NULL
Employee_Attrition$Over18<-NULL
Employee_Attrition$StandardHours<-NULL
Employee_Attrition$EmployeeNumber<-NULL
Employee_Attrition$Education<-as.factor(Employee_Attrition$Education)
Employee_Attrition$EnvironmentSatisfaction<-as.factor(Employee_Attrition$EnvironmentSatisfaction)
Employee_Attrition$JobInvolvement<-as.factor(Employee_Attrition$JobInvolvement)
Employee_Attrition$JobSatisfaction<-as.factor(Employee_Attrition$JobSatisfaction)
Employee_Attrition$PerformanceRating<-as.factor(Employee_Attrition$PerformanceRating)
Employee_Attrition$WorkLifeBalance<-as.factor(Employee_Attrition$WorkLifeBalance)
Employee_Attrition$JobLevel<-as.factor(Employee_Attrition$JobLevel)
Employee_Attrition$RelationshipSatisfaction<-as.factor(Employee_Attrition$RelationshipSatisfaction)
# vect_of_factor<-c("Education","EnvironmentStatisfication")
#Employee_Atrrition[,vect_of_factor]<-lapply(Employee_Atrrition[,vect_of_factor],as.factor)
summary(Employee_Attrition$ï..Age)
# training : testing 75:25
#run the model on training data
#select only required varaibles
#regenerate the model with significant variables
#predict probabilties on training data
#get cutoff
# calculate accuracy,sensitivity, specificity
#evaluate the model on testdata
#calculate accuracy, sensitivity and specificity for the data