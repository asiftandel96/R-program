install.packages("arules")
install.packages("arulesViz")
library(arules)
library(arulesViz)
data("Groceries")
str(Groceries)
class(Groceries)
a<-Groceries@itemInfo
head(a)
Groceries@itemsetInfo
Groceries@data@p

itemFrequencyPlot(Groceries,topN=20,type="absolute")
itemFrequencyPlot(Groceries,topN=20,type="relative",col="red")

rules<-apriori(Groceries,parameter = list(supp=0.001,conf=0.8))
inspect(rules[1:5])

rules1<-sort(rules,by="support",decreasing = T) # Sorting
inspect(rules1[1:5])

rules2<-sort(rules,by="confidence",decreasing = T)
inspect(rules2[1:5])

rules3<-sort(rules,by="lift",decreasing = T)
inspect(rules3[1:5])

rules4<-apriori(Groceries,parameter = list(supp=0.001,conf=0.8,minlen=2),
                appearance = list(default="lhs",rhs="whole milk"))


rules5<-apriori(Groceries,parameter = list(supp=0.001,conf=0.1,minlen=2),
                appearance = list(default="rhs",lhs="whole milk"))

rules6<-apriori(Groceries,parameter = list(supp=0.001,conf=0.1,minlen=2),
                appearance = list(default="rhs",lhs="whole milk"),control = list(verbose=F))


inspect(rules5[1:10])


# breadbasket data
trans <- read.transactions("C:/Users/asif/Downloads/BreadBasket.csv", format="single",
                           cols=c(3,4), sep=",", rm.duplicates=TRUE)
trans_csv <- read.csv("C:/Users/asif/Downloads/BreadBasket.csv")

class(trans)
class(trans_csv)

library(ggplot2)
# Absolute Item Frequency Plot
dev.new()
itemFrequencyPlot(trans, topN=15, type="absolute")
dev.new()
itemFrequencyPlot(trans, topN=15, type="relative")

# Load data 
trans_csv <- read.csv("C:/Users/asif/Downloads/BreadBasket.csv")
head(trans_csv)
install.packages("lubridate")
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(arules)
library(arulesViz)

# Transactions per month
trans_csv %>%
  mutate(Month=as.factor(month(Date))) %>%
  group_by(Month) %>%
  summarise(Transactions=n_distinct(Transaction)) %>%
  ggplot(aes(x=Month, y=Transactions)) +
  geom_bar(stat="identity", fill="red", colour="black") +
  geom_label(aes(label=Transactions)) +
  labs(title="Transactions per month") +
  theme_bw() 

# Transactions per hour
trans_csv %>%
  mutate(Hour=as.factor(hour(hms(Time)))) %>%
  group_by(Hour) %>%
  summarise(Transactions=n_distinct(Transaction)) %>%
  ggplot(aes(x=Hour, y=Transactions)) +
  geom_bar(stat="identity", fill="blue",colour="black") +
  geom_label(aes(label=Transactions)) +
  labs(title="Transactions per hour") +
  theme_bw()

# Transactions per weekday
trans_csv %>%
  mutate(WeekDay=as.factor(weekdays(as.Date(Date)))) %>%
  group_by(WeekDay) %>%
  summarise(Transactions=n_distinct(Transaction)) %>%
  ggplot(aes(x=WeekDay, y=Transactions)) +
  geom_bar(stat="identity", fill="pink", 
           colour="black") +
  geom_label(aes(label=Transactions)) +
  labs(title="Transactions per weekday") +
  scale_x_discrete(limits=c("Monday", "Tuesday", "Wednesday", "Thursday",
                            "Friday", "Saturday", "Sunday")) +
  theme_bw()
# Support,confidence,Lift- 3 important thing of Market Basket Analysis
# Support and confidence values
supportLevels<-c(0.1,0.05,0.01,0.005,0.001)
confidenceLevels<-c(0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1)
# Empty Integers
rules_sup10<-integer(length=9)
rules_sup5<-integer(length=9)
rules_sup1<-integer(length=9)
rules_sup0.5<-integer(length=9)
rules_sup0.1<-integer(length=9)
# Apriori algorithm with a support level of 10%
for(i in 1:length(confidenceLevels)){
  rules_sup10[i]<-length(apriori(trans,parameter = list(sup=supportLevels[1],
                                 conf=confidenceLevels[i],target="rules")))
}
for(i in 1:length(confidenceLevels)){
  rules_sup5[i]<-length(apriori(trans,parameter = list(sup=supportLevels[2],
                                                        conf=confidenceLevels[i],target="rules")))
}
for(i in 1:length(confidenceLevels)){
  rules_sup1[i]<-length(apriori(trans,parameter = list(sup=supportLevels[3],
                                                        conf=confidenceLevels[i],target="rules")))
}
for(i in 1:length(confidenceLevels)){
  rules_sup0.5[i]<-length(apriori(trans,parameter = list(sup=supportLevels[4],
                                                        conf=confidenceLevels[i],target="rules")))
}
for(i in 1:length(confidenceLevels)){
  rules_sup0.1[i]<-length(apriori(trans,parameter = list(sup=supportLevels[5],
                                                        conf=confidenceLevels[i],target="rules")))
}

rules_sup1_conf50<-apriori(trans,parameter = list(sup=supportLevels[3],
                                                       conf=confidenceLevels[5],target="rules"))
inspect(rules_sup1_conf50)
# scatter plot
plot(rules_sup1_conf50, measure=c("support","lift"),shading="confidence")
# graph
plot(rules_sup1_conf50,method="graph")
# grouped matrix plot
plot(rules_sup1_conf50,method="grouped")
