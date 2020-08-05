##########################################
#R code for MBA analysis
#Last revised July 2020
setwd("/Users/shelleyxiang/Documents/MBA last spring term 第三学期/Marketing Big Data")

rm(list=ls())
install.packages(c("lubridate","arules","arulesViz","tibble"))
Load required packages
library(arules)
library(ggplot2)
library(arulesViz)
library(xlsx)
library(tibble)
library(dplyr)
library(lubridate)

transData=data.table::fread("/Users/shelleyxiang/Documents/MBA last spring term 第三学期/Marketing Big Data/BlackFriday.csv",header=T)
str(transData)

#data info can be found here
#variable description

#User_ID: User number. Nominal, a 7-digit integral number uniquely assigned to each customer.
# Product_ID#Gender#Age#Occupation#City_Category#Stay_In_Curretn_City_Years#Marital_Status#Product_Category_1,Product_Category_2,Product_Category_3#Purchase
head(transData)
colnames(transData)
str(transData)


###Part 1: Plots the transaction details####

#Bar plot of top-ten best sellers 
bestseller <-transData %>% 
  group_by(Product_ID) %>% 
  summarize(count = n()) %>%
  arrange(desc(count))%>%top_n(10)%>%

ggplot(aes(x=reorder(Product_ID,count),y=count))+ ggtitle("Top 10 Best Sellers")+ 
  geom_bar(stat="identity",fill="indianred")+
  coord_flip()
bestseller

###Part 2: Market basket analysis/association rules mining####

list.tr=split(transData$Product_ID, transData$User_ID)##using split to creat a list of basket. always product first)
list.tr[1]#take a look

tr=as(list.tr,"transactions")

inspect(tr[1:5])

itms <- itemFrequency(tr, type = "absolute")#list the most frequently purchased. 
head(sort(itms, decreasing = TRUE), n = 10)#list top ten 

itemFrequencyPlot(tr, topN=10, type='absolute',col="sandybrown",main="Most Popular Items")#Plot top 10 popular merchandise. Similar to the bar plot above

#Generate association rules

#rules = arules::apriori (tr, parameter = list(supp = 0.001, conf = 0.8,maxlen=10)) # Min Support as 0.001, confidence set as 0.8. When maxlen increases, the No. rules decreased. 
length(rules)

rules = arules::apriori (tr, parameter = list(supp = 0.003, conf = 0.8,maxlen=10))
length(rules)

#Sort rules by confidence
head(sort (rules, by="confidence", decreasing=TRUE),3)
inspect(head(sort (rules, by="confidence", decreasing=TRUE),3))
rules_conf = sort (rules, by="confidence", decreasing=TRUE) # create a list of 'high-confidence' rules.

inspect(head(rules_conf,10)) 
inspect(rules_conf[8:10])
summary(rules_conf[8:10])#provide summary of rules

# create a list of 'high-lift' rules.
rules_lift = sort (rules, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(rules_lift[1:3]) # show the top 3 rules in lift, with support, lift and confidence values

dev.off()

###Part 3: Rules plots==========
library(arulesViz)

plot(rules[1:1000], measure="support",jitter=2,method="scatterplot",shading="lift",main="Scatterplot for Association Rules")#plot of all rules in the study. jitter makes the plot look more "random" by adding some noise

plot(rules[1:1000], measure="lift",jitter=2,method="scatterplot",shading="support",main="Scatterplot for Association Rules")#on confidence and lift dimension



#Plot top n rules
plot(rules_conf[8:13], measure="confidence",method = "graph",shading="lift",main = "Merchandise Association by Confidence")
inspect(rules_conf[8:13])

plot(rules_lift[1:3], measure="confidence",method="graph",shading="lift",main="Top 3 Association Rules by Lift",measureLabels=T,precision=2,cex=0.8,arrowSize=0.4)#add measure labels


topRules= rules_lift[1:6000]#get more rules for filtering 

# I also tried the products of best sellers to find out related bundles which could meet the rules. Actually the below bundles could be also my promotions if I am allowed to promote more than 3 bundles

P00059442.topRules = subset(topRules, lhs %in% "P00059442")#confurther filter the top rules for a specific item on lhs
length(P00059442.topRules)
inspect(P00059442.topRules[1:1])
plot(P00059442.topRules[1], measure="confidence",method="graph",precision=2,main="CHILDS GARDEN FORK P00059442 as Antecedent",cex=0.8,arrowSize=0.4)


P00046742.topRules = subset(topRules, rhs %in% "P00046742")#further filter the top rules for a specific item on rhs
length(P00046742.topRules)
inspect(P00046742.topRules[15:17])
plot(P00046742.topRules[17], measure="confidence",method="graph",precision=2,measureLabels=T,main="P00046742",cex=0.8,arrowSize=0.4)

P00058042.topRules = subset(topRules, rhs %in% "P00058042")#further filter the top rules for a specific item on rhs
length(P00058042.topRules)
inspect(P00058042.topRules[20])
plot(P00058042.topRules[17], measure="confidence",method="graph",precision=2,measureLabels=T,main="P00058042",cex=0.8,arrowSize=0.4)

