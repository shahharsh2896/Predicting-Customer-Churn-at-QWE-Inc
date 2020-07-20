str(churn_data)
View(churn_data)

#1
#We only require the data of customers who have churned as 1.
churn_data$`Churn (1 = Yes, 0 = No)`<-as.factor(churn_data$`Churn (1 = Yes, 0 = No)`)
churned<-churn_data[churn_data$`Churn (1 = Yes, 0 = No)`=='1',]
hist(churned$`Customer Age (in months)`, breaks=10)
table(churned$`Customer Age (in months)`)
sum(churned$`Customer Age (in months)`>5 & churned$`Customer Age (in months)`<15)
#165
sum(churned$`Customer Age (in months)`<6 | churned$`Customer Age (in months)`>14)
#158
#The histogram clearly indicates that customer's with age between 6 months and 14 months(165) is more 
#as compared to those customer's with age less than 6 months and greater than 14 months and almost
#50% of the customer's who churn lie in this age group. 
#also the peak value is at 12 months, where 56 customers churned.
#Yes, Wall's belief about the dependence of churn rates on customer age is supported by the data.


#2
library(ggplot2)
library(Rcpp)
churn_data<-churn_data[,-1]

# forward selection
 
null = glm(`Churn (1 = Yes, 0 = No)`~ 1, data= churn_data, family ="binomial") # only includes one variable
full = glm(`Churn (1 = Yes, 0 = No)`~ ., data= churn_data, family = "binomial") # includes all the variables
 # We can perform forward selection using the command:
 step(null, scope=list(lower=null, upper=full), direction="forward")

#Logistic Regression Model
options(scipen=99)
mylogit = glm( `Churn (1 = Yes, 0 = No)`~`CHI Score Month 0`+ `Customer Age (in months)` +`Days Since Last Login 0-1`+`CHI Score 0-1`, data = churn_data, family = "binomial")
summary(mylogit)

#Predicting probabilities
churn_data$Prob<- predict.glm(mylogit, newdata = churn_data, type = "response")
#672 >>> Preidcted Probability >>> 0.03416394 >>> 3.42% >>> Low(Will not leave), did not leave actually.
#354 >>> Preidcted Probability >>> 0.04267323 >>> 4.27% >>> Low(Will not leave), did not leave actually.
#5203 >>> Preidcted Probability >>> 0.041397347 >>> 4.14% >>> Low(will not leave), did not leave actually.

#Selecting 100 customers with highest probability of churn
churn_data1<-churn_data[order(-churn_data$Prob),]
churn_data1<- head(churn_data1,100)
# forward selection
null1 = glm(`Churn (1 = Yes, 0 = No)`~ 1, data= churn_data1) # only includes one variable
full1 = glm(`Churn (1 = Yes, 0 = No)`~ ., data= churn_data1) # includes all the variables
# We can perform forward selection using the command:
step(null, scope=list(lower=null, upper=full), direction="forward")

#Top 3 drivers
#`CHI Score Month 0` +  `Customer Age (in months)` + `Days Since Last Login 0-1`