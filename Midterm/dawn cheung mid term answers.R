####MIDTERM ANSWERS

library(tidyverse)
library(dplyr)
library(class)
library(rpart)
library(rpart.plot)
data = read.csv("~/Github/DSA1101 Slayers/datasets/abalone2.csv") #Q1

set.seed(803)
glimpse(data)

data$sex = as.factor(data$sex)
attach(data)

#2
head(data)
#rownames(data)
#report of names of all the columns: sex, length, diameter, weight,  age


#3
Year = age
Year[which(age <= 10.5)] = "young"
Year[which(age > 10.5)] = "old"
#data %>%
#  mutate(Year = Year)


#Q4
table(Year)


#Q5
qqnorm(age, pch = 10)
qqline(age, col = "red")
#comments: the distribution of age is not normal because from theoretical quantiles 1 and above, the QQ plot curves upwards away from the red line.
#hence the left tail is shorter than normal
#However, when theoretical quantiles is between -2 and 1, the plots are on the red line, so in the range it can be considered normal

#Q6
boxplot(age, xlab = "variable", col = "blue")
Outlier = boxplot(age)$out
length(Outlier)
#there are 278 outliers
# There are multiple outliers, 3 are below the median and the rest above.



#Q7
#assume that 'large outliers' are outliers detected by R
out_weight = weight[which(age == Outlier)]
mean(out_weight)

mean(weight)

#the mean of the outlier weights is larger by the
#mean of the weights in the entire dataset
mean(out_weight) - mean(weight)
#the difference is 53.53811
#which is relatively large

#Q8
plot(age, weight)
#general positive relationship
#as age increases, variability of weight increases significantly 
#not good for linear model 


###PART III


#Q9
M1 = lm(age ~ sex + length + diameter + weight)
#factor done earleir
summary (M1)

#I(sex = "M") and weight are not significant 
#p-values:
#I(sex = "M"): 0.0679
#weight: 0.1049


#10
#plot(M1)
#F statistic is low, at p-value: < 2.2e-16
#quantitative values are used: suitable
#

#11
Q = data.frame(sex = "M", length = 120, diameter = 90, weight = 240)
predict(M1, Q)
#prediction value: 12.16405



#KNN

#12
type2 = c()

#using length, diameter, weight
glimpse(data)
stand.X = scale(data[, 2:4])

  
setA = sample(1:4177, size = 835)
q = 1:4177
q = q[which(q != setA)]
setB = sample(q, size = 3342)

for (i in 11:50) {
  train.x = stand.X[setB,]
  test.x = stand.X[setA,]
  train.y = Year[setB]
  test.y = Year[setA]
  knn.pred = knn(train.x, test.x, train.y, k = i)
  result = data.frame(test.y, knn.pred)
  result = table(result)
  T1 = result[2,1] / (result[2,1] + result[2,2])
  T2 = result[1,2] / (result[1,1] + result[1,2])
  type2 = c(type2, T2)
}

plot(11:50, type2)
#we want type 2 error to be minimised
#46 or 45
W = data.frame(no = 11:50, type2 = type2)
W[which(W$no == 46), 2]
W[which(W$no == 45), 2]

#15

fit <- rpart(Year ~ length + diameter + weight,
             method = "class",
             data = data,
             control = rpart.control(cp = 0.003),
             parms = list(split = "information"))
#16
rpart.plot(fit, type = 4, extra = 2, clip.right.labs = FALSE)
#sex = M, length = 120 mm, diameter = 90 mm, and weight = 240 grams.
#weight abover 130g => abalone is OLD

#17
table(fit)
#accuracy = 




