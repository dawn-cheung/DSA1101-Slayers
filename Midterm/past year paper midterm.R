###PAST YEAR PAPER MIDTERM

library(tidyverse)
library(class)
library(rpart)
library(rpart.plot) #remember its a diff package as rpart!!
crabdata = read.csv("~/Github/DSA1101 Slayers/datasets/data1.txt", sep = "", header = TRUE)
attach(crabdata)

#WHY DOES THIS KEEP POPING UP PLS
#Error in bindingIsActive(name, env) : 
#  no binding for "color.spine.width.satell.weight"
#Error in bindingIsActive(name, env) : 
#  no binding for "color.spine.width.satell.weight"



head(crabdata)
#note all female 

##Part I: Exploring the response variable - satell

#Q1
hist(satell, col = 10, freq = FALSE)
lines(density(satell), col = "red")

#comments:
# extremely right skewed
# multimodal-- 2 peaks at 0 and 4
# modal value is 0
# range is 0 to 15
# not normally distributed because it is not symmetrical at all

#Q2
boxplot(satell)
#shows 2 outliers

#comments:
# IQR is 5
# Majority of datapoints are below 5
# Median is around 2

outlier = boxplot(satell)$out
outlier
#[1] 14 15


#Q3
qqnorm(satell, pch = 10)
#comments:
# when sample quantiles == 0, horizontal straight line is formed
# when sample quantiles =! 0, points form a linear positive line??
# so maybe its normally distributed
# not normally distributed


##Part II: Variable color

#Q4

col = c()

#shortcut: ctrl + k + c to comment
#ctrl k + u to uncomment
# for (i in 1:length(color)) {
#   if (color[i] < 4) {
#     col = c(col, "light") # or append(og_vector, new_values)
#   } else {
#     col = c(col, "dark")
#   }
# }

col = c()
col[which(color <= 3)] = "light"
col[which(color >= 4)] = "dark"
col


#Q5
freq = table(col)
freq
# dark crabs: 66
# light crabs: 107

#Q6

library(dplyr)

plot(weight, satell)
points(weight[which(col == "light")], satell[which(col == "light")], col = "blue", )
points(weight[which(col == "dark")], satell[which(col == "dark")], col = "red")

#Q7
color = as.factor(color)
attach(crabdata)
summary(lm(satell ~ color + weight + width))

#Q2
library(class)
breastdata = read.csv("~/Github/DSA1101 Slayers/datasets/data2.csv")

breastdata$survival.status = as.factor(breastdata$survival.status)
breastdata = breastdata[,c(1,3,4)]
attach(breastdata)
glimpse(breastdata)

set.seed(999)

setA = sample(1:306, size = 102)
q = 1:306
q = q[which(q != setA)]
setB = sample(q, size = 102)
q = q[which(q != setB)]
setC = sample(q, size = 102)


train.x = breastdata[c(setA, setB), c(1,2)]
test.x = breastdata[setC, c(1,2)]
train.y = breastdata[c(setA, setB), c(3)]
test.y = breastdata[setC, c(3)]

knn.pred = knn(train.x, test.x, train.y, k = 1)

result = data.frame(test.y, knn.pred)
result = table(result)

FPRA = result[2,1] / (result[2,1] + result[2,2])
FNRA = result[1,2] / (result[1,1] + result[1,2])






#type1 = 


