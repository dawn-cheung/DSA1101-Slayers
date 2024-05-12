# DSA1101 FINALS ANSWER DOCUMENT

library(tidyverse)
library(dplyr) # Piping (%>%)
library(class) #K-nearest neighbours
library(rpart) #Decision Tree
library(rpart.plot) #Decision Tree
library(e1071) #Naive Bayes
library(ROCR) #ROC / AUC
library(arules) #Association Rules
library(arulesViz) #Association Rules


set.seed(666)


#Q1a
# False

#Q1b
# True

#Q1c
# False

#Q1d
# True

#Q1e
# True

#Q2a
# B

#Q2b
# C

#Q2c
# A

#Q2d
# C

#Q2e recheck
# D


# QUESTION 3

# PART 1
#Q1
df1 = read.csv("~/Github/DSA1101 Slayers/datasets/crab.csv")
glimpse(df1)

attach(df1)

hist(satell)
# the distribution is very right skewed
# the median is around 0, where the frequency is substancially higher than all other satell values, at above 80



#Q2
plot(satell, width)
# there is no linear correlation, as the width does not seem to impact the satell value
# majority of the points are within 7-5 for satell and 22-10 for width


#Q3
df1$color = as.factor(df1$color)
df1$spine = as.factor(df1$spine)

attach(df1)

M1 = lm(satell ~ color + spine + width + weight,
        data = df1)
summary(M1)$r.squared

# the R^2 value is 0.151
# this is very low


#Q4
summary(M1)
# the most insignificant feature is I(color = 4) because it has the lowest Pr(>|t|) value.
# therefore the color is the most insignificant 


## PART 2
length(satell)
#Q5
df1$status = numeric(173)

status = ifelse(satell[which(satell > 0)], 1L, 0L)

df1$status

for (i in 1:length(df1$satell)) {
  if (satell[i] == 0) {
    df1$status[i] = 0
  } else {
    df1$status[i] = 1
  }
}
df1$status

attach(df1)


#Q6

length(which(df1$status == 1))
length(which(df1$status == 0))


df1$status = as.factor(df1$status)
attach(df1)
#number of crabs with at least one satellite is 111
# answer: 111

#Q7
freqtable = table(df1$color, df1$status)
CS.table = prop.table(freqtable)
CS.table #print contingency table

freqtable
#number of female crabs that are of medium color and has at least a satellite is 69
#ans: 69

#Q8
CS.table[1,1] / CS.table[1,2]
CS.table[2,1] / CS.table[2,2]
CS.table[3,1] / CS.table[3,2]
CS.table[4,1] / CS.table[4,2]

# light color: 0.333

# medium color: 0.3767

# dark color:  0.692

# darker color: 2.14

#as the color becomes darker, the conditional proportion increases,
# where the highest conditional proportion is for the dakerst color at 2.14

#Q9
M2 <- glm(status ~ width + weight + color,
          data = df1,
          family = binomial(link ="logit"))

summary(M2)

paste0("log[phat/(1-phat)] = ",
       M2$coeff[1]," + ", M2$coeff[2],
       "width + ", M2$coeff[3],
       "weight + ", M2$coeff[4],
       " * I(color == 3) + ", M2$coeff[5],
       " * I(color == 4) + ", M2$coeff[6],
       " * I(color == 4)")
# log[phat/(1-phat)] = -8.64449311729399 + 0.290594327211923width + 0.772730769115634weight + 0.131027125107058 * I(color == 3) + -0.161048125118175 * I(color == 4) + -1.2452597560655 * I(color == 5)
# ans: log[phat/(1-phat)] = -8.64 + 0.291width + 0.773weight + 0.131 * I(color == 3) + -0.161 * I(color == 4) + -1.25 * I(color == 5)
# phat is the fitted response variable of satell
# I(color = 3) is the catagorical variable of color where it is equal to 3


#Q10
# the coefficient is 0.773
# When the weight increases by 1 unit, then the log odds of status becoming 1 increases by 0.773,
# keeping all other variables the same.
# When the weight increases by 1 unit, then the odds of status becoming 1 changes by e^0.773 times, keeping all other variables the same


#Q11 i think its odds ratio == relook
# 0.131 * I(color == 3)
# -1.25 * I(color == 5)

# crabs with medium colour are more likely to have satellites than
# crabs with darker colour. crabs with darker color are less likley to have satellites as the
# coefficient is negative 

#Q12

#Crab A: 
Aframe = data.frame(width = 26, weight = 2.6, color = "4", spine = "1")
predict(M2, Aframe, type = "response")
#probability for A: 0.681139 

# B
Bframe = data.frame(width = 30, weight = 4.0, color = "2", spine = "3")
predict(M2, Bframe, type = "response")
#probability for B: 0.9594669 


#Q13

score = predict(M2, df1[,1:5], type = "response")
pred <- prediction(score , df1[,6])

perf <- performance(pred , "tpr", "fpr")

plot(perf, lwd = 2)
#plot complete


#14

auc = performance(pred, measure = "auc")
auc@y.values[[1]]
#auc value: 0.7775356

alpha <- round(as.numeric(unlist(perf@alpha.values)) ,4)
length(alpha) #its 164 very low pls dont fault me

fpr <- round(as.numeric(unlist(perf@x.values)) ,4)
tpr <- round(as.numeric(unlist(perf@y.values)) ,4)
plot(alpha, tpr ,xlab ="Threshold", xlim =c(0 ,1) ,
     ylab = "True positive rate ", type ="l", col = "blue")
par(new ="True")
plot(alpha, fpr, xlab ="", ylab ="", axes=F, xlim =c(0, 1) , type ="l", col = "red" )
axis(side =4) # to create an axis at the 4th side
mtext(side =4, line =3, "False positive rate")
text(0.57 ,0.18 , "FPR")
text(0.88 ,0.61 , "TPR")

#Q15

cbind(alpha, tpr, fpr)[70:150,] 
# 0.59 ≤ δ ≤ 0.6
# δ = 0.5995
# tpr = 0.7748
# fpr = 0.3226


#Q16

#FPR = FP / FP + TN

#TPR = TP / TP + FN

#perf <- performance(pred , "accuracy")
accuracy = 0.7748 + 0.3226

#17

M3 = naiveBayes(status ~ color + spine + width + weight, df1)
results <- predict(M3, df1[1:5],"class")
results

confusion.matrix = table(df1$status, results)
confusion.matrix

diag(confusion.matrix)

accuracy = sum(diag(confusion.matrix))/sum(confusion.matrix)
accuracy
#report accuracy: 0.7052023


#18
predict(M3, Aframe, "raw")[,2]
# crab A: 0.6758422 


predict(M3, Bframe, "raw")[,2]
# crab A: 0.9999156

# QUESTION 4

# Q1
pens = read.csv("~/Github/DSA1101 Slayers/datasets/penguins-dsa1101.csv")
glimpse(pens)
attach(pens)

plot(bill_depth, mass)
#i would choose k =  2 because there are 2 distinct clusters, one in the range of
# mass between 3800 and 6200, and billdepth between 12 and 17.8
# and the other cluster in the range of 
# mass between 2500 and 4900, and billdepth between 15.5 and 23

#Q2
Standardise.X = scale(pens[,])

wss = numeric(8)

for (k in 1:8) {
  kout = kmeans(Standardise.X, centers = k)
  wss[k] = kout$withinss
  print(wss)
}

plot(1:8, wss, type = 'b')

#best k is 2 because the largest drop in wss happens before just before that

#4
kout = kmeans(Standardise.X, centers = 2)
kout$centers
# bill_depth       mass
#1  0.6168934 -0.6122838
#2 -1.0983711  1.0901639


#first center is (0.6168934, -0.6122838)
#2second center is (-1.0983711,  1.0901639)




