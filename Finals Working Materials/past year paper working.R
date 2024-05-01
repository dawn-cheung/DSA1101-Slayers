## past year paper working

library(tidyverse)
library(dplyr) # Piping (%>%)
library(class) #K-nearest neighbours
library(rpart) #Decision Tree
library(rpart.plot) #Decision Tree
library(e1071) #Naive Bayes
library(ROCR) #ROC / AUC
library(arules) #Association Rules
library(arulesViz) #Association Rules


set.seed(1101)

data = read.csv("~/Github/DSA1101 Slayers/datasets/data_finals.csv")
glimpse(data)

#Q1
data = data[4:8]
attach(data)

#Q2
length(Status[which(Status == "Developed")]) #14 countries


#Q3
data$Status[which(data$Status == "Developing")] = 0
data$Status[which(data$Status == "Developed")] = 1

attach(data)
glimpse(data)



#Q4
M1 = lm(Status ~ Life_expectancy + Adult_mortality + infant_deaths + Alcohol)
summary(M1)$r.squared #r squared value: 0.300 3sf

#Q5
summary(M1) #there are 2(??)


#Q6
#status is a factor variable, with only 2 possible values, 1 and 0
#the linear model is more suited for quantitative response, not catagorical 

#Q7
score = predict(M1, newdata = data[,2:5], type = "response")
pred <- prediction(score , data[,1])
perf <- performance(pred , "tpr", "fpr")

aucLM = performance(pred, measure = "auc")
aucLM@y.values[[1]] #AUC value = 0.8895238

plot(perf , col = "red")
abline(a=0, b=1, col ="blue", lty =3)

#Q8
data$Status = as.factor(data$Status)

attach(data)
glimpse(data)

M2 <- glm(Status ~ Life_expectancy + Adult_mortality + infant_deaths + Alcohol,
          data = data,
          family = binomial(link = "logit"))

summary(M2)

paste0("log[phat/(1-phat)] = ", M2$coeff[1]," + ",
       M2$coeff[2], "Life_expectancy + ", M2$coeff[3], "Adult_mortality + ",
       M2$coeff[4], "infant_deaths + ", M2$coeff[5], "Alcohol")
#final equation: log[phat/(1-phat)] = -19.5256652906616 + 0.227590793959752Life_expectancy + 0.000854875770951936Adult_mortality + -0.47171464738735infant_deaths + 0.148746770871717Alcohol


#Q9
# the coefficient of Alcohol is 0.148746770871717.
# When the alcohol consumption (per capita (15+) consumption, in liters of pure alcohol) increases by 1 unit, then the log odds of Status changing from developing to developed increasing by 0.149, keeping all other variables the same.
# When alcohol consumption increases by 1 unit, then the odds of being a developed country changes by e^0.148 times, keeping all other variables the same



#Q10
help = data.frame("Life_expectancy" = 83, 
           "Adult_mortality" = 57,
           "infant_deaths" = 2,
           "Alcohol" = 3)

predict(M2, help, type = "response")
#probability: 0.2527327 

#Q11
score = predict(M2, newdata = data[,2:5], type = "response")
pred <- prediction(score , data[,1])
perf <- performance(pred , "tpr", "fpr")

aucGLM = performance(pred, measure = "auc")
aucGLM@y.values[[1]]

plot(perf , col = "red", type = "l")
abline(a=0, b=1, col ="blue", lty =3)#AUC value: 0.9342857


#Q12
alpha <- round(as.numeric(unlist(perf@alpha.values)) ,4)
length(alpha) 
fpr <- round(as.numeric(unlist(perf@x.values)) ,4)
tpr <- round(as.numeric(unlist(perf@y.values)) ,4)

q = cbind(alpha, tpr, fpr)
plot(fpr, tpr)

# adjust margins and plot TPR and FPR
par(mar = c(5 ,5 ,2 ,5))

plot(alpha ,tpr , xlab ="Threshold", xlim =c(0 ,1),
     ylab = "True positive rate ", type ="l", col = "blue")
par( new ="True")
plot(alpha ,fpr , xlab ="", ylab ="", axes = F, xlim =c(0,1), type ="l", col = "red" )
axis(side =4) # to create an axis at the 4th side
mtext(side =4, line =3, "False positive rate")
text(0.18 ,0.18 , "FPR")
text(0.58 ,0.58 , "TPR")


#Q13
M3 <- naiveBayes(Status ~ Life_expectancy + Adult_mortality + infant_deaths + Alcohol,
                 data)

results <- predict(M3, data[,2:5], "class"); results


confusion.matrix = table(Status, results)
confusion.matrix

diag(confusion.matrix)

accuracy = sum(diag(confusion.matrix))/sum(confusion.matrix)
accuracy



results <- predict(M3, help, type = "raw"); results
# probability: 0.9845948

score <- predict(M3, data[,2:5], "raw")[,2]
pred <- prediction(score , data[,1])
perf <- performance(pred , "tpr", "fpr")

aucNB = performance(pred, measure = "auc")
aucNB@y.values[[1]] #auc value = 0.9361905

plot(perf , col = "red", lwd =2)
abline(a=0, b=1, col ="blue", lty =3)#AUC value: 0.9342857


standardised.X = scale(data[,2:5])

accuracy = c()

for (i in 2:10){
  
  M4 <- knn(standardised.X, standardised.X, data[,1], k=i)
  confusion.matrix = table(data[,1], M4); confusion.matrix
  accuracy = append(accuracy, sum(diag(confusion.matrix))/sum(confusion.matrix))
  print(accuracy)
}

accuracy
#best is k = 2


#Q14
pred.knn =  knn(standardised.X, standardised.X, data[,1], k=2)

pred.knn.prob= knn(standardised.X, standardised.X, data[,1], k=2, prob = TRUE)

winning.prob = attr(pred.knn.prob, "prob") # to extract the winning probabilities

n = length(data[,1])

prob = numeric(length = n) # n is the length of "test.y" used in knn() above

for (i in 1:n) {
  prob[i] = ifelse(pred.knn[i] == "yes", winning.prob[i], 1 - winning.prob[i])
  print(prob)
} 

helppain = prediction(prob, data[,1]) 

rocObjKNN = performance(helppain, measure = "tpr", x.measure = "fpr")

plot(rocObjKNN, lwd =2, type = "l")
abline(a=0, b=1, col ="blue", lty =3)

aucLM = performance(pred, measure = "auc")
aucLM@y.values[[1]] #AUC value = 0.9361905


z = c(0, 5, 10)  # 3 points
z2 = z^2 + z*2+ 10 
plot(z2~z, type = "l")   #Figure 1


x = seq(0,10, 0.5)
x2 = x^2 + x*2+ 10 
plot(x2~x, type = "l") # Figure 2

M4 <- rpart(Status ~ .,
             method = "class",
             data = data,
             control = rpart.control(cp = 0.001),
             parms = list(split = "gini"))

rpart.plot(M4, type = 4, extra = 2, clip.right.labs = FALSE)
#most important: Alchohol consumption and life expectancy


results <- predict(M3, help, type = "raw"); results
# probability: 0.9845948

score <- predict(M3, data[,2:5], "raw")[,2]
pred <- prediction(score , data[,1])
perf <- performance(pred , "tpr", "fpr")

aucNB = performance(pred, measure = "auc")
aucNB@y.values[[1]] #auc value = 0.9361905

plot(perf , col = "red", lwd =2)
abline(a=0, b=1, col ="blue", lty =3)#AUC value: 0.9342857


#DECSION TREE
scoreDT <- predict(M4, data[,2:5], type = "prob")[,2]
predDT <- prediction(scoreDT, data[,1])
perfDT <- performance(predDT, "tpr", "fpr")

aucDT = performance(predDT, measure = "auc")
aucDT@y.values[[1]] #auc value = 0.9361905

plot(perfDT, add = TRUE, col = "blue", lwd =2)
legend("bottomright", c("Naive Bayes", "Decsion Tree"), col = c("red", "blue"), lty = 1)

#Q15


#Q16


#Q17


#Q18


#Q19


#Q20


#Q21


#Q22


#Q23


#Q24

