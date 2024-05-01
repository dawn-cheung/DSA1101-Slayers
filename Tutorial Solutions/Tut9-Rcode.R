
setwd("~/Github/DSA1101 Slayers/")

# TUTORIAL 9 

library(ROCR)
library(e1071)


titanic= read.csv("~/Github/DSA1101 Slayers/datasets/Titanic.csv")
dim(titanic)
head(titanic)

attach(titanic)

table(Survived)



# (a) Logistic regression

sur = ifelse(Survived == 'Yes', 1, 0)# response must be of 0 and 1 to fit the model

sur = as.factor(sur)

titanic$sur = sur ; head(titanic)

M2 <- glm(sur~ Class + Sex + Age, data=titanic, family=binomial(link= "logit"))

summary(M2)

# (b) write down the fitted model, phat = predicted probability of survival:

# log[phat/(1-phat)] = 2.0438 -1.0181*I(Class = 2nd) -1.7778*I(Class = 3rd) 
#                   -0.8577* I(Class = Crew) -2.4201*I(Sex = Male) + 1.0615*I(Age = Child)

# (c) Interpret the coefficient of variable SEX:

# FEMALE IS REFERENCE. MALE IS INDICATED BY INDICATOR.
# coefficient is estimated = -2.4201. 
# It means, given the same condition on the class and age,
# when comparing to a female, the LOG-ODDS of survival for a male is less than by 2.42.
# It means, the ODDS of survival of a male passenger will be less than that of a female by
# e^2.42 = 11.25 TIMES.


# (d) Interpret the coefficient of variable AGE:
 
# "ADULT" IS CHOSEN AS REFERENCE. CATEGORY "CHILD" IS INDICATED BY AN INDICATOR.
# coefficient is estimated = 1.0615.
# It means, given the same condition on the class and gender,
# when comparing to an adult, the LOG-ODDS of survival of a child is larger by 1.0615.
# That means, the ODDS of survival of a child passenger is larger than that of an adult passenger by 
# e^1.0615 = 2.89 TIMES.


# (e) ROC & AUC
#different classfiers produce different performances
#plot 2 ROC curves in the same plane

library(ROCR)

# ROC for Logistic Regresison:
pred = predict(M2, type="response") # type = response to get the probability of survived
pred_log = prediction(pred, titanic$Survived)
roc_log = performance(pred_log, measure="tpr", x.measure="fpr")
plot(roc_log, col = "red")

auc1 = performance(pred_log , measure ="auc")
auc1@y.values[[1]] # 0.7597259




# ROC for Naive Bayes classifier
library(e1071)

M1 <- naiveBayes(Survived ~ Class + Sex + Age, data = titanic)

pred.M1 <- predict(M1, titanic[,1:3],type='raw')
# this pred.M1 has two columns: first column is probability of NOT survived;
# second column is probability of survived.
pred.M1 <- pred.M1[, 2] # select the second column, probability of survived only.
pred_nb <- prediction(pred.M1, titanic$sur)
roc_nb = performance(pred_nb, measure="tpr", x.measure="fpr")
plot(roc_nb, add = TRUE, col = "blue") #add = TRUE means plot the curve in the existing plot

# MUST HAVE "add = TRUE" to add this curve to the existing plot  before



auc2 <- performance(pred_nb , "auc")@y.values[[1]]; auc2
#0.7164944


legend("bottomright", c("logistic regression","naive Bayes"),col=c("red","blue"), lty=1)







