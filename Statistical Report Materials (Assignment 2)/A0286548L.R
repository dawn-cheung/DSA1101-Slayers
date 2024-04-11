# DSA1101 INDIVIDUAL ASSIGNMENT: STATISTICAL REPORT

library(tidyverse)
library(dplyr)
library(class)
library(rpart)
library(rpart.plot)
library(e1071)
library(ROCR)

set.seed(1101)

data = read.csv("~/Github/DSA1101 Slayers/datasets/diabetes-dataset.csv")
glimpse(data)
data$gender = as.factor(data$gender)
data$hypertension = as.factor(data$hypertension)
data$heart_disease = as.factor(data$heart_disease) 
data$smoking_history = as.factor(data$smoking_history)
data$diabetes = as.factor(data$diabetes)
glimpse(data)

attach(data)

min(age);max(age)
min(bmi);max(bmi)
min(HbA1c_level);max(HbA1c_level)
min(blood_glucose_level);max(blood_glucose_level)

levels(smoking_history)

#CATAGORICAL: Gender
digen = table(diabetes, gender); digen
digen = prop.table(digen, "gender"); digen
barplot(digen,
        main = "Proportion of Diabetes Patients Across Different Genders",
        xlab = "Genders",
        col = c("darkred", "blue"))
#Among females, 7.62% of them have diabetes.
#Among males, 9.75% of them have diabetes.
#Among transgender individuals, 0.00% of them have diabetes.

#NO Odds Ratio


#CATAGORICAL: Hypertension
dihyp = table(diabetes, hypertension); dihyp
dihyp = prop.table(dihyp, "hypertension"); dihyp
barplot(dihyp,
        main = "Proportion of Diabetes Patients With and Without Hypertension",
        xlab = "Hypertension",
        col = c("darkred", "blue"))
#Among those with hypertension, 27.9% of them have diabetes.
#Among those without hypertension, 6.93% of them have diabetes.
#odds ratio
wohyp = dihyp[2,1] / dihyp[1,1] #odds of success without hypertension 
whyp = dihyp[2,2] / dihyp[1,2] #odds of success with hypertension
OR = whyp / wohyp; OR
#The odds of having diabetes while having hypertension is 5.20 times the odds of having diabetes without having hypertension. 


#CATAGORICAL: Heart disease
dihea = table(diabetes, heart_disease); dihea
dihea = prop.table(dihea, "heart_disease"); dihea
barplot(dihea,
        main = "Proportion of Diabetes Patients With and Without Heart Disease",
        xlab = "Heart Disease",
        col = c("darkred", "blue"))
#Among those with heart disease, 32.1% of them have diabetes.
#Among those without heart disease, 7.53% of them have diabetes.
wohea = dihea[2,1] / dihea[1,1] #odds of success without heart disease
whea = dihea[2,2] / dihea[1,2] #odds of success with heart disease
OR = whea / wohea; OR
#The odds of having diabetes while having heart disease is 5.82 times the odds of having diabetes without having heart disease. 

#CATAGORICAL: Smoking history
dihist = table(diabetes, smoking_history); dihist
dihist = prop.table(dihist, "smoking_history"); dihist
barplot(dihist,
        main = "Proportion of Diabetes Patients Across Different Smoking Histories",
        xlab = "Heart Disease",
        col = c("darkred", "blue"))


#median(bmi)
#IQR(female)
#summary(bmi ~ diabetes)
#var(female)

#CONTINUOUS: BMI
hist(bmi)
boxplot(bmi ~ diabetes, xlab = "diabetes", col = "blue")

#CONTINUOUS: HbA1c_level
hist(HbA1c_level)
boxplot(HbA1c_level ~ diabetes, xlab = "diabetes", col = "blue")

#CONTINUOUS: blood_glucose_level
hist(blood_glucose_level)
boxplot(blood_glucose_level ~ diabetes, xlab = "diabetes", col = "blue")

#CONTINUOUS: age
hist(age)
aggie = boxplot(age ~ diabetes, xlab = "diabetes", col = "blue"); aggie
outtie  = aggie$out; outtie
length(outtie) #number of outiers: 118

#M1 = lm(diabetes ~ age + hypertension + heart_disease + bmi + HbA1c_level + blood_glucose_level,
#   data = data)
#summary(M1)
#LM not that appropriate bc response is a number
#maybe ask in the anonymous Q&A thing?

#DECSION TREE
fit <- rpart(diabetes ~ age + hypertension + heart_disease + bmi + HbA1c_level + blood_glucose_level,
             method = "class",
             data = data,
             control = rpart.control(cp = 0.001),
             parms = list(split = "information"))

rpart.plot(fit , type =4, extra =2, clip.right.labs =FALSE , varlen =0, faclen =0)

prediction = predict(fit, new.data = data[,1:8], type = "class")

confusion.matrix = table(prediction, data[,9])
confusion.matrix

accuracy = sum(diag(confusion.matrix))/sum(confusion.matrix)
accuracy

#ROC / AUC for decision tree

#NAVIVE BAYES
nbmodel = naiveBayes(diabetes ~ ., data)

results <- predict(model,testdata,"raw"); results




