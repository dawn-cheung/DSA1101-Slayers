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


#CATAGORICAL: Heart disease
dihea = table(diabetes, heart_disease); dihea
dihea = prop.table(dihea, "heart_disease"); dihea
barplot(dihea,
        main = "Proportion of Diabetes Patients With and Without Heart Disease",
        xlab = "Heart Disease",
        col = c("darkred", "blue"))
#Among those with heart disease, 32.1% of them have diabetes.
#Among those without heart disease, 7.53% of them have diabetes.


#CATAGORICAL: Smoking history
dihist = table(diabetes, smoking_history); dihist
dihist = prop.table(dihist, "smoking_history"); dihist
barplot(dihist,
        main = "Proportion of Diabetes Patients Across Different Smoking Histories",
        xlab = "Heart Disease",
        col = c("darkred", "blue"))


#CONTINUOUS: BMI
hist(bmi)
boxplot(bmi ~ diabetes, xlab = "diabetes", col = "blue")





