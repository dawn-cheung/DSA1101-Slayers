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
attach(data)