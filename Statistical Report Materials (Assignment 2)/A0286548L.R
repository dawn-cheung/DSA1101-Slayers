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
