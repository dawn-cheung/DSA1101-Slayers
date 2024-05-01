# DSA1101 INDIVIDUAL ASSIGNMENT: STATISTICAL REPORT

library(tidyverse)
library(dplyr)
library(class)
library(rpart)
library(rpart.plot)
library(e1071)
library(ROCR)
library(fastDummies)

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
dihist[2,1] / dihist[1,1] 
dihist[2,2] / dihist[1,2]
dihist[2,3] / dihist[1,3]
dihist[2,4] / dihist[1,4]
dihist[2,5] / dihist[1,5]
dihist[2,6] / dihist[1,6]


#median(bmi)
#IQR(female)
#summary(bmi ~ diabetes)
#var(female)

#CONTINUOUS: BMI
hist(bmi)
boxplot(bmi ~ diabetes, xlab = "diabetes", col = "blue")
bmie = boxplot(bmi ~ diabetes, xlab = "diabetes", col = "blue")
hello = data[which(bmi %in% bmie$out),]

PLEASE = hello %>% #outliers for those with diabetes
  filter(diabetes == 1) %>%
  select(bmi)

OHHH = hello %>% #outliers for those without diabetes
  filter(diabetes == 0) %>%
  select(bmi)

meed = data %>% #median for those with diabetes
  filter(diabetes == 1) %>%
  select(bmi)
meed = median(unlist(meed))

weed = data %>% #median for those without diabetes
  filter(diabetes == 0) %>%
  select(bmi)
weed = median(unlist(weed))

meed;weed

length(unlist(PLEASE))
length(unlist(OHHH))
length(bmie$out)
#outtie$bmi





#CONTINUOUS: HbA1c_level
hist(HbA1c_level)
boxplot(HbA1c_level ~ diabetes, xlab = "diabetes", col = "blue")

meed = data %>% #median for those with diabetes
  filter(diabetes == 1) %>%
  select(HbA1c_level)
meed = median(unlist(meed))

weed = data %>% #median for those without diabetes
  filter(diabetes == 0) %>%
  select(HbA1c_level)
weed = median(unlist(weed))

#CONTINUOUS: blood_glucose_level
hist(blood_glucose_level)
boxplot(blood_glucose_level ~ diabetes, xlab = "diabetes", col = "blue")

meed = data %>% #median for those with diabetes
  filter(diabetes == 1) %>%
  select(blood_glucose_level)
meed = median(unlist(meed))

weed = data %>% #median for those without diabetes
  filter(diabetes == 0) %>%
  select(blood_glucose_level)
weed = median(unlist(weed))

meed;weed

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

#WORKING WITH MODELS

#standardize data
data[,c("age", "bmi", "HbA1c_level", "blood_glucose_level")] = scale(data[,c("age", "bmi", "HbA1c_level", "blood_glucose_level")])

#split test and train data
no_rows = as.numeric(nrow(data))
train_index = sample(1:no_rows, size = (no_rows*0.8))
train.X = data[train_index, 1:8]
train.Y = data[train_index, 9]
test.X = data[-train_index, 1:8]
test.Y = data[-train_index, 9]

train.all = data[train_index, ] #for ease of use in decision tree

#DECSION TREE
fit <- rpart(diabetes ~ gender + age + hypertension + heart_disease + bmi + HbA1c_level + blood_glucose_level + smoking_history,
             method = "class",
             data = train.all,
             control = rpart.control(cp = 0.001),
             parms = list(split = "information"))

rpart.plot(fit, type =4, extra =2, clip.right.labs =FALSE , varlen =0, faclen =0)

preddy = predict(fit, newdata = test.X, type = "class")

dim(preddy)

confusion.matrix = table(test.Y, preddy)
confusion.matrix

diag(confusion.matrix)

accuracy = sum(diag(confusion.matrix))/sum(confusion.matrix)
accuracy #0.9708

#ROC / AUC for decision tree

preddy = predict(fit, newdata = test.X, type = "prob")[,2] #changing to raw for ROC's prediction
pred <- prediction(preddy, test.Y)
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 

plot(perf, lwd =2)
abline(a=0, b=1, col ="blue", lty =3)

aucDT = performance(pred, measure = "auc")
aucDT@y.values[[1]] # 0.8316032


#NAVIVE BAYES
nbmodel = naiveBayes(diabetes ~ ., train.all)
results <- predict(nbmodel,test.X,"class"); results

confusion.matrix = table(test.Y, results)
confusion.matrix

diag(confusion.matrix)

accuracy = sum(diag(confusion.matrix))/sum(confusion.matrix)
accuracy

#ROC / AUC for navive bayes

naiveB = predict(nbmodel, newdata = test.X, type = "raw") #for NB u need to specify what response variables (from the database) are needed

score = naiveB[, 2] 

preObjNB = prediction(score, test.Y)
rocObjNB = performance(preObjNB, measure = "tpr", x.measure = "fpr")
plot(rocObjNB, lwd =2)
abline(a=0, b=1, col ="blue", lty =3)
plot(rocObjNB, add = TRUE, col = "red") #so to add on to our prev graph

#getting AUC value for NB
aucNB = performance(preObjNB, measure = "auc")
aucNB@y.values[[1]]

#KNN

#pre-processing
data = dummy_cols(data, select_columns = c("gender", "smoking_history"))
#gender_male, gender_female, gender_other
#smoking_history_current, smoking_history_ever smoking_history_former, smoking_history_never, smoking_history_not current, smoking_history_No Info
data$hypertension = c(0, 1)[data$hypertension]
data$heart_disease = c(0, 1)[data$heart_disease]
attach(data)
train.X = data[train_index, c(3,4,6,7,8,10,11,12,13,14,15,16,17,18)]
train.Y = data[train_index, 9]
test.X = data[-train_index, c(3,4,6,7,8,10,11,12,13,14,15,16,17,18)]
test.Y = data[-train_index, 9]

accuracy = c()
FPR_KNN = c()
FNR_KNN = c()

for (i in c(20, 30, 40, 50, 100, 200, 300, 350)){ #total train is like 8000
  
  predKNN <- knn(train.X, test.X, train.Y, k=i) # KNN with k = 15,..., 8000
  confusion.matrix = table(test.Y, predKNN); confusion.matrix
  accuracy = append(accuracy, sum(diag(confusion.matrix))/sum(confusion.matrix))
  FPR_KNN = append(FPR_KNN, confusion.matrix[2,1]/(confusion.matrix[2,1]+confusion.matrix[1,1]))
  FNR_KNN = append(FNR_KNN, confusion.matrix[1,2]/(confusion.matrix[2,2]+confusion.matrix[1,2]))
  print(accuracy)
}

accuracy
FPR_KNN
FNR_KNN 
#FINAL: K = 30
#  accuracy = 0.96125
#  FPR_KNN = 0.0002184241
#  FNR_KNN = 0.4570243

pred.knn = knn(train.X, test.X, train.Y, k = 30, prob=TRUE)
pred.prob <- 1 - attr(pred.knn, "prob")
helppain = prediction(pred.prob, test.Y) 
rocObjKNN = performance(helppain, measure = "tpr", x.measure = "fpr")
plot(rocObjKNN, lwd =2)
abline(a=0, b=1, col ="blue", lty =3)
#getting AUC value for KNN
aucKNN = performance(helppain, measure = "auc")
aucKNN@y.values[[1]]
plot(rocObjKNN, add = TRUE, col = "green")

pred.knn =  knn(train.X, test.X, cl = train.Y, k = 30)  # "prob = TRUE" is NOT added --> to get the class labels
pred.knn.prob= knn(train.X, test.X, cl = train.Y, k = 30, prob = TRUE) #  --> to get the probabilities of winning labels
winning.prob = attr(pred.knn.prob, "prob") # to extract the winning probabilities

n = length(test.Y)
prob = numeric(length = 0) # n is the length of "test.y" used in knn() above
for (i in 1:n) {
  prob[i] = ifelse(pred.knn[i] == "yes", winning.prob[i], 1 - winning.prob[i])
} 
helppain = prediction(prob, test.Y) 
rocObjKNN = performance(helppain, measure = "tpr", x.measure = "fpr")
plot(rocObjKNN, lwd =2)
abline(a=0, b=1, col ="blue", lty =3)

    