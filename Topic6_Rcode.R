
#############  EXAMPLE 1: CLASSIFYING FRUITS

fruit.dat= read.csv("~/Github/DSA1101 Slayers/datasets/fruit.csv")
#Long: 1 = Yes, 0 = No
#Sweet: 1 = Yes, 0 = No
#Yellow: 1 = Yes, 0 = No
fruit.dat<- data.frame(lapply(fruit.dat, as.factor))
head(fruit.dat)
attach(fruit.dat)

table(Long)
table(Sweet)
table(Yellow)

#Install package 'e1071' first
#install.packages("e1071")
library(e1071)

model <- naiveBayes(Fruit ~ Long+Yellow+Sweet,fruit.dat)


newdata <- data.frame(Long=1,Sweet=1, Yellow=0)
newdata <- data.frame(lapply(newdata, as.factor))

results <- predict (model,newdata,"raw")
results

results <- predict (model,newdata,"class") # default setting
results



######## EXAMPLE 2: EMPLOYEE & ONSITE EDUCALTIONAL PROGRAM


sample <- read.table("~/Github/DSA1101 Slayers/datasets/sample1.csv",header=TRUE,sep=",")
head(sample)
dim(sample)
sample
attach(sample)
# Enrolls = RESPONSE with 2 categories

######### PART 1: MANUAL FORMING NAIVE BAYES CLASSIFIER ######

traindata <- as.data.frame(sample[1:14,])
testdata <- as.data.frame(sample[15,]) #note that this is only the last row/obs, with an empty enroll
testdata

# get the probability of each categories of the response
tprior <- table(traindata$Enrolls);tprior
tprior <- tprior/sum(tprior); tprior

# Get P(X = xi|Y = yj): row-wise proportion for feature AGE
ageCounts <- table(traindata[,c("Enrolls", "Age")]);ageCounts
ageCounts <- ageCounts/rowSums(ageCounts); ageCounts

# Get P(X = xi|Y = yj): row-wise proportion for feature INCOME
incomeCounts <- table(traindata[,c("Enrolls", "Income")])
incomeCounts <- incomeCounts/rowSums(incomeCounts);incomeCounts

# Get P(X = xi|Y = yj): row-wise proportion for feature JOBSATISFACTION
jsCounts <- table(traindata[,c("Enrolls", "JobSatisfaction")])
jsCounts <- jsCounts/rowSums(jsCounts);jsCounts

# Get P(X = xi|Y = yj): row-wise proportion for feature DESIRE
desireCounts <- table(traindata[,c("Enrolls", "Desire")])
desireCounts <- desireCounts/rowSums(desireCounts);desireCounts


# point 15 (test point) has:
#    Age Income JobSatisfaction Desire Enrolls
#   <=30 Medium             Yes   Fair

# Proportion that point 15 will be "Yes" for the outcome is proportional to:
prob_yes <-
ageCounts["Yes",testdata[,c("Age")]]*
incomeCounts["Yes",testdata[,c("Income")]]*
jsCounts["Yes",testdata[,c("JobSatisfaction")]]*
desireCounts["Yes",testdata[,c("Desire")]]*
tprior["Yes"]

# Proportion that point 15 will be "No" for the outcome is proportional to:
prob_no <-
ageCounts["No",testdata[,c("Age")]]*
incomeCounts["No",testdata[,c("Income")]]*
jsCounts["No",testdata[,c("JobSatisfaction")]]*
desireCounts["No",testdata[,c("Desire")]]*
tprior["No"]

#MAKING DECISION:
prob_yes/prob_no #4.115226. Hence the 15th observation should be classified as YES.



######### PART 2: USE PACKAGE e1071 FORMING NAIVE BAYES CLASSIFIER ######

library(e1071)

model <- naiveBayes(Enrolls ~ Age+Income+JobSatisfaction+Desire, traindata)#, laplace=0)

#model <- naiveBayes(response_var ~ predictor1 + predictor2, traindata)

results <- predict(model,testdata,"raw"); results

results[2]/results[1] # 4.115226


results <- predict(model,testdata,"class"); results


############### BANK-SAMPLE DATA ==> ROC and AUC


banktrain <- read.csv("~/Github/DSA1101 Slayers/datasets/bank-sample.csv", header=TRUE)
dim(banktrain)
head(banktrain)

# drop a few UNNECESSARY columns for the TRANNING DATA SET
drops <- c("balance", "day", "campaign", "pdays", "previous", "month")
banktrain <- banktrain [,!(names(banktrain) %in% drops )]

# TESTING DATA SET
banktest <- read.csv("~/Github/DSA1101 Slayers/datasets/bank-sample-test.csv")
banktest <- banktest[,!( names ( banktest ) %in% drops )]

library(e1071)

# build the naive Bayes classifier
nb_model <- naiveBayes( subscribed ~., data = banktrain)

# perform on the test set BUT we need to remove the respponse column first

head(banktest);
ncol(banktest) # number of columns = 11. Respone = 11th column.

nb_prediction <- predict(nb_model, newdata = banktest[,-ncol(banktest)], type ='raw') #remove the last column bc thats the response variable: u cant use the response variable to determine the response variable lol
# this is the predicted response for the test set
nb_prediction

cbind(nb_prediction, banktest[,ncol(banktest)])



# PLOT ROC CURVE FOR THE NAIVE BAYES CLASSIFIER ABOVE:
#install.packages("ROCR") 
# https://cran.r-project.org/web/packages/ROCR/ROCR.pdf

library(ROCR)
score <- nb_prediction[, c("yes")] 
# score is the conditional prob from Naive Bayes classifier for each test point

actual_class <- banktest$subscribed == 'yes' # actual response is 0 or 1 SO TO TRANSFORM the yes and no into 1 and 0, so prediction() can understand

pred <- prediction(score , actual_class) 
# this is to "format" the input so that we can use the function in ROCR to get TPR and FPR
#NOT ACTUALLY FOR PREDICTING

perf <- performance(pred , "tpr", "fpr")

plot (perf, lwd =2) # lwd is to specify how thick the curve is
abline (a=0, b=1, col ="blue", lty =3)


# COMPUTE AUC FOR NAIVE BAYES CLASSIFIER:
auc <- performance(pred , "auc")@y.values[[1]]
#auc <- unlist(slot (auc , "y.values"))
auc
# auc is used to compare between Naive Bayes methd with other 
# methods such as linear model, logistic model, DT, etc. 
# the one with larger auc value is better. (closer to 1)


# VISUALIZE ON HOW THE THRESHOLD CHANGES WILL CHANGE TPR AND FPR:

threshold <- round (as.numeric(unlist(perf@alpha.values)) ,4)
fpr <- round(as.numeric(unlist(perf@x.values)) ,4)
tpr <- round(as.numeric(unlist(perf@y.values)) ,4)

# adjust margins and plot TPR and FPR
par(mar = c(5 ,5 ,2 ,5))
# mar = a numerical vector of the form c(bottom, left, top, right) = c(5,4,4,2)
# http://127.0.0.1:14187/library/graphics/html/par.html

plot(threshold ,tpr , xlab ="Threshold", xlim =c(0 ,1) ,
     ylab = "True positive rate ", type ="l", col = "blue")
par( new ="True")
plot(threshold ,fpr , xlab ="", ylab ="", axes =F, xlim =c(0 ,1) , type ="l", col = "red" )
axis(side =4) # to create an axis at the 4th side
mtext(side =4, line =3, "False positive rate")
text(0.4 ,0.05 , "FPR")
text(0.6 ,0.35 , "TPR")


