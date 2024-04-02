
###### Q1:  TITANIC DATA SET & NAIVE BAYES


titanic= read.csv("~/Github/DSA1101 Slayers/datasets/Titanic.csv")
dim(titanic)
head(titanic)

attach(titanic)

table(Survived)

# (a) Compute the probabilities P(Y = 1) (survived) and P(Y = 0) (did not survive).

tprior <- table(Survived) # Number of ppl survived & not survived
tprior
tprior <- tprior/sum(tprior) # the probability scores
tprior


# (b) Compute the conditional probabilities P(Xi = xi|Y = 1) and P(Xi = xi|Y = 0):

classCounts <- table(titanic[,c("Survived", "Class")]); classCounts
classCounts <- classCounts/rowSums(classCounts); classCounts

genderCounts <- table(titanic[,c("Survived", "Sex")]); genderCounts
genderCounts <- genderCounts/rowSums(genderCounts) ; genderCounts

ageCounts <- table(titanic[,c("Survived", "Age")]); ageCounts
ageCounts <- ageCounts/rowSums(ageCounts); ageCounts


# (c) Predict survival for an adult female passenger in 2nd class cabin.

prob_survived <-
classCounts["Yes","2nd"]*
genderCounts["Yes","Female"]*
ageCounts["Yes","Adult"]*
tprior["Yes"]

prob_survived


prob_not_survived <-
classCounts["No","2nd"]*
genderCounts["No","Female"]*
ageCounts["No","Adult"]*
tprior["No"]


prob_survived
prob_not_survived

prob_survived/prob_not_survived


# (d) Compare your prediction above with the one performed by the naiveBayes()
#function in package `e1071'
library(e1071)

M1 <- naiveBayes(Survived ~ Class + Sex + Age, titanic)

test <- data.frame(Class="2nd", Sex="Female", Age="Adult")

results <- predict(M1,test)
results
results <- predict(M1,test, "raw")
results

#ratio of probability score
prob_survived/prob_not_survived

#ratio of actual probabilities
results[1,"Yes"]/results[1,"No"]



################# CONCLUDE: SAME ANSWER COMPARED TO MANUAL CALCULATION



####################  TITANIC DATA SET & DECISION TREE



### THIS IS TO TRANSOFRM Survived with Yes/No to 1/0
# THEN FORMING THE TREE TO GET THE CLASS LABEL FOR OUTCOMES

sur = ifelse(Survived == 'Yes', 1, 0) #

sur = as.factor(sur)

titanic$sur = sur ; head(titanic)



library("rpart")
library("rpart.plot")


M2<- rpart(sur ~ Class + Sex + Age,
            method ="class",
            data = titanic,
            control = rpart.control(minsplit = 1),
            parms = list(split ='information'))

rpart.plot(M2 , type =4, extra =2, clip.right.labs = FALSE , varlen =0, faclen =0)




pred.M2 = predict(M2, newdata = titanic[,1:3], type = 'class'); #pred

data.frame(pred.M2, titanic$sur)[1:20,] # FIRST 20 ROWS




# ROC & AUC for both NAIVE BAYES & DECISION TREES:

# different classfiers produce different performances
# plot 2 ROC curves in the same plane

library(ROCR)

####  FOR NAIVE BAYES CLASSIFIER:



# ROC for Naive Bayes classifier
pred.M1 <- predict(M1, titanic[,1:3],type='raw')   # get the probabilities: P(Y =No) and P(Y = Yes)
score <- pred.M1[, 2] # ONLY TAKE THE PROBABILITY OF YES

pred_nb <- prediction(score, titanic$sur)
roc_nb = performance(pred_nb, measure="tpr", x.measure="fpr")
plot(roc_nb,  col = "red") #add = TRUE means plot the curve in the existing plot

auc1 <- performance(pred_nb , "auc")@y.values[[1]]; auc1
#0.7164944


########  FOR DECISION TREES:

# ROC for Decision Trees:
pred.M2 = predict(M2, titanic[,1:3], type='class') # to get the class (0 or 1), not the probabilities
pred.M2= as.numeric(paste(pred.M2))


pred_dt = prediction(pred.M2, titanic$sur)
roc_dt = performance(pred_dt, measure="tpr", x.measure="fpr")
plot(roc_dt, add = TRUE)

legend("bottomright", c("Naive Bayes","Decision Trees"),col=c("red","black"), lty=1)


auc2 = performance(pred_dt , measure ="auc")
auc2@y.values[[1]] # 0.683162

# BASED ON AUC, NAIVE BAYES IS BETTER THAN DECISION TREE.






#######################################  
############### 
############### WHEN WE FORM THE TREE USING SURVIVED (YES/NO) AND GET THE PROBABILITIES
############### INSTEAD OF GETTING THE CLASS FOR OUTCOME, THEN:


M2<- rpart(Survived ~ Class + Sex + Age, 
            method ="class",
            data = titanic,
            control = rpart.control(minsplit = 1),
            parms = list(split ='information'))


rpart.plot(M2 , type =4, extra =2, clip.right.labs = FALSE , varlen =0, faclen =0)


#by probabilities

pred.M2 = predict(M2, newdata = titanic[,1:3], type = 'prob') # GET THE PROBABILTIES of No and Yes, NOT THE CLASS

####  FOR NAIVE BAYES CLASSIFIER: SAME AS BEFORE

# ROC for Naive Bayes classifier
pred.M1 <- predict(M1, titanic[,1:3],type='raw')
score1 <- pred.M1[, 2] # ONLY TAKE THE PROBABILITY OF YES

pred_nb <- prediction(score1, titanic$Survived)
roc_nb = performance(pred_nb, measure="tpr", x.measure="fpr")
plot(roc_nb,  col = "red") #add = TRUE means plot the curve in the existing plot

auc1 <- performance(pred_nb , "auc")@y.values[[1]]; auc1
#0.7164944


########  FOR DECISION TREES:

# ROC for Decision Trees:
pred.M2 = predict(M2, titanic[,1:3], type='prob') # GET THE PROBABILTIES of No and Yes, NOT THE CLASS

score2 = pred.M2[,2] # ONLY TAKE THE PROBABILITY OF YES


pred_dt = prediction(score2, titanic$Survived)
roc_dt = performance(pred_dt, measure="tpr", x.measure="fpr")
plot(roc_dt, add = TRUE)

legend("bottomright", c("Naive Bayes","Decision Trees"),col=c("red","black"), lty=1)


auc2 = performance(pred_dt , measure ="auc")
auc2@y.values[[1]] 
# 0.7262628


###############  CONCLUDE: DECISION TREE IS BETTER THAN NAIVE BAYES

# when forming the ROC curve, it's better to use the PROBABILITIES of (Y = Yes), NOT THE CLASS
# hence, the later conclusion is more precise. 







