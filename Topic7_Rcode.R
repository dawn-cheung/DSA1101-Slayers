
# LOGISTIC CURVE

z = seq ( -10 ,10 ,0.1);
logistic = function (z) {exp(z)/(1+ exp(z))}

plot(z, logistic(z), xlab ="x", ylab ="p", lty =1, type ='l')


##########  DATA SET ON CUSTOMER CHURN

churn = read.csv("C:/Data/churn.csv")
head(churn)

churn$Churned = as.factor(churn$Churned)
churn$Married = as.factor(churn$Married)
churn= churn[,-1] #Remove ID column

attach(churn)

table(Churned)
prop.table(table(Churned))


# LOGISTIC MODEL
M1<- glm( Churned ~., data =churn,family = binomial)
summary(M1)


M2<- glm( Churned ~ Age + Married + Churned_contacts,
 data =churn,family = binomial(link ="logit"))
summary(M2)

M3<- glm( Churned ~Age + Churned_contacts,
 data =churn,family = binomial(link ="logit"))
summary(M3)

predict(M3, newdata = data.frame(Age = 50, Churned_contacts = 5), type = 'response')
# type = 'response' means we want to get the Pr(Y = 1).




# ROC CURVE FOR LOGISTIC MODEL

library(ROCR)
prob = predict(M3, type ="response")

# above is to predict probability Pr(Y = 1) for each point in the training data set, using M3
# type = c("link", "response", "terms"). 
# http://127.0.0.1:14187/library/stats/html/predict.glm.html

pred = prediction(prob , Churned )
roc = performance(pred , "tpr", "fpr")
auc = performance(pred , measure ="auc")
auc@y.values[[1]]
plot(roc , col = "red", main = paste(" Area under the curve :", round(auc@y.values[[1]] ,4)))



# HOW TPR, FPR CHANGE WHEN THRESHOLD CHANGES:

# extract the alpha(threshold), FPR , and TPR values from roc
alpha <- round (as.numeric(unlist(roc@alpha.values)) ,4)
length(alpha) 
fpr <- round(as.numeric(unlist(roc@x.values)) ,4)
tpr <- round(as.numeric(unlist(roc@y.values)) ,4)

# adjust margins and plot TPR and FPR
par(mar = c(5 ,5 ,2 ,5))

plot(alpha ,tpr , xlab ="Threshold", xlim =c(0 ,1) ,
     ylab = "True positive rate ", type ="l", col = "blue")
par( new ="True")
plot(alpha ,fpr , xlab ="", ylab ="", axes =F, xlim =c(0 ,1) , type ="l", col = "red" )
axis( side =4) # to create an axis at the 4th side
mtext(side =4, line =3, "False positive rate")
text(0.18 ,0.18 , "FPR")
text(0.58 ,0.58 , "TPR")


# there are some metrics that can help to choose a threshold: G-mean; Youden’s J statistic; etc


