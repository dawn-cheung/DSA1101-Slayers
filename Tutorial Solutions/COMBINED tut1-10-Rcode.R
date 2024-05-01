
setwd("~/Github/DSA1101 Slayers")


###########  SOLUTION OF TUTORIAL 1 #######



#Q1 SALARY IS NOT CHANGED OVER THE YEARS

price = 1200000 # House's price
cost = price*0.25 # down payment amount

r= 0.02 # percentage of monthly return from investment

portion_save = 0.4 # portion of salary for saving, every month

# salary is the monthly salary

#### FIRST PERSON WITH salary = 7000

salary = 7000
  
saved <- 10000 # initial savings that parents give
  
month <- 0
  
while(saved < cost){
    month = month +1
    saved = saved+ portion_save *salary + saved*r
  }
print(month)


# when salary = 7000 # answer should be 55 months



#### SECOND PERSON WITH salary = 10000

salary = 10000
  
saved <- 10000 # initial savings that parents give
  
month <- 0
  
while(saved < cost){
    month = month +1
    saved = saved+ portion_save *salary + saved*r
  }
print(month)


# when salary = 10000 # answer should be 44 months

# Extra question: 
# Can you think of a way which can make the code be easy if we have 10 persons with different salary?
# Hint: Which part of the code above for 2 persons is repeated? Use FOR loop

####### SHORTER CODE WITH FOR LOOP

price = 1200000 # House's price
cost = price*0.25 # down payment amount

r= 0.02 # percentage of monthly return from investment

portion_save = 0.4 # portion of salary for saving, every month

sal = c(7000,10000) # vector of salary for two persons

total.month = numeric(length(sal)) # vector to record the output - number of months

print(cbind(sal,total.month)) # we'll let the code to update the second column

for (i in 1:length(sal)){
  
salary = sal[i]
saved <- 10000 # initial savings that parents give

month <- 0

while(saved < cost){
  month = month +1
  saved = saved+ portion_save *salary + saved*r
}
total.month[i] = month
}

print(cbind(sal,total.month))


#####################################

#Q2 THERE IS A RAISE IN THE SALARY EVERY 4 MONTHS:

# rate is the raise in salary per 4 months, change from person to person

price = 1200000 # House's price

cost = price*0.25 # down payment amount

r= 0.02 #percentage of monthly return from investment

portion_save = 0.4 # portion of salary for saving, every month



# FIRST PERSON: SALAPRY = 7000 & rate = 0.02

salary = 7000

rate = 0.02

  
saved <- 10000 # savings given by parents initially
  
month <- 0

while(saved < cost){
      
    month = month +1
    
    saved = saved+ portion_save *salary + saved*r
    
    if (month%%4 ==0){salary = salary*(1+rate)} # increase the salary per 4 months
  }

print(month)


# when salary = 7000, & rate = 0.02,  answer: 52 months


# SECOND PERSON: SALAPRY = 10000 & rate = 0.01

salary = 10000

rate = 0.01

  
saved <- 10000 # savings given by parents initially
  
month <- 0

while(saved < cost){
      
    month = month +1
    
    saved = saved+ portion_save *salary + saved*r
    
    if (month%%4 ==0){salary = salary*(1+rate)} # increase the salary per 4 months
  }

print(month)


# when salary = 10000, & rate = 0.01, answer: 43 months

#################3


#Extra question: Can you use FOR loop to write a shorter code for Q2? Answer is given below


price = 1200000 # House's price

cost = price*0.25 # down payment amount

r= 0.02 #percentage of monthly return from investment

portion_save = 0.4 # portion of salary for saving, every month

sal = c(7000, 10000) # salary of 2 persons
Rate = c(0.02,0.01) # rate of increasement of 2 persons

total.month = numeric(length(sal)) # vector to record the output - number of months


print(cbind(sal, Rate,total.month))

for (i in 1:length(sal)){
  
  salary = sal[i]
  rate = Rate[i]
  saved <- 10000 # initial savings that parents give
  
  month <- 0
  
  while(saved < cost){
    month = month +1
    saved = saved+ portion_save *salary + saved*r
    if (month%%4 ==0){salary = salary*(1+rate)} # increase the salary per 4 months
  }
  total.month[i] = month
}

print(cbind(sal,Rate,total.month))


#R code for Tutorial 2 Solution

#### FEV dataset

fev = read.csv("C:/Data/FEV.csv")
names(fev)
attach(fev)

#Qb
hist(FEV, col = 10, freq= FALSE)

#Qc
boxplot(FEV, col = 10, ylab = "FEV", main = "Boxplot of FEV")

#outlier values
out = boxplot(FEV, col = 10, ylab = "FEV", main = "Boxplot of FEV")$out

# get the index of the outliers:
index = which(FEV %in% c(out)) 
index

#information of all the outliers:
fev[c(index),]



#Qd
qqnorm(FEV, pch = 20)
qqline(FEV, col = "red")


#Qe

female = FEV[which(Sex==0)] # or FEV[Sex==0]

male = FEV[which(Sex==1)] # or FEV[Sex==1]

opar <- par(mfrow=c(1,2)) #arrange a figure which has 1 row and 2 columns (to contain the 2 hitograms)

hist(female, col = 2, freq= FALSE, main = "Histogram of Female FEV", ylim = c(0,0.52))

hist(male, col = 4, freq= FALSE, main = "Histogram of Male FEV", ylim = c(0,0.52))

par(opar)

median(female)
IQR(female)
summary(female)
var(female)

median(male)
summary(male)
IQR(male)
var(male)


#Qf-g
plot(height, FEV)

plot(height, FEV, type = "n")
points(female ~ height[which(Sex==0)], col = "red", pch = 20)
points(male ~ height[which(Sex==1)], col = "darkblue", pch = 20)
legend(1.2, 5, legend = c("Female", "Male"), col = c("red","darkblue"), pch=c(20,20))

cor(FEV, height)


###########################  Q2
#(a)
Fibo = numeric(45)
Fibo[1:2] = 1
for(i in 3:45) {
  Fibo[i] = Fibo[i-1] + Fibo[i-2]
}

#(b)
Fibo[40]

# the 40th term is: 102334155

# find smallest n where Fn is more than 5 mil.
n = sum(Fibo<=5000000) + 1 ;n # 34

# OR can use this code
n = max(which(Fibo<=5000000)) + 1; n

Fibo[n] # 5702887



########### TUTORIAL 3 ##########


##########   Q1
simple <- function(x , y) {
  beta_1 <- (sum(x*y)- mean (y)* sum (x ))/( sum(x^2)- mean(x)* sum(x));
  beta_0 <- mean(y)- beta_1* mean(x) ;
  return(c( beta_0 , beta_1)) ;
}


dat= read.table("C:/Data/Colleges.txt",header =TRUE,sep= "\t")
names(dat)
head(dat)

# Compare outputs
simple(dat$SAT, dat$Acceptance )
lm(Acceptance ~ SAT , data =dat )




##########   Q2
#(a)
price = 1200000 # House's price
cost = price*0.25 # down payment amount
r= 0.02 #monthly rate return from investment
portion_save = 0.4 # portion of salary for saving, every month
### salary = monthly salary is the argument of F1
F1 = function(salary){ 
  saved <- 10000
  month <- 0
  while(saved < cost){
    month = month +1
    saved = saved+ portion_save *salary + saved*r
  }
  return(month)
}
###  Test F1 for the two cases:
F1(7000) # answer should be 55 months
F1(10000) # answer should be 44 months


#(b)
F2 <- function(salary, price = 1200000, rate = 0.01, portion_save = 0.4) {
  r = 0.02 #monthly rate return from investment
  saved <- 10000 # savings given by parents initially
  month <- 0
  cost = 0.25*price
  while(saved < cost){
    month = month +1
    saved = saved+ portion_save *salary + saved*r
    if (month%%4 ==0){salary = salary*(1+rate)} 
  }
  return(month)
}
### Test function F2
F2(salary = 7000, rate = 0.02) # answer: 52
F2(salary = 10000, rate = 0.01) # answer: 43

#(c)
F3 = function(price,salary){
  rate = 0.01 # raise in salary per 4 months
  portion = seq(0.01,1, by = 0.01) # possible percentage of salary for saving monthly
  i = 1
  month = 80
  while((month > 60)&(i <=100)){
    portion_save = portion[i]
    month = F2(price = price, salary = salary, rate = rate, portion_save)
    if (month>60){i = i+1}
  }
  
  return(portion_save) 
}
### Test function F3:
F3(price = 1200000, salary = 7000) # answer: 0.32
F3(price = 800000, salary = 4000) # answer: 0.35


########### TUTORIAL 4 ##########


##########   Q1
dat= read.table("C:/Data/Colleges.txt",header =TRUE,sep= "\t")
names(dat)
head(dat)

matrix <- function(x, y) {
  beta <- solve(t(x )%*% x )%*% t(x )%*% y
  return( beta )
}
matrix( x = cbind (1,dat$SAT),y = dat$Acceptance )

lm(Acceptance ~SAT , data =dat )


matrix( cbind (1, dat$SAT ,dat$Top.10p), dat$Acceptance )
# Compare outputs with lm()
lm(Acceptance ~ SAT +Top.10p, data = dat )


##########   Q2

house = read.csv("C:/Data/house_selling_prices_FL.csv")
names(house) # names of columns
dim(house) # 100 observations and 9 columns
house$NW = as.factor(house$NW) # to declare that NW is categorical
attach(house)


#(a)
cor(price, size)

#(b)
plot(size, price, pch = 20)

#(c)
M1 = lm(price ~ size, data = house)

summary(M1)


#(d)
M2 = lm(price ~ size + NW, data = house)
summary(M2)


#(f)
predict(M2, newdata=data.frame(size=4000, NW = "1"))


########### TUTORIAL 5

#Q1 CRAB DATA SET

data<-read.csv('C:/Data/crab.csv')#, header=T)
head(data)
data$spine = as.factor(data$spine)
table(data$spine)
attach(data)

plot(width,weight, type = "n")
points(width[which(spine==1)],weight[which(spine==1)],pch = 20, col = "black")
points(width[which(spine==2)],weight[which(spine==2)],pch = 6, col = "red")
points(width[which(spine==3)],weight[which(spine==3)],pch = 10, col= "blue")
legend(22, 5, legend = c("Spine = 1", "Spine = 2", "Spine = 3"), 
       col = c("black", "red", "blue"), pch = c(20, 6, 10))


M = lm(weight ~ width + spine, data = data)
summary(M)


# Q3
#(FPR, TPR) = (0.4, 1), (0.4, 0.4), (0, 0.4)

x = c(0.4, 0.4, 0)
y = c(1, 0.4, 0.4)
plot(x,y, type = "n", xlab = "FPR", ylab = "TPR", ylim = c(0,1), xlim = c(0,1))
points(0.4,1, pch = 10, col = "red") # sigma = 0.3
points(0.4,0.4, pch = 10, col = "blue") # sigma = 0.6
points(0,0.4, pch = 10, col = "black") # sigma = 0.8
legend(0.6, 0.3, legend = c("sigma = 0.3", "sigma = 0.6", "sigma = 0.8"), 
       col = c("red", "blue","black"), pch = c(10, 10, 10))




# Q4

caravan = read.csv("C:/Data/Caravan.csv")
head(caravan)
dim(caravan) # 87 columns, the first column can be ignored
#description: 85 predictors to measure demographic characteristics.
# Response = Purchase, indicates whether or not an individual purchase a caravan insurance policy.


#(a)
table(caravan$Purchase)
table(caravan$Purchase)[2]/sum(table(caravan$Purchase)) #6%
# dataset has 6% of people purchased caravan insurance

#(b)
caravan=caravan[,-1] # remove the first column since it's of no information

#SCALING THE INPUT FEATURES
standardized.X= scale(caravan[,-86]) # scaling all the features, except the last column = RESPONSE


# (c)

set.seed (5)

n = dim(caravan)[1] # sample size = 5822

test = sample(1:n, 2000) # sample a random set of 2000 INDEXES, ranging from 1:n.
train.X=standardized.X[-test ,] #training set of features
test.X =standardized.X[test ,]  # test set of features
train.Y=caravan$Purchase[-test] # response for training set
test.Y =caravan$Purchase[test] # response for test set


# (d) 


knn.pred = knn(train.X,test.X,train.Y,k=1) # KNN with k = 1
confusion.matrix=table(test.Y,knn.pred)
confusion.matrix # Yes is in the second column/row  = POSITIVE
precision = confusion.matrix[2,2]/sum(confusion.matrix[,2])
precision



knn.pred = knn(train.X,test.X,train.Y,k=3) # KNN with k = 3
confusion.matrix=table(test.Y,knn.pred)
confusion.matrix # Yes is in the second column/row = POSITIVE
precision = confusion.matrix[2,2]/sum(confusion.matrix[,2])
precision


knn.pred = knn(train.X,test.X,train.Y,k=5) # KNN with k = 5
confusion.matrix=table(test.Y,knn.pred)
confusion.matrix
precision = confusion.matrix[2,2]/sum(confusion.matrix[,2])
precision



##########  GERMAN CREDIT DATA SET

library(class)


credit = read.csv("C:/Data/German_credit.csv")
dim(credit)
head(credit)

library(base)

set.seed (1)

# Qb
# scaling data

credit[ ,2:5] = scale(credit[ ,2:5])

# credit[ ,2:5] = apply(credit[ ,2:5], 2, scale )# margin = 2 for columns, 1 = for rows
# OR another way as below
# credit[ ,2:5] = lapply(credit[,2:5], scale)


# Qc
train = sample (1:1000 , 800); #randomly sample a set of 800 indexes in 1:1000
train.data = credit[train,] # 800 data points for the train set
test.data = credit[-train,] # 200 data points for the test set


train.x = train.data[ ,2:5]
test.x = test.data[ ,2:5]
train.y = train.data[ ,1]
test.y = test.data[ ,1]

# Qd

knn.pred = knn(train.x, test.x, train.y,k=1)
confusion.matrix = table(knn.pred , test.y)
confusion.matrix
accuracy = sum(diag(confusion.matrix))/sum(confusion.matrix); accuracy


# Qe
n_folds=5 # each fold has 200 data points

folds_j <- sample(rep(1:n_folds, length.out = dim(credit)[1] )) 

table(folds_j)

X = credit[,2:5] # input features
Y = credit[,1] # response

acc=numeric(n_folds) # to store the accuracy for each iteration of n-fold CV

for (j in 1:n_folds) {
  test_j <- which(folds_j == j) # get the index of the points that will be in the test set
  pred <- knn(train=X[ -test_j, ], test=X[test_j, ], cl=Y[-test_j ], k=1) # KNN with k = 1, 5, 10, etc
  
  acc[j]=mean(Y[test_j] == pred) 
  # this acc[j] = sum(diag(confusion.matrix))/sum(confusion.matrix), where confusion.matrix=table(Y[test_j],pred)
}

mean(acc)




# Qf

K = 100 # can try KNN with k = 1,2,...K.

accuracy=numeric(K) # to store the average accuracy of each k.

acc=numeric(n_folds) # to store the accuracy for each iteration of n-fold CV

for (i in 1:K){
  
  for (j in 1:n_folds) {
    test_j <- which(folds_j == j) # get the index of the points that will be in the test set
    pred <- knn(train=X[ -test_j, ], test=X[test_j, ], cl=Y[-test_j ], k=i) # KNN with k = 1, 5, 10, etc
    
    acc[j]=mean(Y[test_j] == pred) 
    # this acc[j] = sum(diag(confusion.matrix))/sum(confusion.matrix), where confusion.matrix=table(Y[test_j],pred)
  }
  accuracy[i] = mean(acc)
  
}

max(accuracy)
sort(accuracy)[98:100] # the three largest accuracy
index = which(accuracy == max(accuracy)) ; index # give index which is also the value of k.

plot(x=1:100, accuracy, xlab = "K")
abline(v = index, col = "red", )


##################  IRIS DATA SET

#install.packages("rpart")
#install.packages("rpart.plot")

library("rpart")
library("rpart.plot")

iris = read.csv("C:/Data/iris.csv")
head(iris)
names(iris) = c("Sepal.Length", "Sepal.Width","Petal.Length","Petal.Width","Species")



fit.iris <- rpart(Species ~ .,
                  method = "class", data =iris, control = rpart.control( minsplit =1),
                  parms = list( split ='gini'))
#split = 'information' or 'gini' 


rpart.plot(fit.iris , type =4, extra =2, clip.right.labs =FALSE , varlen =0, faclen =0)

# EXTRA: predict if a new observation that has measurement of:
# sepal.length = 5.1, sepal.width = 3.3, petal.length = 1.6, petal.width = 0.2
# 

#code:
predict(fit.iris, newdata = data.frame(Sepal.Length = 5.1, Sepal.Width = 3.3, 
                                       Petal.Length = 1.6, Petal.Width = 0.2), type = 'class')
#answer: that observation should be Iris-setosa.


# predict for this observation:
#sepal.length = 5.1, sepal.width = 3.3, petal.length = 3, petal.width = 1.8

predict(fit.iris, newdata = data.frame(Sepal.Length = 5.1, Sepal.Width = 3.3, 
                                       Petal.Length = 3, Petal.Width = 1.8), type = 'prob') # get probability

#answer:   Iris-setosa Iris-versicolor Iris-virginica
#   1           0      0.02173913      0.9782609







# EXTRA VISUALIZATION by ggplot2 package
library(ggplot2)
library(magrittr)
# sepal width vs. sepal length
ggplot(iris , aes(x=sepal.length, y=sepal.width, color =species)) +
  geom_point ()+
  labs (x = "sepal length")+ labs (y = "sepal width")

# petal width vs. petal length
ggplot(iris , aes(x=petal.length , y=petal.width , color =species)) +
  geom_point ()+
  labs(x = "petal length")+ labs (y = "petal width")


# OR using a usual way:

# sepal width vs. sepal length

attach(iris)
plot(sepal.length, sepal.width, type = "n")
points(sepal.length[species=="Iris-setosa"],sepal.width[species=="Iris-setosa"], pch = 20, col = "darkgreen")
points(sepal.length[species=="Iris-versicolor"],sepal.width[species=="Iris-versicolor"], pch = 20, col = "red")
points(sepal.length[species=="Iris-virginica"],sepal.width[species=="Iris-virginica"], pch = 20, col = "blue")
legend(6.3,4.48, legend=c("setosa", "versicolor", "virginica"), col = c("darkgreen", "red", "blue"), pch = c(20,20,20))



# petal width vs. petal length

plot(petal.length, petal.width, type = "n")
points(petal.length[species=="Iris-setosa"],petal.width[species=="Iris-setosa"], pch = 20, col = "darkgreen")
points(petal.length[species=="Iris-versicolor"],petal.width[species=="Iris-versicolor"], pch = 20, col = "red")
points(petal.length[species=="Iris-virginica"],petal.width[species=="Iris-virginica"], pch = 20, col = "blue")
legend(5,0.6, legend=c("setosa", "versicolor", "virginica"), col = c("darkgreen", "red", "blue"), pch = c(20,20,20))


library("rpart")
library("rpart.plot")

########## CHURN DATA SET  (DECISION TREE)


# (i) Age (years)
# (ii) Married (true/false)
# (iii) Duration as a customer (years)
# (iv) Churned contacts (count)-Number of the customer's contacts
# that have churned (count)
# (v) Churned (true/false)-Whether the customer churned


churn = read.csv("C:/Data/churn.csv")
head(churn)

summary(as.factor(churn$Churned))

#Remove ID column
churn= churn[,-1]

#churn.X = churn[,-1] # only the input features




fit<- rpart(Churned ~ Age + Married + Cust_years + Churned_contacts,
            method ="class",
            data = churn ,
            control = rpart.control(minsplit = 1),
            parms = list(split ='information'))

rpart.plot(fit , type =4, extra =2, clip.right.labs = FALSE , varlen =0, faclen =0)


# minsplit = 20 is by default. But we can change it.
# Equivalently, we can use cp instead of minsplit.
# cp =0.01 is by default, we can change it. 
# Smaller cp makes the tree be bigger, more complex.
# Try cp = 0.001, 0.002, 0.005



age = c(26,23,56,36,45,28,22,22,60,32)
married = c(1,1,1,1,0,0,1,0,1,0)
cust =c(2,3,5,5,2,2,3,3,2,3)
contact = c(2,3,2,2,1,2,0,2,1,1)

head(churn)

new = data.frame(Age = age,Married = married, Cust_years = cust,Churned_contacts = contact)
new

pred = predict(fit, newdata = new, type = 'class'); pred
# Answer:  1  1  0  0  0  1  1  1  0  0 if we used minsplit = 1 for the decison tree
# Answer:  1  1  0  0  0  1  1  1  0  0 if we used cp = 0.002 for the decison tree
# Answer:  1  1  0  0  0  1  1  1  0  0 if we used cp = 0.004 for the decison tree
# Answer:  1  1  0  0  0  1  1  1  0  0 if we used cp = 0.005 for the decison tree





########## IRIS DATA SET  (DECISION TREE & N-FOLD CV)

library(rpart)

set.seed(555)

iris = read.csv("~/Github/DSA1101 Slayers/datasets/iris.csv")
head(iris)
names(iris)

n_folds=5 #

folds_j_1 <- sample(rep(1:n_folds, length.out = 50 ))  # for type 1 = setosa

folds_j_2 <- sample(rep(1:n_folds, length.out = 50 ))  # for type 2 = versicolor

folds_j_3 <- sample(rep(1:n_folds, length.out = 50 ))  # for type 2 = virginica


table(folds_j_1)
table(folds_j_2)
table(folds_j_3)

data1 = iris[1:50,] # data for type 1 = setosa
data2 = iris[51:100,] # data for type 2 = versicolor
data3 = iris[101:150,] # data for type 3 = virginica

acc=numeric(n_folds)

j= 1

for (j in 1:n_folds) {
  
  test1 <- which(folds_j_1 == j)
  test2 <- which(folds_j_2 == j)
  test3 <- which(folds_j_3 == j)
  
  train.1=data1[ -test1, ]
  train.2=data2[ -test2, ]
  train.3=data3[ -test3, ]
  
  train = rbind(train.1, train.2, train.3) # this is the training data set
  
  test = rbind(data1[test1,], data2[test2,], data3[test3,] ) # test data 
  
  fit.iris <- rpart(class ~ .,
                    method = "class", data =train, control = rpart.control( minsplit =1),
                    parms = list( split ='information'))
  
  
  pred = predict(fit.iris, newdata = test[,1:4], type = 'class')
  
  confusion.matrix = table(pred, test[,5])
  
  acc[j] = sum(diag(confusion.matrix))/sum(confusion.matrix)
  
}
acc
mean(acc) # the accuracy is very high, 0.94


test = c(1, 2, 3,5, 7)




#### BANK-SAMPLE DATA SET

########## N-FOLD CROSS-VALIDATION FOR DECISION TREE  
library(rpart) 
library(rpart.plot)

banktrain <- read.csv("~/Github/DSA1101 Slayers/datasets/bank-sample.csv", header=TRUE)
dim(banktrain)

## drop a few columns to simplify the tree
drops<-c("age", "balance", "day", "campaign", 
         "pdays", "previous", "month", "duration")
banktrain <- banktrain [,!(names(banktrain) %in% drops)]

head(banktrain)

## total records in dataset
n=dim(banktrain)[1]; n

length(which(banktrain[,9] =="yes")) # 211 out of 2000 customers subscribed.


## We'll randomly split data into 10 sets of (about) equal size
# regardless of percentage of "yes" in each set.

n_folds=10
folds_j <- sample(rep(1:n_folds, length.out = n))
# this is to randomly sample the indexes of subsets for the observation
#table(folds_j)

cp=10^(-5:5); length(cp)
misC=rep(0,length(cp)) # a vector to record the rate of mis-classification for each cp

for(i in 1:length(cp)){
  misclass=0
  for (j in 1:n_folds) {
    test <- which(folds_j == j)
    train=banktrain[-c(test),]
    fit <- rpart(subscribed ~ job + marital + 
                   education +d efault + housing + 
                   loan + contact+poutcome, 
                 method="class", 
                 data=train,
                 control=rpart.control(cp=cp[i]),
                 parms=list(split='information'))
    
    new.data=data.frame(banktrain[test,c(1:8)])
    ##predict label for test data based on fitted tree
    pred=predict(fit,new.data,type='class')
    misclass = misclass + sum(pred!=banktrain[test,9])
  }
  misC[i]=misclass/n
}

plot(-log(cp,base=10),misC,type='b')

## determine the best cp in terms of
## misclassification rate



best.cp =cp[which(misC == min(misC))] ; best.cp
# 0.01
# this is the value of cp that gives the lowest mis-classification rate

## Fit decision tree with that smallest cp
fit <- rpart(subscribed ~ job + marital + education+default + housing + loan + contact+poutcome, 
             method="class", 
             data=banktrain,######
             control=rpart.control(cp=best.cp),
             parms=list(split='information'))

# to get the tree plotted:
rpart.plot(fit, type=4, extra=2, clip.right.labs=FALSE, varlen=0)#, faclen=3)

##TUTORIAL 8

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


##TUTORIAL 10

# Q1
x1 = c(1, 1.5, 3, 3.5, 4.5)
x2 = c(1,2,4,5,5)

plot(x1, x2, pch = 20, col = "blue")

text(1.1,1.1,"A")
text(1.6, 2.2, 'B')
text(3.1, 4.1, 'C')
text(3.63, 5, 'D')
text(4.35, 5, 'E')

# Adding the starting centroids 
points(2,2, pch = 2, col = 'red')
text(2.2, 2.1, 'C-P')
points(4,4, pch = 10, col = 'darkgreen')
text(4,3.8, 'C-Q')

# Adding the new centroids after the first iteration:
points(1.25, 1.5, col = 'red', pch = 2)
text(1.35, 1.4, 'C-P-new')
points(11/3, 14/3, col = 'darkgreen', pch = 10)
text(11/3, 4.5, 'C-Q-new')



data = data.frame(x1, x2)
data
kout = kmeans(data, centers = 2)
kout$withinss
kout$tot.withinss

# Q2

data = read.csv("~/Github/DSA1101 Slayers/datasets/hdb-2012-to-2014.csv")

dim(data)
names(data)

attach(data)

plot(floor_area_sqm, resale_price, pch = 20)




#########  IT IS RECOMMENDED TO STANDARDIZE THE INPUT FEATURES BEFORE K-MEANS



# PLOT WSS vs K TO PICK OPTIMAL K:

K = 15 
wss <- numeric(K)

for (k in 1:K) { 
  wss[k] <- sum(kmeans(scale(data[,c("floor_area_sqm","resale_price")]), centers=k)$withinss)
}


plot(1:K, wss, col = "blue", type="b", xlab="Number of Clusters",  ylab="Within Sum of Squares")

# k=3 might be a good choice.


# k = 3 groups
kout <- kmeans(scale(data[,c("floor_area_sqm","resale_price")]),centers=3)

# visualize the 3 groups:

plot(data$floor_area_sqm, 
     data$resale_price, 
     col=kout$cluster)

# THE CENTROIDS OF THE THREE CLUSTERS:

kout$centers

#  floor_area_sqm resale_price
#1      0.2619077   0.09266398
#2      1.6425666   1.98456430
#3     -1.0531087  -0.90274280


# THE SIZE (NUMBER OF POINTS) IN EACH CLUSTER:

kout$size
#[1] 3297  754 1996
