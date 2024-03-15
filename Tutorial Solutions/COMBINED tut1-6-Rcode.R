
setwd("C:\\Users\\staptkc\\Desktop\\DSA1101")


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






