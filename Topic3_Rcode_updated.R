
########## TOPIC 3 - LINEAR REGRESSION ###########


resale = read.csv("C:/Data/hdbresale_reg.csv")
head(resale[ ,2:7]) # 1st column indicates ID of flats

head(resale[ ,8:11])

########## MODELLING

x = c( -1, 3, 5)
y = c( -1, 3.5 , 3)
lm(y~x)

########## PREDICTING

M = lm(y~x)             # M = name of the fitted model
new = data.frame(x = 2) # create dataframe of new point
predict(M, newdata = new)



########## MODEL FOR HDB RESALE FLATS

price = resale$resale_price
area = resale$floor_area_sqm
lm(price~area)$coef # coefficients of the model

########## RSE
#Calculating RSE:
sqrt(sum((y - M$fitted)^2)/(length(y) - 2))

summary(M) # Take the Residual standard error

########## R^2

TSS = var(y)*(length(y) -1) # or
TSS = sum((y- mean (y)) ^2)

RSS =sum((y- M$fitted )^2)

R2 = 1 - RSS/TSS; R2


summary(M)$r.squared

########## MULTIPLE LINEAR MODEL

set.seed(520)
x1 = rnorm(100) 
x2 = rnorm(100) 
y = 1 + 2*x1 -5*x2+ rnorm(100)
lm(y~x1+x2)

#instal.packages("rgl") # THIS IS OPTIONAL, NOT COMPULSORY TO HAVE 3D PLOT BELOW
library(rgl)
M.2 = lm(y~x1+x2)
# 3D plot to illustrate the data points
plot3d (x1 , x2 , y, xlab = "x1", ylab = "x2", zlab = "y",
       type = "s", size = 1.5 , col = "red")

coefs = coef(M.2)
a <- coefs[2] # coef of x1
b <- coefs[3] # coef of x2
c <- -1       # coef of y in the equation: ax1 + bx2 -y + d = 0.
d <- coefs[1] # intercept
planes3d (a, b, c, d, alpha = 0.5) # the plane is added to the plot3d above.


########### MLR for HDB RESALE FLATS

resale = read.csv("~/GitHub/DSA1101 Slayers/datasets/hdbresale_reg.csv")
resale$age = 2024 - resale$lease_commence_date # AGE OF THE FLAT to 2024
# create a new column for "resale" to calculate the age of the flat till year 2024


M1 = lm(resale_price ~ floor_area_sqm, data = resale)
summary(M1)

M2 = lm(resale_price ~ floor_area_sqm + age, data = resale)
summary(M2)

#Coefficients:
#                Estimate    Std. Error t value Pr(>|t|)    
#  (Intercept)    74412.16    5684.37  13.091   <2e-16 ***
#  floor_area_sqm  3254.58      32.28 100.828   <2e-16 ***
#  age              830.45      99.28   8.365   <2e-   ***

#so eqn of M2 is:
# (hat)price = 74412.16 + 3254.58floor_area_sqm + 830.45age


#############  DIVIDING FULL DATASET INTO TRAIN SET AND TEST SET RANDOMLY

n = dim(resale)[1] # total number of rows/observations

# first, mix up all the indexes of the full data
# then, take the first 80% of those mixed indexes as indexes of train data

sample(1:n) #this jumbles up the numbers from 1 to n lol

#> sample(1:20)
#[1]  9  5 20 11 18  6 12  3  8  4  2 16 17 13  7 19 14 10 15  1

index.train = sample(1:n)[1:(0.8*n)] #the [1:(0.8*n)] takes 80% of the scrambled numbers

train.data = resale[index.train, ]


test.data = resale[ - index.train, ] #rows not inside the index.train




# forming model based on the train set:

M3 = lm(resale_price ~ floor_area_sqm + age, data = train.data)
summary(M3)


# predict the response for test set:

prediction = predict(M3, test.data)
prediction

cbind(prediction, test.data$resale_price)
# the prediction using model M3 is the first column
# second column is the actual price





##########  ADDING CATEGORICAL VARIABLE TO THE MLR MODEL
# FLAT TYPE has 5 categories

  
  M4 = lm(resale_price ~ floor_area_sqm + age + flat_type, data = resale)
  summary(M4)

# R chooses "2 ROOM" as reference category
# coefficient 26753.48 of "3 ROOM" means:
# fixing other variables in the model, compared to a 2-ROOM flat then
# on average, a 3-ROOM flat is more expensive by $26753.48




######### FITTED VALUES OF A LINEAR MODEL

# MODEL M2
M2$fitted.values
# these are all the prediction for the data set used to form the model
cbind(prediction, M2$fitted.values)

# MODEL M3
M3$fitted.values






############ ASSUMPTIONS OF RESPONSE TO FORM A LINEAR MODEL
# response should be symmetric (1); variability of response is stable when regressors change (2)
# how to check for (1)?
# At our level, checking the symmetricity by histogram would be enough

 hist(resale$resale_price) # if we plan to use full data to form model

 hist(train.data$resale_price) # if we plan to use train.data to form the model

# in any case, the response is NOT symmetric, very right skewed.
# hence, it's NOT SUITABLE to fit a linear model for resale price.

# TRANSFORMATION IS BETTER, such as taking log-e, or sqrt.


hist(log(resale$resale_price)) # slightly better, more symmetric

# hence, fitting a linear model for the log-e of the price is better than 
# fitting a LM for the price itself.




# HOW TO CHECK FOR (2) = THE VARIABILITY OF RESPONSE BE STABLE WHEN REGRESSORS CHANGE?
# using scatter plot of y vs quantitative x
# bo FUNNEL shape is ok.


plot(resale$floor_area_sqm, resale$resale_price)

plot(resale$age, resale$resale_price)
