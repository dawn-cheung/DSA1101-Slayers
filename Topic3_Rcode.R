
########## TOPIC 3 - LINEAR REGRESSION ###########

setwd("C:/Data")

resale = read.csv("hdbresale_reg.csv")
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

hdb.model = lm(price~area)

summary(hdb.model)


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

set.seed(250)
x1 = rnorm(100) 
x2 = rnorm(100) 
y = 1 + 2*x1 -5*x2+ rnorm(100)
lm(y~x1+x2)

#instal.packages("rgl")
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

resale = read.csv("C:/Data/hdbresale_reg.csv")
years_left = 2022 - resale$lease_commence_date
price = resale$resale_price
area = resale$floor_area_sqm

M1 = lm(price ~ area)
summary(M1)

M2 = lm(price ~ area + years_left)
summary(M2)






