##TOPIC 2 MY OWN WORKING SPACE LOL


library(tidyverse)
library(dplyr)

setwd("~/GitHub/DSA1101 Slayers")
sales <- read.csv("~/GitHub/DSA1101 Slayers/datasets/yearly_sales.csv") #OK YAY THIS WORKS even if its in another file WOOO
head(sales)

#focusing on sales total

total = sales$sales_total

#summary of the center: mean, median, mode
n = length(total) #sample size
summary(total)

#summary of the variability
range(total) #returns min and max values, on the same line
#ie [1]   30.02 7606.09

var(total) #returns variance
#measures how far a set of numbers are spread out from their average

sd(total) #returns standard deviation
#measures spread of data too-- literally this is the sqr root of variance

#so Variance = SD**2

IQR(total) #Inter QUARTILE range
#u chop the data into 4 parts, and take the middle 2 parts


#Inferences from numerical summaries:

#1) if the mean is the same as the median, the sample is close to symmetrical 
#2) mean is sensitive to outliers, median is not
#3) mean > median == sample is right skewed
#   mean < meidan == sample is left skewed

hist(total, freq = FALSE, main = paste("Histogram of total sales"),
     xlab = "total",
     ylab = "Frequency",
     col = "blue",
     ylim = c(0, 0.0045))
lines(density(total), col = "red")

#for left skewed histograms, the left tail is longer than the right tail
#ie theres more values on the left edge than the right edge, some vid said the left side looks more squished than the right side

#vice versa for right skew

qqnorm(total, main = "QQ Plot", pch = 20)
qqline(total, col = "red") #this makes the x=y line

order = sales$num_of_orders
cor(total, order)

#for correlation value r,
#positive value -> positive association
#negative value -> negative association
#closer ther absolute value of r to 1-> stronger association between X and Y

#From lecture
#a = round(variable, didgits = 2)
#others used: total, length, summary, be clear on hist parameters

# HISTOGRAM WITH NORMAL DENSITY 
hist(total, freq=FALSE, main = paste("Histogram of Total Sales"),
     xlab = "total sales", ylab="Probability", 
     col = "grey", ylim = c(0, 0.002))
x <- seq(0, max(total)) #she literally said "if u no stats bg i cannot help u, just follow"
y <- dnorm(x, mean(total), sd(total))
lines(x, y, col = "red") # this is the normal density curve

outlier = boxplot(total)$out
length(outlier)

total[9816]
index = which(total %in% outlier) 
index

sales[c(index),]


###TUTORIAL 2 STARTS HERE
Forced_Expiratory_Volume <- read.csv("~/GitHub/DSA1101 Slayers/datasets/FEV.csv")
fev_var <- Forced_Expiratory_Volume$FEV
attach(Forced_Expiratory_Volume)
#1a) Response variable: FEV measurement

hist(fev_var, freq = FALSE, main = paste("Histogram of Forced Expiratory Volume (FEV)"),
     xlab = "FEV",
     ylab = "Frequency",
     col = "blue",
     ylim = c(0, 0.5))
lines(density(fev_var), col = "red")

#1b) The histogram is right skewed, more values to the right of the median than the left.
# The range of FEV values is from 1 to 6.
# The modal value is 2(???)
# FEV does not follow a normal distribution because it is not symmetrical

# 1c)
boxplot(fev_var, xlab = "FEV", col = "blue")
boxplot(fev_var) #is fine too lol its just "less fancy"

outtie  = boxplot(fev_var, xlab = "FEV", col = "blue")$out
outtie

indexes = which(fev_var %in% c(outtie))
fev_var[c(indexes)]

# There are some outliers, but marjority of datapoints are below 4.7
# Right skewed. Median is around 2.5
# Unimodal


qqnorm(fev_var, pch = 10)
qqline(fev_var, col = "red")

# 1d) Generally, yes. Becos unimodal. But quite right skweded

#1e)
# Create separate histograms for male and female FEV, then obtain separate numerical summaries
# for males and female FEV. Comment on what you observe

#man im tired, heres the Unhinged way
female_FEV <- Forced_Expiratory_Volume %>%
  select(FEV, Sex) %>%
  filter(Sex == 1)
female_FEV <- female_FEV$FEV

#heres the less unhinged way
female_FEV <- Forced_Expiratory_Volume$FEV[Forced_Expiratory_Volume$Sex == 1]

male_FEV <- Forced_Expiratory_Volume$FEV[Forced_Expiratory_Volume$Sex == 0]

FEV <- fev_var

#EVEN MORE LESS UNHINGED:
female = FEV[which(Sex == 0)] #or FEV[Sex == 0] works also
male = FEV[which(Sex == 1)]

#TO PRINT GRAPHS TGT/SIMULTANIOUSLY
opar <- par(mfrom = c(1,2))

hist(female_FEV, freq = FALSE, main = paste("Histogram of Forced Expiratory Volume (FEV) by 1"),
     xlab = "FEV",
     ylab = "Frequency",
     col = "blue",
     ylim = c(0, 0.6))


hist(male_FEV, freq = FALSE, main = paste("Histogram of Forced Expiratory Volume (FEV) by 0"),
     xlab = "FEV",
     ylab = "Frequency",
     col = "red",
     ylim = c(0, 0.6),
     xlim = c(0, 5))


#Comments
#Female FEV has a larger range than male FEV
#Male modal value (2.5) slightly higher than Female (2)
#Male is left skew, Female is right skew

#1f) Create a scatterplot with height (in metres) on the x-axis and FEV on the y-axis
heighty = Forced_Expiratory_Volume$height

plot(heighty, fev_var, pch = 20, col = "darkblue")

opar <- par(mfrom = c(1,2))

plot(heighty, fev_var, type = "n")
points(female ~ height[which(Sex==0)], col = "red", pch = 20)
points(male ~ height[which(Sex==1)], col = "red", pch = 20)
legend(1.2, 5, legend = c("Female", "Male"), col = c("red", "darkblue"), pch = c(20,20))
#WHATEVER it doesnt work but thats the code from the tut


#1g) Create a scatterplot with height (in metres) on the x-axis and FEV on the y-axis

cor(heighty, fev_var) #0.8675619

#There is a positive association btwn height and FEV bc corr vairable is positive
#Since corr variable is close to 1, there is a strong association between height and FEV

#try using points() too

#2a)
Fibo = 0
F1 = 1
F2 = 1
for (i in 1:45) {
  Fibo = F1 + F2
  F1 = F2
  F2 = Fibo
  print(Fibo)
}

#BETTER WAY: USE LISTS to hold all Fibo numbers
Fibo[1:2] = 1
for (i in 3:45){
  Fibo[i] = Fibo[i-1] + Fibo[i-2]
}
print(Fibo)
print(Fibo[40])


n = sum(Fibo <= 5000000) + 1; n

n = max(which(Fibo<=5000000)) + 1; n

#2b) 

Fibo = 0
F1 = 1
F2 = 1
for (i in 1:45) {
  Fibo = F1 + F2
  F1 = F2
  F2 = Fibo
  if (i == 40) {
    print(Fibo)
    break
  }
}

Fibo = 0
F1 = 1
F2 = 1
count = 0
while (Fibo < 5000000) {
  Fibo = F1 + F2
  F1 = F2
  F2 = Fibo
  count = count + 1
}
paste0("smallest n is ", count)








