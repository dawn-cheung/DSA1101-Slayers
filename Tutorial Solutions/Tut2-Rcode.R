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


