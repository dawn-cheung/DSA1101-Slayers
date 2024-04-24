setwd("C:/Data")

set.seed(803)



##################################### Part I: DATA PREPARATION

# Q1 (2 points)
data = read.csv("abalone2.csv")


# Q2 (2 points)
names(data)
# [1] "sex"      "length"   "diameter" "weight"   "age"

# Q3 (2 points)

data$Year = ifelse(data$age <=10.5, "young", "old")

attach(data)



##################################### Part II: DATA EXPLORATION

# Q4 (2 points)
# Write code to create a frequency table for variable \textbf{Year} created above. 
# Report the number of abalones that are in ``old'' group.

table(Year) # 2081 old abalones


# Q5 (4 points) Create a QQ plot for the sample of abalone's age. 
# Give your comments.

qqnorm(age, pch = 20)
qqline(age, col = "red")
# right tail LONGER than normal
# left tail is normal
# in general, distribution of age is NOT normal


# Q6 (4 points) Create a box plot for the sample of abalone's age. 
# Does it show any outliers? If yes, how many outliers are there?

boxplot(age)

# there are many outliers (both above and below median
#identifying outliers:
out = boxplot(age)$out
out # all the outliers from the boxplot, including large and small outliers
length(out)

# 278 outliers in total



# Q7 (6 points) Find the mean weight of all the abalones that are the large outliers in the box plot of age above. 
#Compared to the mean weight of all abalones in the data set, what's is the difference? 
#Give your comments.

index = which(age %in% out) # find the indexes of age that are matching with values of outliers
outliers = data[index,] # information of all the outliers (including small and large outliers

outliers

large.outliers = outliers[-which(outliers$Year == "young"),]
# remove all the YOUNG/SMALL outliers from the set of all outliers


mean(large.outliers$weight)

#Mean weight of LARGE outliers = 240.772 grams

mean(weight) # mean weight of all 4177 abalones from the data = 165.748 grams

mean(large.outliers$weight) - mean(weight) 

# difference = 75.023 grams
# comments: those abalones that are heavier than average weight might be older in age



# Q8 (2 points) Create a scatter plot of \textbf{age} against \textbf{weight}. 
# Give your comments.


plot(weight, age)

# there is a positive relationship; might be linear
# the variability of age is NOT STABLE when weight changes. the plot has FUNNEL shape



##################################### Part III: LINEAR MODEL



# Q9 (4 points) Fit a linear regression model for the response variable \textbf{age}, named as 
# \textbf{M1}, using all input features, \textbf{sex}, \textbf{length}, \textbf{diameter} and \textbf{weight}. 
# Report p-values of the regressors that are NOT significant in the model, at significance level 0.1.



M1 = lm(age ~ sex + length + diameter + weight, data = data)

summary(M1)

# for your reference on how to write the fitted model:
# age-hat = 6.23 - 1.2*I(sex = Infant) - 0.18* I(sex = Male) - 0.052 *length +0.132*diameter + 0.0018*weight


# Variable weight is NOT significant, its p-value = 0.1049 > 0.1.



# Q10 (4 points) Give your comments on the suitability to fit a linear model for \textbf{age}.

# It is NOT suitable to fit a linear model for Age, since 
# (1) Age is not symmetric
# (2) the variability of age is NOT STABLE (increases) when weight changes (increases). The plot has FUNNEL shape


# Q11 (2 points) An abalone has information listed below. Write code to predict the age of this abalone. 
# \texttt{sex} = M, \texttt{length} = 120 mm, \texttt{diameter} = 90 mm, and \texttt{weight} = 240 grams.



new.point = data.frame(sex = "M", length = 120, diameter = 90, weight = 240)

predict(M1, newdata = new.point)
# 12.164 # years in age


##################################### Part IV: KNN


# Q12 (6 points) Use all the observations given with standardized numeric input features to form the KNN classifiers 
# where 11 <= K <= 50.


library(class)

type.2 = numeric()  # to store the type 2 error of the classifier for each k from 11 to 50

standardized.X = scale(data[,2:4])  # STANDARDIZING 3 FEATURES, NOT INCLUDING SEX


for (i in 11:50){

	pred <- knn(train=standardized.X, test=standardized.X, cl=data[,6], k=i) # KNN with k = 11,..., 50

      confusion.matrix = table(data[,6], pred)
      type.2 = append(type.2, confusion.matrix[1,2]/(confusion.matrix[1,1] + confusion.matrix[1,2]) )

      }

#OLD = POSITIVE; YOUNG = NEGATIVE
# type 2 error = FN / (TP + FN) # CHECK SLIDE 29/49 IN THE TOPIC 4 PDF NOTES




# Q13 (4 points) Write code to produce a scatter plot of Type 2 Error, type.2 against values of
# k in the question above.

type.2

plot(11:50, type.2, xlab = "k", ylab = "type 2 error") 


# Q14 (4 points) Based on Type 2 Error, report the best k found and the Type 2 Error rate of
# the KNN classifier with that k.


which(type.2 == min(type.2)) # the value of k may vary
# such as k = 45 or 50
# type 2 error is around  0.2037482
# Please NOTE: vector type.2 has 40 values for k = 11 to 50.






##################################### Part V: DECISION TREE




# Q15 (4 points) Write code to form a decision tree, named as \textbf{fit}, 
# with complex parameter $cp = 0.003$ and Information Gain for splitting the branches.


library(rpart)
library(rpart.plot)


##  fittting a tree using the full data set given, with cp = 0.003

fit = rpart(Year ~ sex + length + diameter + weight, 
             	method="class", 
             	data=data,
             	control=rpart.control(cp=0.003),
             	parms=list(split='information'))



# Q16 (4 points) Plot the tree \textbf{fit}. 
# Based on this plot, predict if the abalone in Question 10 is ``young'' or ``old''.



rpart.plot(fit, type=4, extra=2, clip.right.labs=FALSE, varlen=0, faclen = 0)

# from the plot: the abalone with weight = 240grams is predicted as "old" (left branch of the tree).


# Q17 (4 points) Find and report the accuracy of \textbf{fit}

# USING predict FUNCTION:
new = data[,1:4] # all the 4 features from the data set, no response age or Year.
prediction = predict(fit, new.data = new,type='class') #this is a fucking lie use newdata instead of new.data unless uk what ur doing

confusion.matrix = table(prediction, Year)
confusion.matrix

accuracy = sum(diag(confusion.matrix))/sum(confusion.matrix)
accuracy # 0.7457505



