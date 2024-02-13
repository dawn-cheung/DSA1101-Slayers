#TUTORIAL STARTS ON WEEK 3 PLS (FOLLOW CANVAS)
#console gets the output: goes by session. can choose to save via image BUT usually we dont
#this window is called the script

#WTF is inversion of a matrix
#the inversion of 5 is 0.5, such that when you multiply each other u get 1
#this is NOT the case for matrixes

#[  ]
#[  ]

#vectors are like 1D matrices
#(1,2,3,4,5)*2 = (2,4,6,8,10)

##############  TOPIC 1 - INTRODUCTION TO R

# creating a vector of numbers:
number<-c(2,4,6,8,10); number 

# c() stands for concatenation

# creating a vector of strings/characters:
string<-c("weight", "height", "gender"); string

# creating a Boolean vector (T/F):
logic<- c(T, T, F, F, T); logic

#TRUE is treated as 1 numerically, similarly for FALSE
logic*5

#vectors are EITHER numeric OR string, if u add strings to a numeric vector it will all convert to strings

### FUNCTION numeric()

number.2<-numeric(3)
number.2

### APPENDING TWO VECTORS
c(number, number.2)


### FUNCTION rep()

# rep(a,b): replicate the item a by b times where a could be a number or a vectora
number.3<-rep(2,3); number.3

number.3<-rep(c(1,2),3)
number.3

rep(string,2)


### FUNCTION seq()

seq(from=2, to=10, by=2)

seq(2,10,2)

seq(from=2, to=10, length = 5)

seq(10) # a sequence from 1 up to 10, distance by 1



##WK2 LECTURE STARTS HERE
### FUNCTION matrix()

v <- c(1:6)
m <- matrix(v, nrow=2, ncol=3)
m # will fill up matrix by row
#ans:
#     [,1] [,2] [,3]
#[1,]    1    3    5
#[2,]    2    4    6

#to fill the matrix by rows:
m <- matrix(v, nrow=2, ncol=3, byrow=T) 
m



### FUNCTION rbind()
a <- c(1,2,3,4)
b <- c(5,6,7,8)
ab_row <- rbind(a,b) # such that each of them is 1 row
ab_row


### FUNCTION cbind()

ab_col <- cbind(a,b); 
ab_col
#     a b
#[1,] 1 5
#[2,] 2 6
#[3,] 3 7
#[4,] 4 8



### LIST IN R

list.1 <- list(10.5, 20, TRUE, "Daisy") #contains DIFFERENT MODES eg strings etc
list.1

x = c(2,4,6,8) # length 4, all are numeric
y = c(T, F, T) # length 3, all are logical/Boolean
list.2 = list(A = x, B = y) # assign names to list members
list.2
#$A
#[1] 2 4 6 8
#
#$B
#[1] TRUE FALSE  TRUE

#NOTE: the names A and B are optional. can be removed to be
#list.2 = list(x,y)
#[[1]]
#[1] 2 4 6 8
#
#[[2]]
#[1]  TRUE FALSE  TRUE


list.2[1] # reference by INDEX: use square brackets []

list.2$A # reference by NAME: use dollar sign($)
#[1] 2 4 6 8

list.2$A[1] #get a member in an item of the list
#[1] 2


list.1[1] # shows first item in the list but NOT CONSIDERED NUMERIC
#list.1[1] * 2 # error coz this is NOT numeric

list.1[[1]] # double the square brackets
list.1[[1]]*2 #now we have accessed the element inside the item of the list




###### DATAFRAME IN R

## MANUALLY CREATE A DATAFRAME

#creating a vector for each variable first 
height = c(1.6, 1.8, 1.7) # height of 3 people in meter
weight = c(46, 75, 68) # weight of them in kg

#the *order of values* must be the same, ie length of each vector is the same

HW = data.frame(height, weight)
HW

#    height weight
# 1    1.6     46
# 2    1.8     75
# 3    1.7     68

# this dataframe has 2 variables (2 columns) and 3 observations (3 rows)




## IMPORTING A FILE IN TO R AS A DATAFRAME

setwd("~/GitHub/DSA1101 Slayers") 

data1<-read.csv("crab.txt", sep = "", header = TRUE)
data1

#the header=TRUE is the names of the variables
#the names of the columns ie the headers are not counted as 1 row

data1[1:4,] #first 4 rows

names(data1) # names of columns 
#returns a character vector (each element is a header)


data1<-read.csv("crab.txt",sep = "", header = FALSE)
data1[1:4,] #first 4 rows to see what happens when we specify "header" wrongly


## FILE WITH NO HEADER

data2<-read.table("ex_1.txt", header = FALSE, sep = "") #these are the default commands WITH HEADER FALSE

data2

names(data2)

names(data2) = c("Subject", "Gender", "CA1", "CA2", "HW")


head(data2) # this helps us to see the first 6 rows of a dataframe
# names of columns have been changed


#### READ THE FILE "ex_1_comma.txt" into R:

#by read.csv(), we need to specify header = FALSE
data<-read.csv("ex_1_comma.txt", header = FALSE)


# or you could use read.table(), and must specify sep = ","
data<-read.table("ex_1_comma.txt", sep = "," )
data

###ASSESSING PARTS OF A DATAFRAME: NameOfDataframe[x,y]
#if x or y is not selected, then all columns are selected


##### 
data2<-read.table("ex_1.txt", header = FALSE, sep = "")

names(data2) = c("Subject", "Gender", "CA1", "CA2", "HW")

data2[2,] #2nd row, all columns
data2[,3] #all rows, 3rd column -> all CA1 scores

#OR accessing via column name
data2$CA1

attach(data2) # R will 'memorise' all the columns inside the df,
# so u can call the columns directly w/o mentioning the df
# properties of df columns will be memorised as well (eg is all strings? etc)
# 
CA1

data2[,1] # select only first column and all rows are listed

data2[,2:4] # all columns from 2 up to 4

data2[1:2,] # row 1 to row 2

data2[3,3] # value at 3rd row & 3rd column

data2[3,4] # value at 3rd row & 4th column
#REMEMBER each row can be called observations!

# all the rows (observations) whose gender = M:
data2[Gender == "M",]

#all the rows (observations) whose gender = M and CA2>85
data2[Gender == "M" & CA2 > 85,]


which(HW$Height == 1.8) # RETURNS INDEXES OF ITEMS, not the items themselves

#############  WHILE LOOP remember to run the entire chunk if copying to console/using the run button lol

x = 1
while(x<=3) {print("x is less than 4")
  x = x+1}


# Find the sum of first 10 integers:
x<-0; S<-0
while(x<=10) {S<- S+ x
              x<-x+1}

S

s = 0
for (i in 1:10) {
  s = s + i
print(s) # u can cheat the indents lol r doesnt care
}


#############  FOR LOOP

# Example: find the sum of first 10 integers
S<-0
for(i in 1:10){S <-S+i}
S

# Find the mean of vector x
x = c(2, 4, 3, 8, 10)
l = length(x)
S = 0
for (i in 1:l){S = S + x[i]}
ave = S/l; ave

#Find the sum of all even numbers from 1 up to 100.
x = c(1:100) 
S = 0
for (i in 1:length(x)){
 if(x[i]%%2 ==0){S = S + x[i]} else {S = S}
 }
print(S)

#less-painful-formatting version:
x = c(1:100); s = 0
for (i in 1:length(x)){
  print("starting loop")
  if (x[i]%%2 == 0) {
    s = s + x[i]
    print(s)
  } else {
    s = s
  }
}


#############  CONDITIONS WITH if()... else if()... else()

x = c(1:10); # a vector of numbers from 1 to 10
# we want to divide this vector into 3 subsets: 
# a set of all small numbers from 1 to 3
# a set of all medium numbers, from 4 to 7
# a set of large numbers from 8 to 10

S = numeric(0)
M = numeric(0)
L = numeric(0)
for (i in 1:length(x)){
 if (x[i] <=3){S = append(S, x[i])} else if (x[i]< 8)
 {M = append(M, x[i])} else {L = append(L, x[i])}
 }
print(S)

print(M)

print(L)


### FUNCTION ifelse() NEW FUNCTION AH

#q = ifelse(condition, if condition is true do this, if condition is false do this)
  
x = c(1:8);x
y = ifelse(x%%2 == 0, "even", "odd") #IMPT!!
y


s = numeric(0)
m = numeric(0)
l = numeric(0)

x = c(1:8)
s = ifelse(x<3, s = x, s = s)

############  REPEAT LOOP

# EXAMPLE: print the first five integers
i <-1
repeat {
  print (i)
  if(i ==5) { break }
  i <- i+1
 }

# Example: obtain the sum of first 5 integers
S = 0

i <-1
repeat {
 S <-S+i;
 if(i ==5) { break }
 i <- i+1
 }
S







########### USER-DEFINED FUNCTIONS IN R

#Function for finding odds ratio in general for a matrix of 2x2:

OR<-function(x){
  if(any(x==0)) {x<-x+0.5}
  odds.ratio<-x[1,1]*x[2,2]/(x[2,1]*x[1,2])
  
  return(odds.ratio) }


v = c(30, 40, 50, 60)
table = matrix(v, 2, 2)
table

OR(table)





















































