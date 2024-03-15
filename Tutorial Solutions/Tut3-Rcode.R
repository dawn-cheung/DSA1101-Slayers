
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








