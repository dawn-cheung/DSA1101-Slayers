#tutorial 1
ngl_im_jealous <- function(salary) {
  price = 1200000 #cost of house FIXED VALUE, and its data type is auto detected
  down_payment = price*0.25 #cost of house la
  saved = 10000 #this is for 1st month. unsure if need to start from month 0
  #salary = 10000 #paid at the end of the month
  
  #ok the months r monthing now
  month_counter = 0
  while (saved < down_payment) {
    saved = saved * 1.02 #DOES NOT INCLUDE THE SALARY FOR THIS MONTH
    saved = saved + (salary * 0.4)
    month_counter = month_counter + 1
  }
  paste0("you need ",month_counter," months to get that house. This is longer than my will to live.")
  print("debug")
}

ngl_im_jealous(7000) #1i) 55
ngl_im_jealous(10000) #1ii) 44

sal = c(7000, 10000) #sal is a 2 dimensional vector here
for (i in 1:length(sal)){
  ngl_im_jealous(sal[i]) #the paste0() doesnt work here idk why
  #try using cbind to make it into a matrix to input salary and corresponding months
}

MOREmoney <- function(salary, rate) {
  price = 1200000 #cost of house
  down_payment = price*0.25
  saved = 10000 #this is for 1st month. unsure if need to start from month 0
  
  #ok the months r monthing now
  month_counter = 0
  while (saved < down_payment) {
    month_counter = month_counter + 1
    saved = saved * 1.02 #DOES NOT INCLUDE THE SALARY FOR THIS MONTH
    saved = saved + (salary * 0.4)
    if (month_counter%%4 == 0) {
      salary = salary * (1 + rate)
    }
  }
  paste0("with your improved (insane) salary , you need ",month_counter," months to get that house. This is longer than my will to live.")
}


MOREmoney(7000, 0.02) #2i) 52 # but if you put month_counter below the salary calc it becomes 51
MOREmoney(10000, 0.01) #2ii) 43
