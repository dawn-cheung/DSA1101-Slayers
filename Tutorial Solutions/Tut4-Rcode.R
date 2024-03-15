
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







