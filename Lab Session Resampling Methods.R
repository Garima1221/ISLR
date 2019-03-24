library(ISLR)
set.seed(1)
#randomly choose sample size of 392*196
train = sample(392,196)

attach(Auto)

lm.fit=lm(mpg~horsepower,data = Auto,subset = train)

mean((mpg - predict(lm.fit))[-train]^2) # gives MSE = 26.14

lm.fit2 = lm(mpg~poly(horsepower,2),data = Auto,subset = train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2) # gives MSE = 19.82

lm.fit3 = lm(mpg~poly(horsepower,3),data = Auto,subset = train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)  # gives MSE = 19.78

##########################LOOCV###############################################

#linear regression using glm
glm.fit = glm(mpg~horsepower,data = Auto)
coef(glm.fit)

#cross validation using cv.glm
#BOOT library contains cv.glm
library(boot)
glm.fit = glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta

#for different polynomials
#k=1 Thus LOOCV
cv.err=rep(0,5)
for(i in 1:5){
  glm.fit = glm(mpg~poly(horsepower,i),data = Auto)
  cv.err[i] = cv.glm(Auto,glm.fit)$delta[1]
}

cv.err
lines(cv.err)

#k fold cross validation

set.seed(10)
cv.error10=rep(0,10)
for(i in 1:10){
  glm.fit = glm(mpg~poly(horsepower,i),data = Auto)
  cv.error10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]  # Note K should be capital
}

lines(cv.error10)
#############################Bootstrap#######################################################

alpha.fn = function(data,index){
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/var(X)+var(Y)-2*cov(X,Y))
}

alpha.fn(Portfolio,1:100)

#using Bootstrap i.e sampling with replacement

set.seed(100)
alpha.fn(Portfolio,sample(100,100,replace = TRUE))#from range 1 to 100 select 100 samples

boot(Portfolio,alpha.fn,R=1000) #generate 1000 bootstraps

boot.fn = function(data,index){
  return(coef(lm(mpg~horsepower,data = data,subset = index)))
}

boot.fn(Auto,1:392)

set.seed(111)
boot.fn(Auto,sample(392,392,replace = T))

#boot function to estimate standard errors of boot 
boot(Auto,boot.fn,1000)

summary(lm(mpg~horsepower,data = Auto))$coef

boot.fn=function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data = data,subset = index))
  set.seed(111)
  boot(Auto,boot.fn,1000)
  