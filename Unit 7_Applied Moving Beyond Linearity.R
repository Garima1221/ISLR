####################6#########################
#a

rm(list = ls())
dev.off()
library(ISLR)
library(boot)
library(glmnet)

str(Wage)
summary(Wage)
fix(Wage)
data(Wage)


######### Choosing degree d using cross validation
cv.error = rep(NA,10)
# let us perform 10 fold cross validation error(K = 10) and check upto 10 th degree polynomial 
for(i in 1:10){
  glm.fit = glm(wage~poly(age,i),data = Wage)
  cv.error[i] = cv.glm(Wage,glm.fit,K = 10)$delta[2]
}
cv.error

plot(1:10,cv.error,type = 'l')
which.min(cv.error)
#4

###### choosing degree d using ANOVA
fit.1 = lm(wage~poly(age, 1), data=Wage)
fit.2 = lm(wage~poly(age, 2), data=Wage)
fit.3 = lm(wage~poly(age, 3), data=Wage)
fit.4 = lm(wage~poly(age, 4), data=Wage)
fit.5 = lm(wage~poly(age, 5), data=Wage)
fit.6 = lm(wage~poly(age, 6), data=Wage)
fit.7 = lm(wage~poly(age, 7), data=Wage)
fit.8 = lm(wage~poly(age, 8), data=Wage)
fit.9 = lm(wage~poly(age, 9), data=Wage)
fit.10 = lm(wage~poly(age, 10), data=Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5, fit.6, fit.7, fit.8, fit.9, fit.10)

agelims = range(age)
age.grid = seq(from = agelims[1],to = agelims[2])
preds = predict(fit.4,newdata = list(age = age.grid),se = TRUE) 
se.bands = cbind(preds$fit + 2*preds$se.fit,preds$fit-2*preds$se.fit)

par(mfrow = c(1,2) , mar = c(4.5,4.5,1,1),oma = c(0,0,4,0))
plot(age,wage,xlim = agelims ,cex = .5 ,col = "darkgrey")
title("Degree-4 Polynomial",outer = T)
lines(age.grid,preds$fit,lwd = 2,col = 'blue')
matlines(age.grid,se.bands,lwd = 1 ,col = "red" , lty =3)

## degree 4 polynomial is chosen and it gives similar results

###########b. Step function

# let us have 10 cuts and K = 10 in cross validation
# doing cross validation to get optimum number of cuts 
cv.error = rep(NA,10)
for(i in 2:10){
Wage$agecut = cut(age,i)
glm.fit = glm(wage~agecut,data = Wage)
cv.error[i] = cv.glm(Wage,glm.fit,K=10)$delta[2]
}

which.min(cv.error)
#8
plot(1:10,cv.error,type = 'l')
#using 8 cuts for our fit and plotting the same 

glm.fit = glm(wage~cut(age, 8), data=Wage)
agelims = range(Wage$age)
age.grid = seq(from=agelims[1], to=agelims[2])
glm.pred = predict(glm.fit, data.frame(age=age.grid))
plot(wage~age, data=Wage, col="darkgrey")
lines(age.grid, glm.pred, col="blue", lwd=2)



############################################## 7 ################################################################
cor(wage,year)
#0.06554428
cor(wage,age)
#0.1956372


plot(maritl,wage)
#Generally Married people are paid more 
plot(race,wage)
# Asians are paid more 
plot(education,wage)
# higher eduction means higher wage
plot(region,wage)
# only New England data available
plot(jobclass,wage)
#information jobclass paid more 
plot(health_ins,wage)

library(splines)


#################################################################################################
#################################################################################################
######################################8##########################################################
rm(list = ls())
library(ISLR)
fix(Auto)
data(Auto)
str(Auto)

cor(Auto[,-9])
pairs(Auto)
## mpg vs displacement, cylinder ,horsepower,weight
#from correlation plot displacement horsepower and weight inverse non linear relationship ,maybe of degree 2 

##### POLYNOMIAL FIT 
# let us choose weight variable which has maximum correlation
fit.1 = lm(mpg~poly(weight,1),data = Auto)
fit.2 = lm(mpg~poly(weight,2),data = Auto)
fit.3 = lm(mpg~poly(weight,3),data = Auto)
fit.4 = lm(mpg~poly(weight,4),data = Auto)

anova(fit.1,fit.2,fit.3,fit.4)
#plot the data and the fit 

pred = predict(fit.2,data = Auto)
plot(Auto$weight,Auto$mpg,col = "darkgrey")
########## **** CHECK ############
#lines(Auto$weight,pred,col = "blue")
lines(sort(Auto$weight), fitted(fit.2)[order(Auto$weight)], col='blue', type='b') 
## 
###############
#############STEP############################
cv.err = rep(NA,10)
# 10 fold and 10 knots 
for(i in 2:10){
  Auto$weight.cut = cut(Auto$weight,i)
  fit = glm(Auto$mpg~Auto$weight.cut,data = Auto)
  cv.err[i] = cv.glm(Auto,fit,K=10)$delta[2]
  pred
}

which.min(cv.err)
#2
plot(1:10,cv.err,type = 'l')
# 2 knots step function is sufficient
Auto$weight.cut = cut(Auto$weight,2)
fit.2 = glm(Auto$mpg~Auto$weight.cut,data = Auto)
weight.list = seq(from = min(Auto$weight),to = max(Auto$weight))
pred.step = predict(fit.2,data.frame(weight = weight.cut))

plot(mpg~weight, data=Auto, col="darkgrey")
lines(sort(Auto$weight),pred.step[order(Auto$weight)] , col="blue", lwd=2)

###################SPLINES######################################
library(splines)
cv.err = rep(NA,15)
for(i in 2:15){
    fit = glm(mpg~ns(weight,df = i),data = Auto)
    cv.err[i] = cv.glm(Auto,fit,K=10)$delta[2]
    
}

which.min(cv.err)
#2
fit = glm(mpg~ns(weight,df=2),data = Auto)
pred = predict(fit,se=TRUE)
plot(Auto$weight,Auto$mpg,col = "darkgrey")
lines(sort(Auto$weight),pred$fit[order(Auto$weight)],col="blue")




############GAMS#################################################
library(gam)
fit = gam(mpg~s(weight,4)+s(displacement,4),data = Auto)
summary(fit)
pred = predict(fit)
#
#library(akima)
#fit = gam(mpg~lo(weight,displacement,span=0.5),data = Auto)
#plot(fit)
##################9 #############################################
### a)
data("Boston")
str(Boston)
lm.fit = lm(nox~poly(dis,3),data = Boston)
summary(lm.fit)
dis.grid = seq(from = min(dis),to = max(dis))
lm.pred = predict(lm.fit,list(dis = dis.grid))
rss.err=mean((Boston$nox - lm.pred)^2)
plot(nox~dis,col="lightblue")
lines(dis.grid,lm.pred,col = "darkblue")
rss.err

### b )
plot(nox~dis,col="lightblue")
col.arr = array(data = c("red","blue","green","orange","purple","pink","brown","purple","black","lightgreen"))
#legend(8,0.9,legend=1:10,col = col.arr[1:10])
rss.err = rep(NA,10)
for(i in 1:10){
  lm.fit = lm(nox~poly(dis,i),data = Boston)
  lm.pred = predict(lm.fit)
  rss.err[i] = sum(lm.fit$residuals^2)
  lines(sort(dis),lm.pred[order(dis)],col=col.arr[i])
}


##c
#10 fold cross validation
cv.err = rep(NA,10)
for(i in 1:10){
  glm.fit = glm(nox~poly(dis,i),data = Boston)
  cv.err[i] = cv.glm(Boston,glm.fit,K=10)$delta[2]
}
which.min(cv.err)
#4
plot(1:10,cv.err,type = 'l')

### d 
spline.fit = lm(nox~bs(dis,4),data = Boston)
summary(spline.fit)
spline.pred = predict(spline.fit)
plot(dis,nox,col="lightblue")
lines(sort(dis),spline.pred[order(dis)])

#e

rss.err = rep(NA,15)
plot(dis,nox,col="lightblue")
for(i in 1:15){
  spline.fit = lm(nox~bs(dis,i),data = Boston)
  spline.pred = predict(spline.fit)
  lines(sort(dis),spline.pred[order(dis)],col= "red")
  rss.err[i] = mean((dis-spline.pred)^2)
}
which.min(rss.err)

#df = 3
plot(1:15,rss.err,type = 'l')


## f 
rss.err = rep(NA,15)
plot(dis,nox,col="lightblue")
for(i in 1:15){
  spline.fit = glm(nox~bs(dis,i),data = Boston)
  spline.pred = predict(spline.fit)
  lines(sort(dis),spline.pred[order(dis)],col= "red")
  rss.err[i] = cv.glm(Boston,glm.fit,K=10)$delta[2]
}
which.min(rss.err)

#df = 2
plot(1:15,rss.err,type = 'l')

########################################10##################################################
library(gam)
set.seed(10)
fix(College)
# 75 % of the data is taken as training set and 25% as test set
train = sample(nrow(College),0.50*nrow(College))
test = (-train)
College.train = College[train,]
College.test = College[test,]
## forward stepwise selection
reg.fit = regsubsets(Outstate ~ ., data = College.train, nvmax = 17, method = "forward")
reg.summary = summary(reg.fit)
which.max(reg.summary$adjr2)
#12
which.min(reg.summary$bic)
#6
which.min(reg.summary$cp)
#11
#############GAM
gam.fit = gam(Outstate ~ Private + s(Room.Board, df = 7) + s(PhD, df = 7) + 
                s(perc.alumni, df = 7) + s(Expend, df = 7) + s(Grad.Rate, df = 7), data = College.train)
par(mfrow = c(2, 3))
plot(gam.fit, se = T, col = "blue")

reg.fit = regsubsets(Outstate ~ ., data = College, method = "forward")
coefi = coef(reg.fit, id = 6)
names(coefi)

gam.pred = predict(gam.fit, College.test)
gam.err = mean((College.test$Outstate - gam.pred)^2)
gam.err

summary(gam.fit)

#################################11###########################################################

set.seed(1)
x1 = rnorm(100)
x2 = rnorm(100)
eps = rnorm(100)
y = -2.1 + 1.3 * x1 + 0.54 * x2 + eps
#Let
beta.0 = rep(NA,1000)
beta.1 = rep(NA,1000)
beta.2 = rep(NA,1000)
 
beta.1[1]=5
for(i in 1:1000){
  a = y-beta.1*x1
  beta.2[i] = lm(a~x2)$coef[2]
  a = y - beta.2[i]*x2
  
  while(i<1000){
  beta.1[i+1] = lm(a~x1)$coef[2]
 }
  
  beta.0[i] = lm(a~x1)$coef[1]
}

par(mfrow = c(1,3))
plot(1:1000,beta.0)
plot(1:1000,beta.1)
plot(1:1000,beta.2)

#f
lm.fit = lm(Y ~ X1 + X2)
plot(1:1000, beta0, type = "l", xlab = "iteration", ylab = "betas", ylim = c(-2.2, 
                                                                             1.6), col = "green")
lines(1:1000, beta1, col = "red")
lines(1:1000, beta2, col = "blue")
abline(h = lm.fit$coef[1], lty = "dashed", lwd = 3, col = rgb(0, 0, 0, alpha = 0.4))
abline(h = lm.fit$coef[2], lty = "dashed", lwd = 3, col = rgb(0, 0, 0, alpha = 0.4))
abline(h = lm.fit$coef[3], lty = "dashed", lwd = 3, col = rgb(0, 0, 0, alpha = 0.4))
legend("center", c("beta0", "beta1", "beta2", "multiple regression"), lty = c(1, 
                                                                              1, 1, 2), col = c("green", "red", "blue", "black"))

##########################################12#############################################################
set.seed(12)

p = 100
n = 1000

x = matrix(data = rnorm(n*p),nrow = n,ncol = p)

beta.p = rnorm(p)
epsilon = rnorm(1000)

y = x%*%beta.p + epsilon



