####Q5
library(ISLR)

str(Default)
summary(Default)
attach(Default)

### 5 a
set.seed(1)
glm.fit = glm(default~balance+income,data = Default,family = binomial)

#### 5 b
# i
#taking 75% data as training set and 25% as test set
train = sample(nrow(Default),0.75*nrow(Default))

#ii
glm.fit = glm(default~balance+income,data = Default,family = binomial,subset = train)
default.trainset = Default[train,]
default.testset = Default[-train,]
##iii
glm.pred = rep('No',nrow(default.testset))
glm.probs = predict(glm.fit,default.testset)
glm.pred[glm.probs>.5]='Yes'
##iv
mean(glm.pred!=default.testset$default)
#test error = 3.56%


#d
glm.fit = glm(default~.,data = Default,family = binomial,subset = train)
default.trainset = Default[train,]
default.testset = Default[-train,]
##iii
glm.pred = rep('No',nrow(default.testset))
glm.probs = predict(glm.fit,default.testset)
glm.pred[glm.probs>.5]='Yes'
##iv
mean(glm.pred!=default.testset$default)

#test error 3.56

#################################6######################
# 6 
# a 
set.seed(1)
glm.fit = glm(default ~ income + balance, data = Default, family = binomial)
summary(glm.fit)

#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -1.154e+01  4.348e-01 -26.545  < 2e-16 ***
#  income       2.081e-05  4.985e-06   4.174 2.99e-05 ***
#  balance      5.647e-03  2.274e-04  24.836  < 2e-16 ***

# b 
boot.fn = function(dataset,index){
  return(coef(glm(default~income+balance,data = dataset,family = binomial,subset = index)))
}

# c 

boot(Default,boot.fn,R=100)#takes time to compute 
#
#similar results as logistic regression using validation approach

#Bootstrap Statistics :
#  original        bias     std. error
#t1* -1.154047e+01  6.612260e-02 4.249672e-01
#t2*  2.080898e-05  4.270750e-08 4.736893e-06
#t3*  5.647103e-03 -3.760099e-05 2.121841e-04


#####################7##################################################

library(ISLR)
str(Weekly)
summary(Weekly)

#7a
glm.fit = glm(Direction~Lag1+Lag2,data = Weekly,family = binomial)
summary(glm.fit)

#b
glm.fit = glm(Direction~Lag1+Lag2,data = Weekly[-1,],family = binomial)
summary(glm.fit)

#c
glm.probs=predict(glm.fit,Weekly[1,],type = 'response')
glm.pred[glm.probs>0.5]='Up'
#predicted = Up whereas actual is Down

#d
count = rep(0,nrow(Weekly))
for(i in 1:nrow(Weekly)){
  #i
  glm.fit=glm(Direction~Lag1+Lag2,data = Weekly[-i,],family = binomial)
  #ii posterior probability
  glm.probs = predict(glm.fit,Weekly[i,],type = 'response')
  glm.pred = 'Down'
  glm.pred[glm.probs>.5]='Up'
  if(glm.pred!=Weekly[i,]$Direction)
    count[i] = 1
  
}

sum(count)
#490 errors
#e
mean(count)
#0.45

#####################################8##################################
#a
set.seed(1)
y=rnorm(100)
x=rnorm(100)
y = x-2*x^2+rnorm(100)#model : rnomr(100) can be considered as random noise

#n = 100
#p = 2

#b
plot(x,y)
#quadratic plot 

set.seed(10)
glm.fit(y~x)
dataset = data.frame(x,y)

#c
#i
cv.glm(dataset,glm.fit)$delta  ### internally uses k = 100
#$delta
#[1] 0.2493299 0.2492773

#ii
glm.fit = glm(y ~ poly(x, 2))
cv.glm(dataset, glm.fit)$delta

#[1] 1.086596 1.086326

#iii
glm.fit = glm(y ~ poly(x, 3))
cv.glm(dataset, glm.fit)$delta
#[1] 1.102585 1.102227


#iv
glm.fit = glm(y ~ poly(x, 4))
cv.glm(dataset, glm.fit)$delta
#[1] 1.114772 1.114334


##################d
set.seed(20)
glm.fit(y~x)
dataset = data.frame(x,y)

#c
#i
cv.glm(dataset,glm.fit)$delta  ### internally uses k = 100
#$delta
#[1] 0.2493299 0.2492773

#ii
glm.fit = glm(y ~ poly(x, 2))
cv.glm(dataset, glm.fit)$delta

#[1] 1.086596 1.086326

#iii
glm.fit = glm(y ~ poly(x, 3))
cv.glm(dataset, glm.fit)$delta
#[1] 1.102585 1.102227

#iv
glm.fit = glm(y ~ poly(x, 4))
cv.glm(dataset, glm.fit)$delta
#[1] 1.114772 1.114334


#Exact same, because LOOCV will be the same since it evaluates n folds of a single observation

#e i mmodel

#f 
summary(glm.fit)

#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  -1.8277     0.1041 -17.549   <2e-16 ***
#  poly(x, 4)1   2.3164     1.0415   2.224   0.0285 *  
#  poly(x, 4)2 -21.0586     1.0415 -20.220   <2e-16 ***
#  poly(x, 4)3  -0.3048     1.0415  -0.293   0.7704    
#poly(x, 4)4  -0.4926     1.0415  -0.473   0.6373 
#from p values linear and quadratic terms seems to be statistically significant


#########################9#####################################

##a 
library(MASS)
str(Boston)
attach(Boston)

mu = mean(medv)
#22.53

#b
#standard error of mu
sderror.fn = function(parameter){
  return(sd(parameter)/(sqrt(length(parameter))))
}

sderror.fn(medv)
#0.41
#c
boot.fn = function(data,index){
  return(mean(data[index]))
}

boot.mu = boot(medv,boot.fn,R = 1000)

#mu = 22.53 
#similar result

#d
lower.CI = boot.mu-2*sderror.fn(medv)#21.71508
higher.CI = boot.mu+2*sderror.fn(medv)#23.35053
#or
t.test(medv)
#95 percent confidence interval:
#  21.72953 23.33608

#e
mu.med = median(medv)#21.2

#f
#Sd of median
boot.fn = function(parameter, index) return(median(parameter[index]))
boot(medv, boot.fn, 1000)
#0.37

#g
mu0.1 = quantile(medv,c(0.1))
#12.75

#h
boot.fn = function(parameter, index) return(quantile(parameter[index], c(0.1)))
boot(medv, boot.fn, 1000)
