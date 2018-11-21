###################### 8 ################################
##a
set.seed(8)
x = rnorm(100)
epsilon = rnorm(100)

##b 
beta0 = 1
beta1 = 0.5 
beta2 = 2
beta3 = -1
y = beta0 + beta1*x + beta2*(x^2) + beta3*(x^3) + epsilon

##c
## Best Subset Selection 
data = data.frame(y,x)
library(leaps)
reg.fit = regsubsets(y~poly(x,10),data = data,nvmax = 10)
reg.summary = summary(reg.fit)

which.max(reg.summary$adjr2)
#4
which.min(reg.summary$cp)
#3
which.min(reg.summary$bic)
#3

plot(reg.fit)
dev.off()
par(mfrow=c(2,2))
par(mar = c(2,2,2,2))
plot(reg.summary$adjr2,type = 'l',col = 1,title = "AdjR2")
points(4,reg.summary$adjr2[4],col = 'red',pch = 4,lwd=3)
plot(reg.summary$cp,type= 'l',col = 2,title = "Cp")
points(3,reg.summary$cp[3],col = 'red',pch = 4,lwd = 3)
plot(reg.summary$bic,type = 'l',col = 3,title = "bic")
points(3,reg.summary$bic[3],col = 'red',pch = 4,lwd = 3)

#coefficients of the best model will be with 3 variable models 

coefficients(reg.fit,id = 3)
# (Intercept) poly(x, 10)1 poly(x, 10)2 poly(x, 10)3 
#3.961331   -46.192885    52.676228   -26.548371 
# the 3 variable moddel includes x^1, x^2 and x^3

##d
## forward selection
regfit.fwd = regsubsets(y~poly(x,10),data = data , nvmax = 10,method = 'forward')
regfwd.summary=summary(regfit.fwd)
which.max(regfwd.summary$adjr2)
#4
which.min(regfwd.summary$cp)
#3
which.min(regfwd.summary$bic)
#3
coefficients(regfit.fwd,id = 3)
# (Intercept) poly(x, 10)1 poly(x, 10)2 poly(x, 10)3 
#3.961331   -46.192885    52.676228   -26.548371 
## Similar results adjR2,Cp and BIC

## backward selection
regfit.bwd = regsubsets(y~poly(x,10),data = data , nvmax = 10,method = 'backward')
regbwd.summary=summary(regfit.bwd)
which.max(regbwd.summary$adjr2)
#4
which.min(regbwd.summary$cp)
#3
which.min(regbwd.summary$bic)
#3
coefficients(regfit.bwd,id = 3)
#(Intercept) poly(x, 10)1 poly(x, 10)2 poly(x, 10)3 
#3.961331   -46.192885    52.676228   -26.548371 


##### e 
## Lasso 
#cv.glmnet takes x input in matrix form
x.matrix = model.matrix(y~poly(x,10),data = data)[,-1]# i.e excluding y column
model.lasso = cv.glmnet(x.matrix,y,alpha = 1)
bestlambda = model.lasso$lambda.min
bestlambda
dev.off()
plot(model.lasso)

#best model 
lasso.fit = glmnet(x.matrix,y,alpha = 1)
lasso.pred = predict(lasso.fit,s = bestlambda,type = 'coefficients')

#(Intercept)     3.96133101
#poly(x, 10)1  -45.93071398
#poly(x, 10)2   52.41405711
#poly(x, 10)3  -26.28620055
#poly(x, 10)4   -0.02628954
#poly(x, 10)5    .         
#poly(x, 10)6   -1.24638611
#poly(x, 10)7   -0.13687648
#poly(x, 10)8   -0.51435728
#poly(x, 10)9    0.44062074
#poly(x, 10)10   0.21555553

# picks all variables except x^5.However , X^1 , x^2 and x^3 have high weightage as compared to other variables

####f 
# let
beta7 = 5

ynew = beta0 + beta7*(x^7) + epsilon

## best subset selection 
reg.fit = regsubsets(ynew~poly(x,7),data = data,nvmax = 10)
reg.summary = summary(reg.fit)

which.max(reg.summary$adjr2)
#7
which.min(reg.summary$cp)
#7
which.min(reg.summary$bic)
#7

# It gives 7 as the best fit

## Lasso

cv.lambda = cv.glmnet(x.matrix,ynew,alpha = 1)
bestlambda = cv.lambda$lambda.min
lasso.fit = glmnet(x.matrix,ynew,alpha = 1)
lasso.pred = predict(lasso.fit,s=bestlambda,type = 'coefficients')
# 1 to 7 all variables seem to be important 

##########################################9####################################
##a
##college data set 
## taking 75% of the data as test set
library(ISLR)
train = sample(1:nrow(College),0.75*nrow(College))
test = (-train)
College.train = College[train,]
College.test = College[test,]
train.mat = model.matrix(Apps~.,data = College.train)
test.mat = model.matrix(Apps~.,data = College.test)
attach(College)
#b
lm.fit = lm(Apps~.,data = College.train)
lm.pred = predict(lm.fit,College.test)

#test error
testerror = mean((College.test$Apps-lm.pred)^2)
##2059186 !!!!!!!!!!!!


# Ridge Regression
cv.lambda = cv.glmnet(train.mat,College.train$Apps,alpha = 0)
plot(cv.lambda)
bestlambda = cv.lambda$lambda.min

ridge.pred = predict(cv.lambda,newx = test.mat,s=bestlambda , alpha = 0)

testerror = mean((College.test$Apps-ridge.pred)^2)
#3668158

# LAsso Regression 

cv.lambda = cv.glmnet(train.mat,College.train$Apps,alpha = 1)
plot(cv.lambda)
bestlambda = cv.lambda$lambda.min

lasso.pred = predict(cv.lambda,s= bestlambda , newx = test.mat)
testerror = mean((College.test$Apps-lasso.pred)^2)
#2236922

############PCR ####################################
library(pls)
pcr.fit = pcr(Apps~.,data = College.train,scale = T ,validation = "CV")
validationplot(pcr.fit,val.type = "MSEP")
pcr.pred = predict(pcr.fit,College.test,ncomp = 15)
testerror = mean((College.test$Apps - pcr.pred)^2)
#4193028

################PLS #########################################

pls.fit = plsr(Apps~.,data = College.train,scale = T ,validation = "CV")
validationplot(pls.fit,val.type = "MSEP")
pls.pred = predict(pls.fit,College.test,ncomp = 7)
testerror = mean((College.test$Apps - pls.pred)^2)
#2113195

#902448.8



###########################10######################################
#a
set.seed(10)
x = matrix(rnorm(20000),nrow = 1000,ncol = 20)
beta = matrix(rnorm(20),nrow = 20,ncol = 1)
sample(1:20,6,replace = FALSE)
#[1] 18  3 10  2 17 11
#we randomly select 6 indices from 1 to 20 to make their respective betas 0 
beta[2] = 0
beta[3]= 0
beta[11] = 0
beta[17]= 0
beta[18] = 0
beta[10] = 0

epsilon = matrix(rnorm(1000),nrow = 1000,ncol = 1)
y =x%*%beta + epsilon

data = data.frame(y,x)
data.mat = model.matrix(y~.,data = data)
#b
train = sample(1:nrow(x),100,replace = FALSE)
test = (-train)
data = data.frame(y,x)

#best subset selection 
model.fit = regsubsets(y~.,data = data[train,],nvmax = 20)
model.summary = summary(model.fit)

which.max(model.summary$adjr2)
#17
which.min(model.summary$bic)
#13
which.min(model.summary$cp)
#15

val.errors = rep(NA,20)
# train error 
predict.regsubseterr = function(rowindex){
for(i in 1:20){
  coefi = coef(model.fit,id = i)
  pred = data.mat[rowindex,names(coefi)]%*%coefi + epsilon[rowindex,]
  val.errors[i] = mean((y[rowindex,]-pred)^2)
  
}
  return(val.errors)
}

predict.regsubseterr(train)
plot(predict.regsubseterr(train), ylab = "Training MSE", xlab = "No.of parameters",type = 'b')
which.min(predict.regsubseterr(train))
#12

predict.regsubseterr(test)
plot(predict.regsubseterr(test), ylab = "Test MSE", xlab = "No.of parameters",type = 'b')
which.min(predict.regsubseterr(test))
#12
## both the training and test MSE  gives 13 variable model as best subset 

coef(model.fit,12)


############################# 11 #########################################################
# clearing all variables
rm(list=ls(all=TRUE))

library(MASS)
library(leaps)
library(glmnet)

# Boston dataset 
fix(Boston)
str(Boston)
summary(Boston)
attach(Boston)

Boston = na.omit(Boston)
sum(is.na(Boston))

regfit.full = regsubsets(crim~.,data = Boston,nvmax = ncol(Boston))
reg.summary = summary(regfit.full)

# as there is no predict function for regsubset ,we create our own function
predict.regsubsets =function (object ,newdata ,id ,...){
  form=as.formula(object$call [[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
# perform 10 fold cross validation 
k =10
p = ncol(Boston)-1

set.seed(11)
folds = sample(1:k,nrow(Boston),replace = TRUE)
table(folds)
cv.errors=matrix(NA,k,p, dimnames=list(NULL, paste(1:p))) # data = NA,nrow = k,ncol = 13, dimnames = null,dimnames contain list of rownames and colnames

for(j in 1:k){
  best.fit = regsubsets(crim~.,data = Boston[folds!=j,] , nvmax = 13)
  for(i in 1:p){
    pred = predict.regsubsets(best.fit,Boston[folds==j,],id=i)
    cv.errors[j,i]=mean((Boston$crim[folds==j]-pred)^2)
  }
}

mean.cv.errors = apply(cv.errors,2,mean) # 2 implies column wise mean
min(mean.cv.errors)
#40.18
plot(mean.cv.errors, type = "b",col="red",pch = 1)

## mean error through cross validation is least for 12 parameters through cross validation : 40.18402

##################### Lasso ###############################################################
library(glmnet)
x = model.matrix(crim ~ . - 1, data = Boston)
y = Boston$crim


lasso.mod = glmnet(x,y,alpha = 1)
plot(lasso.mod,label = TRUE)

set.seed(111)
cv.out = cv.glmnet(x,y,alpha = 1)
plot(cv.out)
coef(cv.out)

bestlambda = cv.out$lambda.min
lasso.pred = predict(lasso.mod,s=bestlambda,newx = x)
mean((lasso.pred-y)^2)
## 40.338

##########################Ridge #######################################
cv.out = cv.glmnet(x,y,alpha = 0)
plot(cv.out)
bestlambda = cv.out$lambda.min     #best lambda giving minimum MSe
bestlambda

coef(cv.out)

ridge.mod = glmnet(x,y,alpha = 0)
ridge.pred = predict(ridge.mod,s=bestlambda,newx = x)
mean((ridge.pred-y)^2)
# 40.7787



###############PCR################################################
library(pls)
pcr.fit = pcr(crim ~ ., data = Boston, scale = TRUE, validation = "CV")
summary(pcr.fit)

## all 13 components selected

##C 
## Best subset selection model 
##
