library(ISLR)
fix(Hitters)

names(Hitter)

sum(is.na(Hitters$Salary))

Hitters = na.omit(Hitters)
dim(Hitters)

sum(is.na(Hitters))


# leaps library contain regsubsets function for best subset selection

library(leaps)
regfit.full = regsubsets(Salary~.,Hitters)# by default it produces 8 variable model
summary(regfit.full)

# to increase number of variable in our models we use nvmax()
regfit.full = regsubsets(Salary~.,data = Hitters , nvmax = 19)
reg.summary = summary(regfit.full)

names(reg.summary)
reg.summary$rsq

par(mfrow=c(2,2))
plot(reg.summary$rss,xlab = "Number of variables",ylab = "RSS",type = "l")
plot(reg.summary$adjr2,xlab = "Number of variables",ylab = "Adj RSq",type = "l") ## type = l(ell) help us plot line

#put points on where R2 is argest

points(which.max(reg.summary$adjr2),reg.summary$adjr2[which.max(reg.summary$adjr2)],col="red",cex=2,pch=20)
points(which.min(reg.summary$rss),reg.summary$rss[which.min(reg.summary$rss)],col="red",cex=2,pch=20)

plot(reg.summary$cp,xlab = "Number of variables",ylab = "Cp",type = "l")
plot(reg.summary$bic,xlab = "Number of variables",ylab = "Bic",type = "l")

points(which.min(reg.summary$cp),reg.summary$cp[which.min(reg.summary$cp)],col="red",cex=2,pch=20)
points(which.min(reg.summary$bic),reg.summary$bic[which.min(reg.summary$bic)],col="red",cex=2,pch=20)

plot(regfit.full,scale = 'r2')
plot(regfit.full,scale = 'adjr2')
plot(regfit.full,scale = 'Cp')
plot(regfit.full,scale = 'bic')

#6 variable model contains lowest BIC
#we can get its coef by :
coef(regfit.full,6)


######################Forward Stepwise Selection #######################

regfit.fwd = regsubsets(Salary~.,data = Hitters,nvmax = 19,method = "forward")
summary(regfit.fwd)


######################Backward Stepwise Selection #########################

regfit.bwd = regsubsets(Salary~.,data = Hitters,nvmax = 19,method = "backward")
summary(regfit.bwd)


#####Best subset model under forward and backward selection contains different variables

coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)

#####################Validation set and Cross Validation set approach##############

set.seed(1)
train = sample(c(TRUE,FALSE),nrow(Hitters),rep = TRUE)
test = (!train)

##Subset selection

regfit.best = regsubsets(Salary~.,data = Hitters[train,],nvmax = 19)
test.mat = model.matrix(Salary~.,data = Hitters[test,])   #model Salary using all the features on test data
#this will only create a matrix from the dataset Hitters[test,] of the same dimension

val.errors = rep(NA,19)
for(i in 1:19){
  coefi = coef(regfit.best,id=i)  # this will store the best coefficients along with the variable name  obtained from subset selection for 1 to 19 th selection of variables
  pred = test.mat[,names(coefi)] %*%coefi 
  # for each i multiply values in the selected variable's place and multiple by the coef predicted in the train model
  # i.e training set's coefficients obained is multiplied with corresponding variable name and Salary value is obtained 
  val.errors[i] = mean((Hitters$Salary[test]-pred)^2)
}

which.min(val.errors)
coef(regfit.best,10)

##############making our own predict function as regsubset doesnt have its own predict function

predict.regsubsets = function(object,newdata,id,...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form,newdata)      
  coefi = coef(object,id=id)
  xvars = names(coefi)
  mat[,xvars]%*%coefi
  
}

##subset selection using all the data in the dataset
regfit.best = regsubsets(Salary~.,data = Hitters,nvmax = 19)
coef(regfit.best,10)

k = 10
set.seed(1)
folds = sample(1:k,nrow(Hitters),replace = TRUE)
cv.errors = matrix(NA,k,19,dimnames = list(NULL,paste(1:19)))

regfit.best = regsubsets(Salary~.,data = Hitters,nvmax = 19)
coef(regfit.best,10)


#########cross validation 
k=10
set.seed(1)
folds = sample(1:k,nrow(Hitters),replace = TRUE) #folds will contain 1 to 10 spread randomly nrow(Hitters) time
cv.errors = matrix(NA,k,19,dimnames = list(NULL,paste(1:19)))

for(j in 1:k){
  best.fit = regsubsets(Salary~.,data = Hitters[folds!=j,] , nvmax = 19)
  for(i in 1:19){
    pred = predict.regsubsets(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
  }
}



mean.cv.errors = apply(cv.errors,2,mean)#2 means apply mean column wise
mean.cv.errors

par(mfrow = c(1,1))
plot(mean.cv.errors,type = 'b')

reg.best = regsubsets(Salary~.,data = Hitters,nvmax =19)
coef(reg.best,11)

####################Ridge Regression #####################################
install.packages(glmnet)
library(glmnet)
x = model.matrix(Salary~.,Hitters)[,-1]#i.e excluding Salary Column
y = Hitters$Salary

grid = 10^seq(10,-2,length = 100)  # by default glmnet uses its own lambda but here we are generating a seq of lambda ranging from 10^10 to 10^-2
ridge.mod = glmnet(x,y,alpha = 0,lambda = grid)#It shows from left to right the number of nonzero coefficients (Df), the percent (of null) deviance explained (%dev) and the value of ???? (Lambda). 

plot(ridge.mod,label = TRUE)# plots with coefficient numbered 1 to 19
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))# -1 signifies except intercept term

ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

predict(ridge.mod,s=50,type = "coefficients")[1:20,] #for different value of lambda say 50

##############ridge regression using different form of sampling ##############

set.seed(1)
train = sample(1:nrow(x),nrow(x)/2)
test = (-train)


ridge.mod = glmnet(x[train,],y[train],alpha = 0,lambda = grid,thresh = 1e-12)
#Convergence threshold for coordinate descent. Each inner coordinate-descent loop continues until the maximum change in the objective after any coefficient update is less than thresh times the null deviance. Defaults value is 1E-7.
ridge.pred = predict(ridge.mod,s=4,newx = x[test,]) # for lambda = 4
mean((ridge.pred-y.test)^2)

mean((mean(y[train])-y.test)^2)

# checking with lambda = 0
ridge.pred = predict(ridge.mod,s=0,newx = x[test,])
#equivalent to lm(y~c,subset = train)
mean((ridge.pred-y.test)^2)



predict(ridge.mod,s=0,type = "coefficients",exact = T)[1:20,]


###########Using cross validation for lamda selection###
set.seed(1)
cv.out = cv.glmnet(x[train,],y[train],alpha = 0)
plot(cv.out)
bestlambda = cv.out$lambda.min#best lambda giving minimm testMSe
bestlambda

ridge.pred = predict(ridge.mod,s=bestlambda,newx = x[test,])
mean((ridge.pred-y.test)^2)

out = glmnet(x,y,alpha = 0)
predict(out,s = bestlambda,type = 'coefficients')[1:20,]


################Lasso ################################

lasso.mod = glmnet(x[train,],y[train],alpha = 1,lambda = grid)
plot(lasso.mod,label = TRUE)

#######cross validation

set.seed(1)
cv.out = cv.glmnet(x[train,],y[train],alpha = 1)
plot(cv.out)

bestlambda = cv.out$lambda.min
lasso.pred = predict(lasso.mod,s=bestlambda,newx = x[test,])
mean((lasso.pred-y.test)^2)

out = glmnet(x,y,alpha = 1,lambda = grid)
lasso.coef = predict(out,type = "coefficients",s=bestlambda)[1:20,]
lasso.coef


############################## PCR and PLS Regression #################

#### model fit on training data and validated also on test data
library(pls)
set.seed(2)

pcr.fit = pcr(Salary~.,data = Hitters,scale = TRUE,validation="CV")

summary(pcr.fit)

validationplot(pcr.fit,val.type = "MSEP")

#### model fit on training data and validated on train data

pcr.pred = predict(pcr.fit,x[test,],ncomp = 7)
mean((pcr.pred-y.test)^2)
####PLS 
pls.fit = plsr(Salary~.,data = Hitters,subset = train,scale = TRUE,validation = "CV")
summary(pls.fit)
validationplot(pls.fit,val.type = "MSEP")
pls.pred = predict(pls.fit,x[test,],ncomp = 2)
mean((pls.pred-y.test)^2)


####PLS using full data set

pls.fit = plsr(Salary~.,data=Hitters,scale=TRUE,ncomp=2)
summary(pls.fit)

