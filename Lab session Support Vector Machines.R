rm(list=ls())

set.seed(1)
#40 observation spread across 20X2 matrix
x = matrix(rnorm(20*2),ncol = 2)
y = c(rep(-1,10),rep(1,10))#it takes binary values of -1 and 1 
x[y==1,]=x[y==1,]+1

plot(x,col = (3-y))

dat = data.frame(x=x,y=as.factor(y))
library(e1071)
###SVM##########
svmfit = svm(y~.,data = dat,kernel = "linear",cost = 10,scale = FALSE)
#cost tells us the cost of violation of the margin 
plot(svmfit,dat)
## x shows support vectors 


#support vector identities 
svmfit$index

summary(svmfit)

#using smaller value of cost parameter leads to larger number of support vectors because the margin is wider
svmfit = svm(y~.,data = dat,kernel = "linear", cost = 0.1,scale = FALSE)
plot(svmfit,dat)
svmfit$index

set.seed(1)
###### Cross validation #################
tune.out = tune(svm,y~.,data = dat,kernel = "linear",ranges = list(cost = c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)

### tune function stores the best model in best.model 
bestmod = tune.out$best.model
summary(bestmod)

xtest = matrix(rnorm(20*2),ncol = 2)
ytest = sample(c(-1,1),20,rep = TRUE)
xtest[ytest == 1,]=xtest[ytest == 1,]+1
testdat = data.frame(x=xtest,y = as.factor(ytest))

ypred = predict(bestmod,testdat)
table(predict = ypred,truth = testdat$y)

svmfit = svm(y~.,data = dat,kernel = "linear",cost = 0.1,scale = FALSE)
ypred = predict(svmfit,testdat)
table(predict = ypred,truth = testdat$y)

## when the two classes are linearly separable 
##### Linearly separable ###### 
x[y==1,]=x[y==1,] + 0.5 
plot(x,col(y+5)/2,pch = 19)

dat = data.frame(x=x,y=as.factor(y))
svmfit = svm(y~.,data = dat,kernel = "linear",cost = 1e5)## no training errors made and only 3 supprt vector used so this will perform poorly on test data
summary(svmfit)

 ### this model will perform poorly on test data 
svmfit = svm(y~.,data = dat,kernel = "linear",cost = 1)
summary(svmfit)
plot(svmfit,dat)


######## Non Linear Model ####
set.seed(1)
x = matrix(rnorm(200*2),ncol = 2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y = c(rep(1,150),rep(2,50))
dat = data.frame(x=x,y=as.factor(y))

plot(x,col = y)

train = sample(200,100)
svmfit = svm(y~.,data = dat[train,],kernel = "radial",gamma = 1,cost = 1)
plot(svmfit,dat[train,])
summary(svmfit)


svmfit = svm(y~.,data = dat[train,],kernel = "radial",gamma = 1,cost = 1e5)
plot(svmfit,dat[train,])

## using cross validation to chose best gamma and cost value

set.seed(1)
tune.out = tune(svm,y~.,data = dat[train,],kernel = "radial",ranges = list(cost=c(0.1,1,10,100,1000),gamma = c(0.5,1,2,3,4)))
summary(tune.out)
## best gamma = 2 and cost = 1

table(true = dat[-train,"y"],pred = predict(tune.out$best.model,newx = dat[-train,]))

#### ROC curve ######################
library(ROCR)
rocplot = function(pred,truth,...){
  predob = prediction(pred,truth)
  perf = performance(predob,"tpr","fpr")
  plot(perf,...)
}

svmfit.opt = svm(y~.,data = dat[train,],kernel = "radial",gamma = 2,cost = 1,decision.values = T)
fitted = attributes(predict(svmfit.opt,dat[train,],decision.values = T))$decision.values

par(mfrow = c(1,2))
rocplot(fitted,dat[train,"y"],main = "Training Data")

# by incrasing gamma,we can produce a more flexible fit and generate further improvements in accuracy
svmfit.flex = svm(y~.,data = dat[train,],kernel = "radial",gamma = 50,cost = 1,decision.values = T)
fitted = attributes(predict(svmfit.flex,dat[train,],decision.values = T))$decision.values
rocplot(fitted,dat[train,"y"],add = T,col = "red")


## now fitting on test data ,we see lower gamm works better
fitted = attributes(predict(svmfit.opt,dat[-train,],decision.values = T))$decision.values
rocplot(fitted,dat[-train,"y"],main = "Test Data")
fitted = attributes(predict(svmfit.flex,dat[-train,],decision.values = T))$decision.values
rocplot(fitted,dat[-train,"y"],add = T,col = "red")

############## SVM with multiple classes ########################## 
set.seed(1) 
x = rbind(x,matrix(rnorm(50*2),ncol = 2))
y = c(y,rep(0,50))
x[y == 0 ,2]=x[y==0,2]+2
dat = data.frame(x=x,y=as.factor(y))
par(mfrow = c(1,1))
plot(x,col = (y+1))

svmfit = svm(y~.,data = dat,kernel = "radial",cost = 10,gamma = 1)
plot(svmfit,dat)

##### Application to Gene Expression Data ############################

library(ISLR)
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)

table(Khan$ytrain)
table(Khan$ytest)

dat = data.frame(x = Khan$xtrain,y = as.factor(Khan$ytrain))
out = svm(y~.,data = dat,kernel = "linear",cost = 10)
summary(out)

table(out$fitted,dat$y)

dat.te = data.frame(x = Khan$xtest,y = as.factor(Khan$ytest))
pred.te = predict(out,newdata = dat.te)
table(pred.te,dat.te$y)

# cost = 10 gives 2 test errors