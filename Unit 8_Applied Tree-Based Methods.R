######################### 7 ########################################################################

library(MASS)
library(randomForest)

set.seed(8)
train = sample(1:nrow(Boston),nrow(Boston)/2)
test = -train

#defining different p for mtry bagging
p1 = ncol(Boston)-1#excluding response variable in the predictor set
p2 = p1/2
p3 = sqrt(p1)


## choosing ntree from a range of values between 1 to no. of observations at an interval of 50
ntree = seq(1,nrow(Boston),50)
mse.ntree = rep(NA,length(ntree))

#for p1
for(i in 1:length(ntree)){
  bag.boston1 = randomForest(medv~.,data = Boston,subset = train,mtry = p1,ntree = ntree[i],importance = TRUE) 
  yhat.bag1 = predict(bag.boston1,newdata = Boston[train,])
  mse.ntree[i] = mean((yhat.bag1-Boston$medv)^2)
}

which.min(mse.ntree)#index 2 shows minimum training mse
bag.boston = randomForest(medv~.,data = Boston,subset = train,mtry = p1,ntree = ntree[which.min(mse.ntree)],importance = TRUE)
plot(1:ntree[which.min(mse.ntree)],bag.boston$mse,type = 'l',col = 'red',xlab = "No. of trees",ylab = "MSE")


# for p2
for(i in 1:length(ntree)){
  bag.boston2 = randomForest(medv~.,data = Boston,subset = train,mtry = p2,ntree = ntree[i],importance = TRUE)  
  yhat.bag2 = predict(bag.boston2,newdata = Boston[train,])
  mse.ntree[i] = mean((yhat.bag2-Boston$medv)^2)
  
}

which.min(mse.ntree)#index 2 shows minimum training mse
bag.boston = randomForest(medv~.,data = Boston,subset = train,mtry = p1,ntree = ntree[which.min(mse.ntree)],importance = TRUE)
lines(1:ntree[which.min(mse.ntree)],bag.boston$mse,col = 'blue')

#for p3
for(i in 1:length(ntree)){
  bag.boston3 = randomForest(medv~.,data = Boston,subset = train,mtry = p3,ntree = ntree[i],importance = TRUE)  
  yhat.bag3 = predict(bag.boston3,newdata = Boston[train,])
  mse.ntree[i] = mean((yhat.bag3-Boston$medv)^2)
  
}

which.min(mse.ntree)#index 2 shows minimum training mse
bag.boston = randomForest(medv~.,data = Boston,subset = train,mtry = p1,ntree = ntree[which.min(mse.ntree)],importance = TRUE)
lines(1:ntree[which.min(mse.ntree)],bag.boston$mse,col = 'green')

legend("topright", c("m=p", "m=p/2", "m=sqrt(p)"), col = c("red", "blue", "green"),cex = 1, lty = 1)


################################8##################################################################
rm(list=ls())
library(ISLR)
library(randomForest)
library(tree)
attach(Carseats)
str(Carseats)

set.seed(8)

#8a
#splitting training and test set 
train = sample(1:nrow(Carseats),nrow(Carseats)/2)
Carseats.train = Carseats[train,]
Carseats.test = Carseats[-train,]

#8b
#fitting regression tree and plotting it 
tree.carseats = tree(Sales~.,data = Carseats ,subset = train)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty = 0)
#Variables actually used in tree construction:
#{"ShelveLoc"   "Price"       "Age"         "CompPrice"   "Advertising" "Income" }
#Residual mean deviance: = 2.27

predict.carseats = predict(tree.carseats,Carseats.test)
mean((Carseats.test$Sales-predict.carseats)^2)
#mse 4.50

## 8 c
set.seed(8)
## selecting trees through cross validation cross validation 
cv.carseats = cv.tree(tree.carseats)
cv.carseats#deviance is lowest with 7 terminal nodes i.e 11 th index showing lowest deviance of 997.6011
#plotting tree size vs deviance 
plot(cv.carseats$size,cv.carseats$dev,type = 'b')

## pruning the tree with best size = 7
prune.carseats = prune.tree(tree.carseats,best = 7)
summary(prune.carseats)
plot(prune.carseats)
text(prune.carseats,pretty = 0)
## misclassification = 0.175

predict.car = predict(prune.carseats,Carseats.test)
mean((predict.car-Carseats.test$Sales)^2)

#mse =4.58
#no pruning did not help 

##### bagging using ntree = 50 and using all 11 predictors
library(randomForest)
bag.carseats = randomForest(Sales~.,data = Carseats , subset = train,mtry = 11 , ntree = 50 ,importance = TRUE)
bag.carseats
bag.predict = predict(bag.carseats,Carseats.test)
mean((bag.predict-Carseats.test$Sales)^2)
# MSE = 2.49
importance(bag.carseats)


##random forest
##### bagging using ntree = 50 and using all 5 predictors
rf.carseats = randomForest(Sales~.,data = Carseats , subset = train,mtry = 10,importance = TRUE)
rf.carseats
rf.predict = predict(rf.carseats,Carseats.test)
mean((rf.predict-Carseats.test$Sales)^2)
# MSE = 2.59 for m = 5
# MSE = 2.38 for m = 10
## increasing m ,MSE decreases
importance(rf.carseats)


###############################################9###########################################
rm(list = ls())
library(ISLR)
str(OJ)

#9a
## creating a training set 
train = sample(1:nrow(OJ),800)
test = -train

#9b
tree.OJ = tree(Purchase~.-Buy,data = OJ,subset = train)
summary(tree.OJ)
# Only 4 predictor used for tree condtruction :"LoyalCH"   "PriceDiff" "SpecialCH" "StoreID" 
# Residual mean deviance = 0.7483
# Misclassification error rate = 0.16

#9c
tree.OJ
#if LoyalCH <0.48285(307 such observations) =>LoyalCH<0.261626(109 such observations)=>Purchase will be MM

#9d
plot(tree.OJ)
text(tree.OJ,pretty = 0)

#9e
tree.predict = predict(tree.OJ,OJ[test,],type = 'class')
table(tree.predict,OJ[test,]$Purchase)
#tree.predict   CH  MM
#           CH 136  25
#           MM  34  75

# Test error = 21.85%

#9f
cv_tree = cv.tree(tree.OJ,FUN = prune.misclass)
## Optimal tree size = 8 or 7 with deviance = 150

#9g 
plot(cv_tree$size,cv_tree$dev,type = 'l')

#9h
#Size of 8 or 7

#9i 
prune_tree = prune.tree(tree.OJ,best = 7)
summary(prune_tree)
# Test error = 16.62
prune_tree = prune.tree(tree.OJ,best = 5)
summary(prune_tree)
## Test error 17.75

## 9 j
# Training error rate of unpruned tree is higher 


##9k

predict.unprune = predict(tree.OJ,OJ[test,],type = 'class')
table(predict.unprune,OJ[test,]$Purchase)
#    CH  MM
#CH 136  25
#MM  34  75
# Test error = 21.85 
predict.prune = predict(prune_tree,OJ[test,],type = 'class')
table(predict.prune,OJ[test,]$Purchase)

#predict.prune  CH  MM
#CH 125  13
#MM  45  87
# Test error = 21.48# Only slight improvement


###################################### 10 ###############################################
#10 a 
set.seed(108)
Hitters_data = Hitters
View(Hitters)

sum(is.na(Hitters_data))
Hitters_data = Hitters_data[-which(is.na(Hitters_data$Salary)),]

Hitters_data$Salary = log(Hitters_data$Salary)

#10b
train = 1:200
test = (-train)

#10c 
pow = seq(-3,-1,by = 0.1)
lambda = 10^pow

boost.trainerror = rep(0,length(lambda))
boost.testerror = rep(0,length(lambda))


library(gbm)
for(i in 1:length(lambda)){
  boost.Hitters = gbm(Salary~.,data = Hitters_data[train,],n.trees = 1000,shrinkage = lambda[i],distribution = "gaussian")
  
  boost.trainpredict = predict(boost.Hitters,Hitters_data[train,],n.trees = 1000)
  boost.testpredict = predict(boost.Hitters,Hitters_data[test,],n.trees = 1000)
  
  boost.trainerror[i] = mean((Hitters_data[train,]$Salary-boost.trainpredict)^2)
  boost.testerror[i] = mean((Hitters_data[test,]$Salary-boost.testpredict)^2)
}

plot(lambda,boost.trainerror,type = 'l')

#10 d 
plot(lambda,boost.testerror,type = 'l')
# min at lambda = 0.04 at index 17
#boost.testerror[17]
#[1] 0.2620017

#10e 
##Linear Regression
fit.lm = lm(Salary~.,data = Hitters_data,subset = test)
predictlm = predict.lm(fit.lm,Hitters_data[test,])
mean((predictlm-Hitters_data[test,]$Salary)^2)
#0.28

#Lasso Regression
install.packages(glmnet)
library(glmnet)

XHitters = model.matrix(Salary~.,data = Hitters_data[train,])
Xtest.Hitters = model.matrix(Salary~.,data = Hitters_data[test,])
YHitters = Hitters_data[train,]$Salary
lasso.fit = glmnet(XHitters,YHitters,alpha = 1)
lasso.pred = predict(lasso.fit,s = 0.01,  newx = Xtest.Hitters)
mean((lasso.pred-Hitters_data[test,]$Salary)^2)
#0.47

#10f 
boost.best = gbm(Salary~.,Hitters_data[train,],distribution = "gaussian",n.trees = 1000,shrinkage = 0.04)
summary(boost.best)
#Important variables
#            var    rel.inf
#CAtBat       CAtBat 25.5146705
#CWalks       CWalks  8.2117863
#CRBI           CRBI  8.0819652
##10g 
library(randomForest)
fit.rf = randomForest(Salary~.,data = Hitters_data,subset = train,ntree=500,mtry=19)
predict.rf = predict(fit.rf,Hitters_data[test,])
mean((predict.rf-Hitters_data[test,]$Salary)^2)
#0.22

#################################11##################################################

#11a
rm(list = ls())
library(ISLR)
train = 1:1000
test = -(train)

#11b
Caravan_data = Caravan
Caravan_data$Purchase = ifelse(Caravan_data$Purchase == "Yes",1,0)

fit.boost = gbm(Purchase~.,data = Caravan_data[train,],distribution = "bernoulli",n.trees = 1000,shrinkage = 0.01)
summary(fit.boost)
#            var     rel.inf
#PPERSAUT PPERSAUT 13.24225740
#MKOOPKLA MKOOPKLA 11.16725598
#MOPLHOOG MOPLHOOG  7.35088360

#11c
predict.probboost = predict(fit.boost,Caravan_data[test,],n.trees = 1000,type = "response")

##
predict.boost = ifelse(predict.probboost>0.20,1,0)

table(Caravan_data[test,]$Purchase,predict.boost)
31/149
# Only 20.80% of the people 

fit.glm = glm(Purchase~.,data = Caravan_data[train,],family = binomial)
predict.probglm = predict(fit.glm,Caravan_data[test,],type = "response")
pred.glm = ifelse(predict.probglm>0.20,1,0)
table(Caravan_data[test,]$Purchase,pred.glm)
58/408

##########################################################################
