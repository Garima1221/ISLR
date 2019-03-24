##################################### Classification Trees#################################

install.packages(c('ISLR','tree'))
library(tree)
library(ISLR)
attach(Carseats)
#Coverting continuous Sales variable into binary form
High = ifelse(Sales<8,"No","Yes")
Carseats = data.frame(Carseats,High)

tree.carseats = tree(High~.-Sales,Carseats)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats,pretty = 0)
tree.carseats

set.seed(2)
train = sample(1:nrow(Carseats),200)
Carseats.test = Carseats[-train,]
High.test = High[-train]

tree.carseats = tree(High~.-Sales,Carseats,subset = train)
tree.pred = predict(tree.carseats,Carseats.test,type = "class")
table(tree.pred,High.test)

#(86+57)/200 = 71.5 %

set.seed(3)
cv.carseats = cv.tree(tree.carseats,FUN = prune.misclass)
names(cv.carseats)

par(mfrow = c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type = 'b')
plot(cv.carseats$k,cv.carseats$dev,type = 'b')


## now pruning to 9 terminal nodes as per our result

prune.carseats = prune.misclass(tree.carseats,best = 9)
plot(prune.carseats)
text(prune.carseats,pretty = 0)

tree.pred = predict(prune.carseats,Carseats.test,type = "class")
table(tree.pred,High.test)# (94+60)/200 i.e 77% correctly classified

# increasing the value of best gives a larger pruned tree and thus lower classification accuracy

############################## Regression Trees ############################

library(MASS)
set.seed(1)
train = sample(1:nrow(Boston),nrow(Boston)/2)
tree.boston = tree(medv~.,data = Boston,subset = train)
plot(tree.boston)
text(tree.boston,pretty = 0)
## implies lower lstat results in higher housing price

#######Cross Validation #################
### tree is selected on the basis of least deviance
cv.boston = cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type = 'b')

### the tree selected from cross validation is pruned 
prune.boston = prune.tree(tree.boston,best =7)
plot(prune.boston)
text(prune.boston,pretty = 0)

## comparing between unpredict

yhat = predict(tree.boston,newdata = Boston[-train,])
boston.test = Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat - boston.test)^2)
# MSE = 25 .04

################### Bagging and Random Forest ##################

### Bagging is special case of random forest with m=p 
library(randomForest)
set.seed(1)

# using all 13 predictors
bag.boston = randomForest(medv~.,data = Boston,subset = train, mtry = 13,importance = TRUE)
bag.boston

yhat.bag = predict(bag.boston,newdata = Boston[-train,])
plot(yhat.bag,boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)
#13.16

## using bagging 
bag.boston = randomForest(medv~.,data = Boston,subset = train,mtry = 13,ntree = 25)
yhat.bag = predict(bag.boston,newdata = Boston[-train,])
mean((yhat.bag-boston.test)^2)
## 13.43

## tweaking mtry parameter of randomForest and comparing with above baging result
set.seed(1)
rf.boston = randomForest(medv~.,data = Boston,subset = train,mtry = 6,importance = TRUE)
yhat.rf = predict(rf.boston,newdata = Boston[-train,])
mean((yhat.rf-boston.test)^2)
#11.48

## random forest gave better prediction over bagging by altering parameter mtry

# checking importance of each variable 
importance(rf.boston)

varImpPlot(rf.boston)

################## Boosting ################################################

library(gbm)
set.seed(1)
boost.boston = gbm(medv~.,data = Boston[train,],distribution = "gaussian",n.trees = 5000 , interaction.depth = 4)
summary(boost.boston)

par(mfrow = c(1,2))
plot(boost.boston,i="rm")
plot((boost.boston-boston.test)^2)

yhat.boost = predict(boost.boston,newdata = Boston[-train,],n.trees = 5000)
mean((yhat.boost-boston.test)^2)
## 11.84

boost.boston = gbm(medv~.,data = Boston[train,],distribution = "gaussian",n.trees = 5000,interaction.depth = 4,shrinkage = 0.2,verbose = F)
yhat.boost = predict(boost.boston,newdata = Boston[-train,],n.trees = 5000)
mean((yhat.boost - boston.test)^2)
#11.51

