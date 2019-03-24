library(ISLR)
names(Smarket)

cor(Smarket)

cor(Smarket[,-9])

attach(Smarket)

plot(Volume)

#fitting Logistic Regression model
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data = Smarket,family = binomial)
summary(glm.fit)

coef(glm.fit)
#or 
summary(glm.fit)$coef[,4]

glm.probs = predict(glm.fit,type = "response")
glm.probs[1:10]
contrasts(Direction)

glm.pred = rep("Down",1250)
glm.pred[glm.probs>0.5]="Up"

table(glm.pred,Direction)

#correct prediction that is TP +TN/TP+TN+FP+FN

mean(glm.pred==Direction)

#train will have TRUE FALSE values of Smarket size satisfying the condition Year <2005

train = Year<2005
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005 = Direction[!train]

#creating glm model 

glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data = Smarket,family = binomial,subset = train)
glm.probs = predict(glm.fit,Smarket.2005,type = 'response')

#we now make direction of stocks up for whom the model gave probabilities of grater than 0.5

glm.pred = rep("Down",252)
glm.pred[glm.probs>.5]="Up"

#now creating confusion matrix on our test data
table(glm.pred,Direction.2005)

#test set error
mean(glm.pred!=Direction.2005)
# stll high at 52% 

# we try removing some of the predictors and refit the model

glm.fit = glm(Direction~Lag1+Lag2,data = Smarket,family = binomial,subset = train)
glm.probs =predict(glm.fit,Smarket.2005,type = 'response')
glm.pred[glm.probs>0.5]="Up"
table(glm.pred,Direction.2005)

mean(glm.pred==Direction.2005)
#58% slight improvement

#predict for a particular value of Lag1 and Lag2

predict(glm.fit,newdata = data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type = 'response')

##############LDA#################################

library(MASS)
lda.fit = lda(Direction~Lag1+Lag2,data = Smarket ,subset = train)
lda.fit
plot(lda.fit)

lda.pred = predict(lda.fit,Smarket.2005)
names(lda.pred)

lda.class = lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class==Direction.2005)

sum(lda.pred$posterior[,1]>=0.5)
sum(lda.pred$posterior[,1]<0.5)


###############################QDA######################################

qda.fit = qda(Direction~Lag1+Lag2,data = Smarket,subset = train)
qda.fit

qda.class = predict(qda.fit,Smarket.2005)$class
table(qda.class,Direction.2005)

mean(qda.class==Direction.2005)


############################KNN #########################################

library(class)
train.x = cbind(Lag1,Lag2)[train,]
test.x = cbind(Lag1,Lag2)[!train,]
train.Direction = Direction[train]

set.seed(1)
knn.pred = knn(train.x,test.x,train.Direction,k=1)
table(knn.pred,Direction2005)

# k =1 result is 0.50 

# thus we increase k

knn.pred = knn(train.x,test.x,train.Direction,k=3)
table(knn.pred,Direction2005)

mean(knn.pred==Direction.2005)
#0.536

###############################Caravan insurance dataset ##################################3

dim(Caravan)
attach(Caravan)
summary(Purchase)

#as scale oof data set is different we standardise the variables using scale()

standardized.X = scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

##############################Splitting data into test set and training set #####################

test = 1:1000
train.x = standardized.X[test,]
test.x = standardized.X[test,]
train.y = Purchase[-test]
test.y = Purchase[test]
set.seed(1)
knn.pred = knn(train.x,test.x,train.y,k=1)
mean(test.y!=knn.pred)
mean(test.y!="No")

table(knn.pred,test.y)
#success rate 11.7% ,k=1

knn.pred = knn(train.x,test.x,train.y,k=3)
table(knn.pred,test.y)
#success rate 19.2 ,k=3

knn.pred = knn(train.x,test.x,train.y,k=5)
table(knn.pred,test.y)
#success rate 26.7, k =5


glm.fit = glm(Purchase~.,data = Caravan,family = binomial,subset = -test)
glm.probs=predict(glm.fit,Caravan[test,],type = 'response')
glm.pred = rep("No",1000)
glm.pred[glm.probs>.5]='Yes'
table(glm.pred,test.y)
#success rate 33% ,5 times better than random guessing i.e 6%(only6/1000 had purchased the insurance)




