################## Q 10 ################################

library(ISLR)
attach(Weekly)
str(Weekly)
# 10.a 
summary(Weekly)

cor(Weekly[-9])

plot(Volume)
plot(Year,Volume)
summary(Direction)
#checking if my output is biased in the sample set
#Down 484
#Up 605


#The volume of trade has increased over the years
#From Correlation matrix it can be seen that there is not much correlation between the variables .


#10.b
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data = Weekly,family = binomial)
summary(glm.fit)
#from z value statistics only Lag 2 value seems to be statistically significant


#10.c
glm.probs=predict(glm.fit,type = "response")
glm.pred = rep("Down",nrow(Weekly))
glm.pred[glm.probs>0.5]="Up"
#confusion matrix
table(glm.pred,Direction)
mean(glm.pred==Direction)
#overall fraction of correct predictions = (54+557)/(48+430+54+557)=56% which is just better than random guessing model 

#10d
summary(Year)
train = (Year<2009) # train it is storing indices 
Weekly.testset = Weekly[!train, ]

glm.fit = glm(Direction~Lag2,data = Weekly,family = binomial,subset = train)
summary(glm.fit)

glm.probs=predict(glm.fit,type = 'response',Weekly.testset)
glm.pred = rep("Down",nrow(Weekly.testset))
glm.pred[glm.probs>.5]="Up"
#confusion matrix
table(glm.pred,Weekly.testset$Direction)
mean(glm.pred==Weekly.testset$Direction)
#correct prediction on the test set is (9+56)/(34+5+9+56) = 62.50%

#10 e
#######LDA
library(MASS)
lda.fit = lda(Direction~Lag2,data=Weekly,subset = train)
summary(lda.fit)
lda.pred = predict(lda.fit,Weekly.testset)
table(lda.pred$class,Weekly.testset$Direction)
mean(lda.pred$class==Weekly.testset$Direction)
#62.5%

#10f
##########QDA
qda.fit = qda(Direction~Lag2,data=Weekly,subset = train)
summary(lda.fit)
qda.pred = predict(qda.fit,Weekly.testset)
table(qda.pred$class,Weekly.testset$Direction)
mean(qda.pred$class==Weekly.testset$Direction)
#58.65

#10g
library(class)
train.X = as.matrix(Lag2[train])
test.X = as.matrix(Lag2[!train])
train.direction = Direction[train]
knn.pred = knn(train.X,test.X,train.direction,k=1)
mean(knn.pred==Direction[!train])
#50.96

#10h
#### Logistic Regression and LDA show better results over QDA and knn 

###########10i
glm.fit = glm(Direction~Lag2:Lag1,data = Weekly,family = binomial,subset = train)
summary(glm.fit)

glm.probs=predict(glm.fit,type = 'response',Weekly.testset)
glm.pred = rep("Down",nrow(Weekly.testset))
glm.pred[glm.probs>.5]="Up"
#confusion matrix
table(glm.pred,Weekly.testset$Direction)
mean(glm.pred==Weekly.testset$Direction)
#58.65

#######LDA
library(MASS)
lda.fit = lda(Direction~Lag2:Lag1,data=Weekly,subset = train)
summary(lda.fit)
lda.pred = predict(lda.fit,Weekly.testset)
table(lda.pred$class,Weekly.testset$Direction)
mean(lda.pred$class==Weekly.testset$Direction)
#57.69%

##########QDA
qda.fit = qda(Direction~Lag2:Lag1,data=Weekly,subset = train)
summary(lda.fit)
qda.pred = predict(qda.fit,Weekly.testset)
table(qda.pred$class,Weekly.testset$Direction)
mean(qda.pred$class==Weekly.testset$Direction)
#43.27


library(class)
train.X = cbind(Lag2[train],Lag1[train])
test.X = cbind(Lag2[!train],Lag1[!train])
train.direction = Direction[train]
knn.pred = knn(train.X,test.X,train.direction,k=1)
mean(knn.pred==Direction[!train])
#48.07
#k = 10 ,49.03
#k = 50,51.92
#k = 100,52.88

###################################### Q11 #############################
library(ISLR)
str(Auto)

#11 a
mpg01 = rep(0,length(mpg))
mpg01[mpg<=median(Auto$mpg)]=1
mpg01[mpg>median(Auto$mpg)]=0

auto.dataframe = data.frame(Auto,mpg01)

#11b
pairs(auto.dataframe)
cor(auto.dataframe[-9])
#displacement ,horsepower and weight seems to be useful feature

#11c
#trainining set and test set split 
train.size = floor(0.75*nrow(auto.dataframe))
train.sample = sample.int(nrow(auto.dataframe),train.size,replace = FALSE)#difference between sample and sample.int
auto.train = auto.dataframe[train.sample,]
auto.test = auto.dataframe[-train.sample,]

#11d
lda.fit = lda(mpg01~cylinders+weight+displacement+horsepower,data = auto.dataframe,subset = train)
lda.pred = predict(lda.fit,auto.test)
mean(lda.pred$class!=auto.test$mpg01)
#12.24

#11e
qda.fit = qda(auto.dataframe$mpg01~auto.dataframe$cylinders+auto.dataframe$weight+auto.dataframe$displacement+auto.dataframe$horsepower,data = auto.dataframe,subset = train)
qda.pred = predict(qda.fit,auto.test)
mean(qda.pred$class!=auto.test$mpg01)
#test error 43.62

#11f
glm.fit = glm(mpg01~cylinders+weight+displacement+horsepower,data = auto.dataframe,subset = train,family = binomial)
glm.probs = predict(glm.fit,auto.test,type = 'response')
glm.pred = rep(0,nrow(auto.test))
glm.pred[glm.probs>.5] = 1
mean(glm.pred!=auto.test$mpg01)
#test error = 13.26%
#11g
train.X = cbind(auto.train$cylinders,auto.train$displacement,auto.train$horsepower,auto.train$weight)
train.mpg01 = auto.train$mpg01
test.X = cbind(auto.test$cylinders,auto.test$displacement,auto.test$horsepower,auto.test$weight)

knn.pred = knn(train.X,test.X,train.mpg01,k=3)
table(knn.pred,auto.test$mpg01)
mean(knn.pred!=auto.test$mpg01)
#12.24
knn.pred = knn(train.X,test.X,train.mpg01,k=10)
table(knn.pred,auto.test$mpg01)
mean(knn.pred!=auto.test$mpg01)
#13.26
knn.pred = knn(train.X,test.X,train.mpg01,k=50)
table(knn.pred,auto.test$mpg01)
mean(knn.pred!=auto.test$mpg01)
#13.26

############################ Q12 #########################################

#Q12 a
Power = function(){
  2^3
  
}
print(Power())
#12 b
Power2 = function(x,a){
  x^a
}
print(Power2(3,5))

#12c
print(Power2(10,3))
print(Power2(8,17))
print(Power2(131,3))

#12 d 
Power3 = function(x,a){
  result = x^a
  return(result)
}

x = 1:10
plot(x, Power3(x, 2), log = "xy", ylab = "Log of y = x^2", xlab = "Log of x", 
     main = "Log of x^2 versus Log of x")

PlotPower = function(x, a) {
  plot(x, Power3(x, a))
}
PlotPower(1:10, 3)

########################## 13#################################
train = sample(nrow(Boston),size = floor(nrow(Boston)*0.75),replace = FALSE)
crim01 = rep(0,nrow(Boston))
crim01[crim>median(crim)]=1

Boston = data.frame(Boston,crim01,-crim01.1,-crim01.2)
Boston.train = Boston[train,]
Boston.test = Boston[-train,]
#attach(Boston)

str(Boston)
summary(Boston)



#Logistic
glm.fit = glm(crim01~.,data = Boston,subset = train,family = binomial)
glm.probs = predict(glm.fit,Boston.test,type = 'response')
glm.pred = rep(0,nrow(Boston.test))
glm.pred[glm.probs>.5]=1
mean(glm.pred!=Boston.test$crim01)
#test error 1.57
# LDA 

lda.fit = lda(crim01~.,data = Boston,subset = train)
lda.pred = predict(lda.fit,Boston.test)
mean(lda.pred$class!=Boston.test$crim01)  #############lda.pred$class
#test error 10.23


# qda
qda.fit = qda(crim01~.,data = Boston,subset = train)
qda.pred = predict(qda.fit,Boston.test)
mean(qda.pred$class!=Boston.test$crim01)
#1.57

# KNN
train.X=cbind(Boston.train[-1])
train.Y=cbind(Boston.train$crim01)
test.X = cbind(Boston.test[-1])

knn.pred=knn(train.X,test.X,train.Y,k=3)
mean(knn.pred!=Boston.test$crim01)
#test error 3.14
