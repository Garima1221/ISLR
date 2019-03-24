library(MASS)
# fix allows editing on Boston Data set 
fix(Boston)
#check features available in Boston data
names(Boston)

#fitting linear model to check response medv using a single predictor lstat in Boston dataset 
lm.fit = lm(medv~lstat,data = Boston)
lm.fit

#Checking some basic information of our linear model 
summary(lm.fit)

#checking information contained in lm.fit
names(lm.fit)

#cheking confidence interval of the coefficient estimates
confint(lm.fit)

#confidence intervals and prediction intervals for the prediction of medv for a given value of lstat

predict(lm.fit,data.frame(lstat=(c(5,10,15))),interval = "confidence")
predict(lm.fit,data.frame(lstat =(c(5,10,15))),interval = "prediction")

#plotting lstat vs medv and the regression line 
attach(Boston)
plot(lstat,medv)
#abline(lm.fit)
#abline(lm.fit,lwd = 3)
abline(lm.fit,lwd = 3,col = "red")
#plot(1:20.1:20,pch=1:20)

#plotting diagnostic plots 

par(mfrow=c(2,2))
plot(lm.fit)

#OR other ways of plotting residuals ,rstudent and leverage values

plot(predict(lm.fit),residuals(lm.fit))
plot(predict(lm.fit),rstudent(lm.fit)) ## the curve plot represent nonlinearity
plot(predict(lm.fit),hatvalues(lm.fit))#for leverage values

############################## Multiple Linear Regression  ############################################

# using 2 predictors lstat and age 
lm.fit = lm(medv~lstat+age,data = Boston)
summary(lm.fit)

#using all the 13 predictors in Boston data 

lm.fit = lm(medv~.,data = Boston)
summary(lm.fit)

#checking multicollinearity in the dataset through vif() function present in car library
install.packages("car")
library(car)
vif(lm.fit)

#age has high p value ,so we can accept Null hypothesis and age has no effect on response
#thus we consider a new fit by removing the age feature 

lm.fit1 = lm(medv~.-age,data = Boston)
summary(lm.fit1)

#OR we can also use update function for the same 

lm.fit1 = update(lm.fit,~.-age)


#Adding interaction term

summary(lm(medv~lstat*age,data = Boston))

#adding non linear transformation 

lm.fit2 = lm(medv~lstat+I(lstat^2))
summary(lm.fit2)


#ANOVA test to check improvement in our model through non linear transformation
lm.fit=lm(medv~lstat,data=Boston)
anova(lm.fit,lm.fit2)

#Higher order polynomials using poly function 
lm.fit5 = lm(medv~poly(lstat,5))
summary(lm.fit5)
#improvement in model fit 



# using log transformation 
summary(lm(medv~log(rm),data = Boston))


######################################Qualitative Predictors ##################################

library(ISLR)
fix(Carseats)
names(Carseats)

#Creating linear model using interaction terms
lm.fit = lm(Sales~.+Income:Advertising +Price:Age,data = Carseats)
summary(lm.fit)

#contrasts() function is used to return the coding that R uses for dummy variable
attach(Carseats)
contrasts(ShelveLoc)





#############################Creating functions in R ############################################

loadLibrary = function(){
  library(ISLR)
  library(MASS)
  print("The libraries have been loaded")
}

loadLibrary()




             
       

        
      
        
        