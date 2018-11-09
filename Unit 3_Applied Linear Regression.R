########################################ISLR Unit 3 Linear Regression #####################################

setwd("F:/DATA SCIENCE/ISLR/Dataset")
getwd()
#to check dataset that exist in the folder
list.files()
#convert ? as na values
auto_data = read.csv("Auto.csv",na.strings = "?",header = TRUE)
# remove na values
auto_data = na.omit(auto_data)

#structure of auto data
#a. Finding qualitative and quantitative factors in the data set
#we find that accept name all data are quantitative in nature but not necessarily numeric
str(auto_data)

#summary will give min and max value for each quantitative variable 
summary(auto_data)

######## Q 8 ############
##a##
#Suppose that we wish to predict gas mileage ("mpg") on the basis of other variables. Do your plots suggest that any of the other variables might be useful in predicting "mpg" ?
#From the plots above, the cylinders, horsepower, year and origin can be used as predictors. Displacement and weight were not used because they are highly correlated with horespower and with each other.
lm.fit = lm(mpg~horsepower,auto_data)
summary(lm.fit)
#Residuals:
#Min       1Q   Median       3Q      Max 
#-13.5710  -3.2592  -0.3435   2.7630  16.9240 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)          39.935861   0.717499   55.66   <2e-16 ***
#  auto_data$horsepower -0.157845   0.006446  -24.49   <2e-16 ***
---
  #  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
  
  #Residual standard error: 4.906 on 390 degrees of freedom
  #Multiple R-squared:  0.6059,	Adjusted R-squared:  0.6049 
  #F-statistic: 599.7 on 1 and 390 DF,  p-value: < 2.2e-16
  ##Is there a relationship between the predictor and the response:
  # a
  # i
  ## F value is 599.7 which is very large .Thus making us reject Ho
  # Null Hypothesis : There is no relationship between the predictor and the response   is rejected as p value is small
  # Hence there is a relationship between the predictor(horsepower) and the response (mpg)
##ii
# generates R2 and RSE
summary(lm.fit)$r.sq
#0.60
summary(lm.fit)$sigma
#4.90

#The R2 of the lm.fit was about 0.6059, meaning 60.5948% of the variance in mpg is explained by horsepower.
#RSE of the model is 4.90
##iii The relationship between predictor and response is negative as evident from the sign of coefficient of horsepower in the regression line


## iV Predicted mpg for horsepower of 98
# Linear Regression : y = mx + c
# mpg = 39.94 - 0.16*Horsepower
mpg = 39.94 -0.16*98
#24.26
## 95% Confidence Interval 
predict(lm.fit,data.frame(horsepower = 98),interval = "confidence")
#fit      lwr      upr
#1 24.46708 23.97308 24.96108
##95% prediction interval
predict(lm.fit,data.frame(horsepower = 98),interval = "prediction")
#    fit     lwr      upr
#1 24.46708 14.8094 34.12476
par(mfrow= c(2,2))
plot(lm.fit)

#b##
plot(auto_data$horsepower,auto_data$mpg,col = "red")
abline(39.93,-0.16)

#c. diagnostic plot 
plot(lm.fit)
## Residual vs fitted has a pattern implies non linear pattern but not heteroskedacity
##Normal Q-Q plot is on the diagonal line implies normality
##Scale Location non linearity can be seen but no heteroskedacity
## No influential or leverage point seen

###############Q 9 #######################

# 9.a Produce a scatter plot matrix which includes all the variables
pairs(auto_data)

# 9.b Compute matrix of correlations between the variables usign the function cor().Exclude the name variable which is qualitative
cor(subset(auto_data, select=-name))

# 9.c Multiple Linear Regression
lm.fit1 = lm(mpg~.-name,data = auto_data)
summary(lm.fit1)

#i Yes there is a relationship between the predictors - displacement,weight,year,origin and the response
#ii Smaller p values tend to have statistically significant  relationship like displacment ,weight,year and origin
#iii The regression coefficient for year is 0.751 which suggests with every year mpg improves by 0.751 unit 
# i.e is cars become more fuel efficient 

# 9.d Use the plot() function to produce diagnostic plots

plot(lm.fit1)

# Residual vs fitted :As the residual plot shows  pattern ,it implies the non linearity in the relationship between the predictors and response
# But no heteroskedacity
#Normal QQ plot - normality 
#Scale Location : no pattern
#High leverage point at 14

#plot(predict(lm.fit1),rstudent(lm.fit1))

# Here, points greater than 3 are outliers

#e. Adding interaction effect
# Using :
#lm.fit2 = lm(auto_data$mpg~auto_data$cylinder:auto_data$displacement+auto_data$cylinder+auto_data$displacement+auto_data$displacement:auto_data$weight+auto_data$weight)
#using *
lm.fit2 = lm(auto_data$mpg~auto_data$cylinders*auto_data$displacement+auto_data$displacement*auto_data$weight)
summary(lm.fit2)
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                                 5.262e+01  2.237e+00  23.519  < 2e-16 ***
#  auto_data$cylinders                         7.606e-01  7.669e-01   0.992    0.322    
#auto_data$displacement                     -7.351e-02  1.669e-02  -4.403 1.38e-05 ***
#  auto_data$weight                           -9.888e-03  1.329e-03  -7.438 6.69e-13 ***
#  auto_data$cylinders:auto_data$displacement -2.986e-03  3.426e-03  -0.872    0.384    
#auto_data$displacement:auto_data$weight     2.128e-05  5.002e-06   4.254 2.64e-05 ***

# from p values it can be seen that interaction term of displacement and weight predictors are statistically significant .
#while we can ignore the cylinder and displacement interaction effect
#f
#log(weight), sqrt(horsepower), and acceleration^2 all have statistical significance of some sort. The residuals plot has less of a discernible pattern than the plot of all linear regression terms. The studentized residuals displays potential outliers (>3). The leverage plot indicates more than three points with high leverage.
lm.fit3 = lm(auto_data$mpg~log(auto_data$weight)+sqrt(auto_data$horsepower)+auto_data$acceleration+I(auto_data$acceleration^2))
summary(lm.fit3)

par(mfrow=c(2,2))
plot(lm.fit3)
plot(predict(lm.fit3), rstudent(lm.fit3))

#log(weight), sqrt(horsepower), and acceleration^2 all have statistical significance of some sort. The residuals plot has less of a discernible pattern than the plot of all linear regression terms. The studentized residuals displays potential outliers (>3). The leverage plot indicates more than three points with high leverage.

#However, 2 problems are observed from the above plots: 1) the residuals vs fitted plot indicates heteroskedasticity (unconstant variance over mean) in the model. 2) The Q-Q plot indicates somewhat unnormality of the residuals.

#So, a better transformation need to be applied to our model. From the correlation matrix in 9a., displacement, horsepower and weight show a similar nonlinear pattern against our response mpg. This nonlinear pattern is very close to a log form. So in the next attempt, we use log(mpg) as our response variable.

#The outputs show that log transform of mpg yield better model fitting (better R^2, normality of residuals).
lm.fit2<-lm(log(mpg)~cylinders+displacement+horsepower+weight+acceleration+year+origin,data=auto_data)
summary(lm.fit2)
par(mfrow=c(2,2)) 
plot(lm.fit2)

############ Q 10 ######################
# loading Carseats data from ISLR library
library(ISLR)
summary(Carseats)
str(Carseats)

#overview of correlation between the variables

#through scatter plot
#pairs(Carseats)

# and 
#through correlation matrix 
#correlation matrix contains only numeric character 
str(Carseats)
#cor(subset(Carseats,select = -c(ShelveLoc,Urban,US)))

#attaching variables to the Carseats dataset
attach(Carseats)

#10 a 
#fitting multiple regression model
lm.fit = lm(Sales~Price+C(Urban)+US,data = Carseats)
summary(lm.fit)
##b Price and US are statistically significant while urn=ban is not
## adj R2 = 0.2335
#Residual standard error: 2.472
## F = 41.52 ,p value = 2.2e^-16
#c Sales = 13.04 -0.054*Price +1.20 US(US = Yes)
# d For Price and US = Yes we can reject the null hypothesis
#e
lm.fit1 = lm(Sales~Price+US,data = Carseats)
summary(lm.fit1)
## adj R2 = 0.2354
## F = 62.43 ,p value = 2.2e^-16
#RSE = 2.469
##f 
plot(Carseats$Sales)
abline(lm.fit,col = 1)
line(lm.fit1,col = 5)

#10f
#summary(lm.fit)#comparingthe summary of the 2 models ,we see only slight improvement of on removing the Urban predictor .RSE has decreased to 2.469 from 2.472

#to check diagonastic graph .Residual vs Fitted graph confirms that predictor vs target relationship is linear

#10g
#predict Sales value in Confidence 
predict(lm.fit1,data.frame(Carseats),interval = "confidence")

# to find outliers 
#10 h we find that all values lie within (-3,3) range .Hence ,no outlier
plot(predict(lm.fit1),rstudent(lm.fit1))

# Leverage points can be seen in the below plot 
plot(lm.fit1)


#10f
#summary(lm.fit)#comparingthe summary of the 2 models ,we see only slight improvement of on removing the Urban predictor .RSE has decreased to 2.469 from 2.472


################## Q 11 #################################
#Generate predictor x  and response y 
set.seed(1)
x = rnorm(100)
y = 2*x*rnorm(100)

# 11 a
#performing simple linear regression without an intercept term.p value is very small so we can reject the null hypothesis
lm.fit = lm(y~x+0)
summary(lm.fit)
#Coefficient = -0.45
# RSE 1.416
# t stat = sqrt = 2.86
##p value = 0.00508

#11 b
# performing linear regression from x onto y
# p value is almost 0 so we reject the null hypothesis
lm.fit1 = lm(x~y+0)
summary(lm.fit1)

# t-stat = 2.865
# p 0.00508
# coeff = -0.1699

#11 c
# both the results are similar

## 11 d
# t stat using the formula is same as computed in summary 
(sqrt(length(x)-1) * sum(x*y)) / (sqrt(sum(x*x) * sum(y*y) - (sum(x*y))^2))

#11 e 11f
lm.fit = lm(y~x)
lm.fit1 = lm(x~y)
summary(lm.fit)
summary(lm.fit1)

#########################################################################################
#12 a
#When the sum of the squares of the observed y-values are equal to the sum of the squares of the observed x-values.

#12b
set.seed(1)
x = rnorm(100)
y = 2*x
lm.fit = lm(y~x+0)
lm.fit2 = lm(x~y+0)
summary(lm.fit)
summary(lm.fit2)
#The regression coefficients are different for each linear regression.

#12c
set.seed(2)
x = rnorm(100)
y = -sample(x,100)
sum(x^2)
sum(y^2)
lm.fit = lm(y~x+0)
lm.fit2 = lm(x~y+0)
summary(lm.fit)
summary(lm.fit2)

## Same coeff
########################################13####################################################

#13
#a
set.seed(100)
X = rnorm(100,mean =0,sd =1)
#b
eps = rnorm(100, mean = 0,sd = 0.25)

#c
Y = -1+0.5*X+eps

nrow(Y)

#beta0 = -1,beta1 = 0.49973
# size of vector Y = 100

dev.off()
#13 d
plot(X,Y) # linear positive

#13 e 
lm.fit = lm(Y~X)
summary(lm.fit)
#beta0_hat = -0.99 ,beta0 = -1
#beta1_hat = 0.47 , beta1 = 0.5

#13 f
par(mar=c(1,1,1,1))
abline(lm.fit,lwd = 3, col = "green")
abline(-1,0.5,lwd = 3,col = "red")
legend(-1, legend = c("actual", "pred"), col=2:3, lwd=3)

#13 g
lm.fit1 = lm(y~x+I(x^2))
summary(lm.fit1)

#ffor lm.fit1 :
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -1.01090    0.03338 -30.286   <2e-16 ***
#  x            0.48732    0.02148  22.683   <2e-16 ***
#  I(x^2)       0.01336    0.01675   0.798    0.427    
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.2477 on 97 degrees of freedom
#Multiple R-squared:  0.8424,	Adjusted R-squared:  0.8391 
#F-statistic: 259.2 on 2 and 97 DF,  p-value: < 2.2e-16

#for lm.fit :
#Residual standard error: 0.2472 on 98 degrees of freedom
#Multiple R-squared:  0.8414,	Adjusted R-squared:  0.8397 
#F-statistic: 519.8 on 1 and 98 DF,  p-value: < 2.2e-16


# R2 and RSE has increased by introducing quadratic term and p value is high suggesting we can ignore X^2
# the model p value is sufficiently high ,thus we accept null hypothesis
#13h
set.seed(1)
eps1 = rnorm(100, 0, 0.125)
x1 = rnorm(100)
y1 = -1 + 0.5*x1 + eps1
plot(x1, y1)
lm.fit1 = lm(y1~x1)
summary(lm.fit1)

abline(lm.fit1, lwd=3, col=2)
abline(-1, 0.5, lwd=3, col=3)
legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)

# R2 and RSE decreased considerably

# 13 i

set.seed(1)
eps2 = rnorm(100, 0, 0.5)
x2 = rnorm(100)
y2 = -1 + 0.5*x2 + eps2
plot(x2, y2)
lm.fit2 = lm(y2~x2)
summary(lm.fit2)

abline(lm.fit2, lwd=3, col=2)
abline(-1, 0.5, lwd=3, col=3)
legend(-1, legend = c("model fit", "pop. regression"), col=2:3, lwd=3)

# R2 and RSE increases considerably

#13j 
confint(lm.fit)
confint(lm.fit1)
confint(lm.fit2)

#All intervals seem to be centered on approximately 0.5, with the second fit's interval being narrower than the first fit's interval and the last fit's interval being wider than the first fit's interva

######################################## Q 14 ########################################################

# 14 a
set.seed(1)
x1 = runif(100)
x2 = 0.5 * x1 + rnorm(100)/10
y = 2 + 2*x1 + 0.3*x2 + rnorm(100)
# beta0 = 2
# beta1 = 2
# beta2 = 0.3


#14 b
cor(x1, x2)

plot(x1, x2)

#14 c
lm.fit = lm(y~x1+x2)
summary(lm.fit)

#beta0_hat=2.0533,beta1_hat=1.4396,beta2_hat=1.0097
#beta1 - no 
#beta2 - yes ,reject null hypothesis

# 14 d
lm.fit = lm(y~x1)
summary(lm.fit)
#yes ,reject null hypothesis ,x1 is statistically significant 

#14 e 

lm.fit = lm(y~x2)
summary(lm.fit)

#14f yes
#Yes, we can reject the null hypothesis for the regression coefficient given the p-value for its t-statistic is near zero.

#No, because x1 and x2 have collinearity, it is hard to distinguish their effects when regressed upon together. When they are regressed upon separately, the linear relationship between y and each predictor is indicated more clearly.


x1 = c(x1, 0.1)
x2 = c(x2, 0.8)
y = c(y, 6)
lm.fit1 = lm(y~x1+x2)
summary(lm.fit1)

lm.fit2 = lm(y~x1)
summary(lm.fit2)

lm.fit3 = lm(y~x2)
summary(lm.fit3)

#In the first model, it shifts x1 to statistically insignificance and shifts x2 to statistiscal significance from the change in p-values between the two linear regressions.


par(mfrow=c(2,2))
plot(lm.fit1)## Leverage point ,outlier

par(mfrow=c(2,2))
plot(lm.fit2) ##no  Leverage point ,no outlier

par(mfrow=c(2,2))
plot(lm.fit3) ## Leverage point ,no outlier

plot(predict(lm.fit1), rstudent(lm.fit1))
plot(predict(lm.fit2), rstudent(lm.fit2))
plot(predict(lm.fit3), rstudent(lm.fit3))

################################################# Q 15  #################################################
## 15a 
library(MASS)
summary(Boston)
str(Boston)

Boston$chas = factor(Boston$chas, labels = c("N","Y"))
summary(Boston)


pairs(Boston)
cor(Boston)

attach(Boston)
lm.zn = lm(crim~zn)
summary(lm.zn) # yes

lm.indus = lm(crim~indus)
summary(lm.indus) # yes

lm.chas = lm(crim~chas) 
summary(lm.chas) # no

lm.nox = lm(crim~nox)
summary(lm.nox) # yes

lm.rm = lm(crim~rm)
summary(lm.rm) # yes

lm.age = lm(crim~age)
summary(lm.age) # yes

lm.dis = lm(crim~dis)
summary(lm.dis) # yes


lm.rad = lm(crim~rad)
summary(lm.rad) # yes

lm.tax = lm(crim~tax)
summary(lm.tax) # yes

lm.ptratio = lm(crim~ptratio)
summary(lm.ptratio) # yes

lm.black = lm(crim~black)
summary(lm.black) # yes

lm.lstat = lm(crim~lstat)
summary(lm.lstat) # yes

lm.medv = lm(crim~medv)
summary(lm.medv) # yes

lm.all = lm(crim~., data=Boston)
summary(lm.all)

####All, except chas. Plot each linear regression using "plot(lm)" to see residuals.

# 15 b

lm.all = lm(crim~., data=Boston)
summary(lm.all)

#Except for these variables : zn, dis, rad, black, medv,others can be ignored

# 15 c
x = c(coefficients(lm.zn)[2],
      coefficients(lm.indus)[2],
      coefficients(lm.chas)[2],
      coefficients(lm.nox)[2],
      coefficients(lm.rm)[2],
      coefficients(lm.age)[2],
      coefficients(lm.dis)[2],
      coefficients(lm.rad)[2],
      coefficients(lm.tax)[2],
      coefficients(lm.ptratio)[2],
      coefficients(lm.black)[2],
      coefficients(lm.lstat)[2],
      coefficients(lm.medv)[2])
y = coefficients(lm.all)[2:14]
plot(x, y)
lm.fit = lm(Boston~.,data = Boston)
summary(lm.fit)

# Coefficient for nox is approximately -10 in univariate model and 31 in multiple regression model

# 15 d

lm.zn = lm(crim~poly(zn,3))
summary(lm.zn) # 1, 2

lm.indus = lm(crim~poly(indus,3))
summary(lm.indus) # 1, 2, 3

# lm.chas = lm(crim~poly(chas,3)) : qualitative predictor
lm.nox = lm(crim~poly(nox,3))
summary(lm.nox) # 1, 2, 3

lm.rm = lm(crim~poly(rm,3))
summary(lm.rm) # 1, 2

lm.age = lm(crim~poly(age,3))
summary(lm.age) # 1, 2, 3

lm.dis = lm(crim~poly(dis,3))
summary(lm.dis) # 1, 2, 3

lm.rad = lm(crim~poly(rad,3))
summary(lm.rad) # 1, 2

lm.tax = lm(crim~poly(tax,3))
summary(lm.tax) # 1, 2

lm.ptratio = lm(crim~poly(ptratio,3))
summary(lm.ptratio) # 1, 2, 3

lm.black = lm(crim~poly(black,3))
summary(lm.black) # 1

lm.lstat = lm(crim~poly(lstat,3))
summary(lm.lstat) # 1, 2

lm.medv = lm(crim~poly(medv,3))
summary(lm.medv) # 1, 2, 3

#the answer is yes for most, except for black and chas.

