# Chapter 3: Linear Regression #
################################
# LAB
# Loading in libraries
library(MASS)
library(ISLR)

boston.data = Boston
names(Boston)

# Simple Linear regression 
attach(boston.data) # adds all cols as vector variables 
lm.fit = lm(medv ~ lstat)
summary(lm.fit)
names(lm.fit)

# Get confidence interval
confint(lm.fit)

# Predict medv values given lstat values 
predict(lm.fit, data.frame(lstat= c(5, 10, 15)), interval = "confidence")

# Plotting
plot(lstat, medv)
abline(lm.fit)
abline(lm.fit, lwd = 3) #increases width of reg line by factor of 3
abline(lm.fit, lwd = 4, col = 'blue') # changes line color 
plot(lstat, medv, col = 'purple') # changes point color
plot(lstat, medv, col = 'purple', pch = 20) # changes point shape
plot(lstat, medv, col = 'purple', pch = '+')
plot(1:20, 1:20, pch = 1:20) # plots x = (1, 2..20) y = (1, 2... 20) each with different point shape

par(mfrow = c(2, 2)) # divides plotting region into 2x2 grid
plot(lm.fit) # gives 4 plots for model in grid
plot(predict(lm.fit), residuals(lm.fit)) # plots the residuals for each fitted value
plot(predict(lm.fit), rstudent(lm.fit)) # does same
plot(hatvalues(lm.fit)) # plots leverage values over the index value (num in df)
which.max(hatvalues(lm.fit)) # gets index of highest leverage value

# Multiple linear regression 
# lm(y ~ x1 + x2 + x3)
lm.fit = lm(medv ~ lstat + age)
summary(lm.fit)
# regression using all predictors
lm.fit = lm(medv ~ ., data = boston.data)
summary(lm.fit)
?summary.lm # gives info, including values we can get
summary(lm.fit)$r.sq # R^2
summary(lm.fit)$sigma # RSE: Residual Standard Error 

library(car)
vif(lm.fit) # computes variance inflation factors 
# VIF = (var of B_j when fitting full model / var when fitting only x_j)

# to exclude predictor
lm.fit1 = lm(medv ~ .-age, data = boston.data)
# or
lm.fit1 = update(lm.fit, ~.-age)
summary(lm.fit)

# Interaction Terms (i.e. x1 * x2 predictor)
summary(lm(medv ~ lstat * age)) # includes lstat, age, and lstat * age as predictors

# Non-linear transformations of predictors
lm.fit2 = lm(medv~lstat+I(lstat^2)) # Wrap in I() bc want to preserve the standard usage of ^, like eval()
summary(lm.fit2) # small p-value so ^2 term helped
anova(lm(medv ~ lstat), lm.fit2) # performs test where Ho: Models equal, Ha: more complex model better
par(mfrow = c(2,2))
plot(lm.fit2) # much less pattern in residual plot
lm.fit5 = lm(medv~poly(lstat, 5)) # gets model using all orders of lstat up to 5
summary(lm.fit5) # each is valuable bc of low p-value
summary(lm(medv ~ log(rm))) # can also transform with log

# Qualitative Predictors
carseats.data = Carseats
attach(Carseats)
names(Carseats)
# R generates dummy vars for qualitative predictors automatically
lm.fit = lm(Sales ~ .+Income:Advertising + Price:Age, data = carseats.data)
summary(lm.fit)
contrasts(ShelveLoc) # Returns coding used by R for dummy vars
# created ShelveLocGood and ShelveLocMedium variables

# Writing Functions
LoadLibraries = function(){
  library(MASS)
  library(ISLR)
  print('Loaded libs')
}
LoadLibraries()

############
# Practice #
############

# Iris data
iris.data = iris
attach(iris)
names(iris)
pairs(iris.data[, 1:5])

# predicting Petal.Width 
lm.fit = lm(Petal.Width ~., data = iris.data)
summary(lm.fit)

# Removing Sepal.Length 
lm.fit1 = lm(Petal.Width ~ .-Sepal.Length, data = iris.data)
summary(lm.fit1)

# Compare 
anova(lm.fit1, lm.fit) # More complex models need much better accuracy to justify complexity
# Complexity is justified here (But barely) as .05 > .0389.
?anova
contrasts(Species)

# residuals
RSE = summary(lm.fit)$sigma # Estimate of epsilon; average amount predictions will be off with even a perfect linear model.
RSE
R.sq = summary(lm.fit)$r.sq # prop. of variance explained by model
R.sq