# Chapter 4: Classification #
#############################
# LAB

# Load libraries
library(ISLR)
names(Smarket)
# % returns for S&P 200 over 1,250 days (2001 - 2005), 
# For each data have % returns for 5 prev. trading days (Lag1 - 5), Volume (in bil), 
# Today (% return on date), and Direction (market up or down).

# Explore data
dim(Smarket)
summary(Smarket)
pairs(Smarket)
cor(Smarket[-9])
attach(Smarket)
plot(Year, Volume)


# Logistic Regression to predict direction
# glm is like lm except we pass in 'family = binomial' to run logistic regression
glm.fits = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)
summary(glm.fits)
# Smallest p-val with Lag1, says if market has positive return yesterday, less likely to go up today.
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[,4] # Z scores
glm.probs = predict(glm.fits, type = 'response')