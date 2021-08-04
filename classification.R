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
# use predict() to get probability of going up (Y) based on all other data (X) aka P(Y=1|X) (use response to specify this is what you want)
glm.probs = predict(glm.fits, type = 'response')
glm.probs[1:10]
# now convert predicted probabilities into "Up" or "Down" predictions
glm.pred = rep('Down', 1250) # creates vector of 1250 "Down" elements
glm.pred[glm.probs > .5] = 'Up' # changes to "Up" for all with a predicted probability > .5
table(glm.pred, Direction) # create confusion matrix to see how many correctly predicted
mean(glm.pred==Direction) # aka (# correctly predicted) / (total) = (507+145)/1250 = .5216
# Logistic regression only predicted correct direction 52.16% of the time on TRAINING data, so likely overly optimistic
# Now, hold out some data to get a realistic idea of how the model performs on unseen data
# adding a column that says if year is < 2005, this is what we will train on 
train = Year < 2005
summary(train)
length(train)
Smarket.2005 = Smarket[!train,] # puts all 2005 rows in a matrix (a submatrix)
Direction.2005 = Direction[!train]
# Now fit logistical regression to all those observations not in 2005
# subset=train only trains using data where train == True
glm.fits = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial, subset = train)
glm.probs = predict(glm.fits, Smarket.2005, type = 'response')
glm.pred = rep('Down', 252)
glm.pred[glm.probs > .5] = 'Up'
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005) # only 48% correct, worse than random guessing
mean(glm.pred != Direction.2005) # 52% error rate

# Logistic regression with nasa data to predict hazardous vs non-hazardous asteroids
setwd('/Users/cf/Desktop/Statistical-Learning-R')
asteroids.data = read.csv('asteroids_data.csv')
asteroids.data = Filter(is.numeric, asteroids.data)
# looking for non-corr variables 
corr = cor(asteroids.data)
pairs(asteroids.data[, 3:15])
asteroids.data$Hazardous[asteroids.data$Hazardous == 'True'] = 1
asteroids.data$Hazardous[asteroids.data$Hazardous == 'False'] = 0
typeof(asteroids.data$Hazardous)
hazardous = strtoi(asteroids.data$Hazardous)
paste('percent hazardous in data:', sum(hazardous) / length(hazardous))
str(asteroids.data)
asteroids.data$Hazardous <- as.numeric(asteroids.data$Hazardous)
asteroids.train = asteroids.data[1:3500,]
asteroids.test = asteroids.data[3501:4687,]

glm.fits = glm(Hazardous ~ Absolute.Magnitude + Est.Dia.in.M.max. + Relative.Velocity.km.per.sec + Orbit.Uncertainity + Jupiter.Tisserand.Invariant + Minimum.Orbit.Intersection + Eccentricity, data = asteroids.train, family = binomial)
summary(glm.fits)

glm.probs = predict(glm.fits, asteroids.test, type = 'response')
glm.pred = rep(0, 1187)
glm.pred[glm.probs > .5] = 1
mean(glm.pred == asteroids.test$Hazardous) # 96.6% correct, something's up..

# LDA on Smarket data
library(MASS)
attach(Smarket)
plot(Lag1, Lag2, col = Direction) # Doesn't look very separable
lda.fit = lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)

# drawing discrimination line
nd.1 = seq(from = min(Lag1), to = max(Lag1), length.out = 1250)
nd.2 = seq(from = min(Lag2), to = max(Lag2), length.out = 1250)
nd = expand.grid(x = nd.1, y = nd.2)
prd = as.numeric(predict(lda.fit, newdata = nd)$class)

plot(Lag1, Lag2, col = Direction)
points(lda.fit$means, pch = "+", cex = 3, col = c('black', 'red'))
contour(x = nd.1, y = nd.2, z = matrix(prd, nrow = 1250, ncol = 1250), 
        levels = c(1, 2), add = T, drawlabels = F) # issues, groups too close?

# back to LDA
lda.pred = predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class = lda.pred$class
table(lda.class, Direction.2005) # almost the same as log regression
mean(lda.class == Direction.2005) # about 56% correct

