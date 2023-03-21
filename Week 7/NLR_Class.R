################ Logistic and Non linear Regression in RStudio #################

# Installing all the required packages for the R Notebook

install.packages("car")
install.packages("pls")
install.packages("ggplot2")
install.packages("gplots")
install.packages("boot")
install.packages ("MASS")
install.packages("glmnet")
install.packages("DAAG")
library(DAAG)
library(glmnet)
library(car)
library(MASS)
library(boot)
library(gplots)
library(ggplot2)
library(pls)

#################### Nonlinear Regression ####################

# Read the data file and variables
mydata = read.csv('Nonlinear_Thrust.csv', header = T, sep = ",")
mydata
cor(mydata)
plot(mydata$x1,mydata$y) 
plot(mydata$x2,mydata$y) 
plot(mydata$x3,mydata$y) 
mymodel = lm(y ~ x1 + x2 + x3, data = mydata) 
summary(mymodel)

# Install car package 
#install.packages("car")
#library(car)
crPlots(mymodel)

# Design polynomial model-1
newmodel1 = lm(y ~ poly(x1, 2, raw = TRUE) + x2 + x3, data = mydata) 
crPlots(newmodel1)

# Design polynomial model-2
newmodel2 = lm(y ~ poly(x1, 3, raw = TRUE) + x2 + x3, data = mydata) 
crPlots(newmodel2)

# Design Final Polynomial model
finalmodel = lm(y ~ poly(x1, 3, raw = TRUE) + poly(x2, 2, raw = TRUE) + sqrt(x3), data = mydata) 
crPlots(finalmodel)
summary(finalmodel)
res = residuals(finalmodel) 
qqnorm(res)
qqline(res) 
shapiro.test(res)

##################### Regression Spline #####################

# Read the data file and variables
mydata = read.csv('Reg_Spline_DFR.csv', header = T, sep = ",")
mydata
design = mydata$Design
coding = mydata$Coding
plot(design, coding)
mymodel = lm(coding ~ design)
summary(mymodel)
pred = predict(mymodel)
plot(design, coding)
lines( design, pred, col = "blue")
design44 = design - 0.44
design44[design44 < 0] = 0
mymodel = lm(coding ~ design + design44)
summary(mymodel)
pred = predict(mymodel)
plot(design, coding)
lines(design, pred, col = "blue")
designsq = design^2
designcb = design^3
design44cb = design44^3
mymodel = lm(coding ~ poly(design, 3, raw = TRUE) + design44cb)
summary(mymodel)
pred = predict(mymodel)
plot(design, coding)
lines(design, pred, col = "blue")

####################### Logistic Regression with 'default' data in ISLR #####################


library(ISLR2)

Default %>% head()

Default %>%
  mutate(prob = ifelse(default == "Yes", 1, 0)) %>%
  ggplot(aes(balance, prob)) +
  geom_point(alpha = .25) +
  geom_smooth(method = "lm") +
  ggtitle("Linear regression model fit") +
  xlab("Balance") +
  ylab("Probability of Default")

#Fitting a simple logistic regression model.

library(modelr)

set.seed(100)

# Split into training and testing data
default_split <- resample_partition(Default, c(test = 0.3, train = 0.7))

default_train <- as_tibble(default_split$train)
default_test  <- as_tibble(default_split$test)

# Fitting a simple logistic regression model
model1 <- glm(default ~ balance, family = binomial, data = default_train)

summary(model1)

# Quick check whether "default = Yes" is coded as "1" by R
contrasts(default_train$default)

# Interpretation of the model coefficients

exp(coef(model1))
##  (Intercept)      balance 
## 1.868042e-05 1.005699e+00

# Let us visualize the model fit.

Default %>%
  mutate(prob = ifelse(default == "Yes", 1, 0)) %>%
  ggplot(aes(balance, prob)) +
  geom_point(alpha = .25) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic regression model fit") +
  xlab("Balance") +
  ylab("Probability of Default")

# Predictions

test.predictions.m1 <- predict(model1, newdata = default_test, type = "response")
test.predictions.m1 <- tibble(balance = default_test$balance,
                              true.default.status = default_test$default,
                              pred.default = test.predictions.m1)
test.predictions.m1

test.predictions.m1 %>%
  filter(true.default.status == "Yes")

############# Multiple Logistic Regression

# Let us now demonstrate the procedure with the Default dataset taking into account all the available predictors, namely, student, balance and income.

model2 <- glm(default ~ student + balance + income, family = binomial, data = default_train)

summary(model2)

# We can do predictions similarly.
test.predictions.m2 <- predict(model2, newdata = default_test, type = "response")
test.predictions.m2 <- tibble(default_test,
                              pred.default = test.predictions.m2)
test.predictions.m2

## Take a look at the predicted default probabilities who actually defaulted

test.predictions.m2 %>%
  filter(default == "Yes")

#Model validation via Classification performance assessment

# First model: only predictor is balance

glm.pred.m1 <- rep("No", dim(default_test)[1])
glm.pred.m1[test.predictions.m1$pred.default > 0.5] <- "Yes"

table(glm.pred.m1, default_test$default)

# Second model: all predictors included

glm.pred.m2 <- rep("No", dim(default_test)[1])
glm.pred.m2[test.predictions.m2$pred.default > 0.5] <- "Yes"

table(glm.pred.m2, default_test$default)

# ROC Curve and AUC

library(ROCR)

test.predictions.m1 <- predict(model1, newdata = default_test, type = "response")
prediction(test.predictions.m1, default_test$default) %>%
  performance(measure = "tpr", x.measure = "fpr") %>%
  plot() %>%
  abline(a=0,b=1, lty = "dashed")

# AUC
prediction(test.predictions.m1, default_test$default) %>%
  performance(measure = "auc") %>%
  .@y.values
## [[1]]
## [1] 0.9409003

########### Binary Logistic Regression with visit data ###########
# Response variable is Categorical

#Reading the file and variables
mydata =  read.csv('Resort_Visit.csv',header = T,sep = ",")
# either attach the data or use data$. to extract columns
visit = mydata$Resort_Visit
income = mydata$Family_Income
attitude = mydata$Attitude.Towards.Travel
importance = mydata$Importance_Vacation
size = mydata$House_Size
age = mydata$Age._Head

#Converting response variable to discrete
visit = factor(visit)

# Computing Correlation Matrix
cor(mydata)
# Correlation between X variables should be low otherwise it will indicate Multicollinearity

# Checking relation between Xs and Y
aggregate(income ~visit, FUN = mean)
aggregate(attitude ~visit, FUN = mean)
aggregate(importance ~visit, FUN = mean)
aggregate(size ~visit, FUN = mean)
aggregate(age ~visit, FUN = mean)
# Higher the difference in means, stronger will be the relation to response variable

# Checking relation between Xs and Y - box plot
boxplot(income ~ visit)
boxplot(attitude ~ visit) 
boxplot(importance ~ visit) 
boxplot(size ~ visit)
boxplot(age ~ visit)

# Fitting Logistic Regression Model
model = glm(visit ~ income + attitude + importance + size + age, family = binomial(logit))
summary(model)

# Perform Logistic regression - ANOVA
anova(model,test = 'Chisq')
# Since p value < 0.05 for Income redo the modeling with important factors only

# Modifying the Logistic Regression Model
model_m = glm(visit ~ income, family = binomial(logit))
summary(model_m)

# Perform Logistic regression - Anova
anova(model_m,test = 'Chisq')

cdplot(visit ~ attitude)

# Fitted Value & Residual
predict(model_m,type = 'response')
residuals(model_m,type = 'deviance')
predclass = ifelse(predict(model_m, type ='response')>0.5,"1","0")
predclass

# Model Evaluation
mytable = table(visit, predclass)
mytable
prop.table(mytable)


prop.table(mytable)

######### Ordinal Logistic Regression ###########

# Read the data file and variables
mydata = read.csv('ST_Defects.csv', header = T, sep = ",")
dd = mydata$DD
effort = mydata$Effort 
coverage = mydata$Test.Coverage
dd = factor(dd)
dd

# Install MASS Package

#install.packages("MASS")
#library(MASS) 

# Fitting the model
mymodel = polr(dd ~ effort + coverage)
summary(mymodel)

# Predicting Values
pred = predict(mymodel) 
pred
fit = fitted(mymodel)
fit
output = cbind(dd, pred)
output

# Comparing Actual Vs Predicted
mytable = table(dd, pred)
mytable


################################# Exercise Solution of Ridge regression and Lasso #####################################


# Apply Ridge Regression and Lasso approach to the Hitters data from ISLR2 package.
# Aim is to predict a basketball player's Salary on the basis of various features

install.packages("ISLR2")
library (ISLR2)

# Loading the data
View(Hitters)
names(Hitters)
dim(Hitters)

# Removing missing values 
sum(is.na(Hitters$Salary)) # Salary is missing for 59 players
Hitters <- na.omit(Hitters) # remove all rows that have missing value in any variable
dim(Hitters)
sum(is.na(Hitters)) # no missing data

# Use glmnet package for Ridge Regression and Lasso. The alpha argument determines the type of model to be fitted.
install.packages("glmnet")
library(glmnet)

# Transform the dependent variable into a matrix, and convert all qualitative variable (if any) into dummy variables using model.matrix() function
x <- model.matrix(Salary ~ . , Hitters)[,-1]
print(x)
y <- Hitters$Salary

########################### Ridge Regression ###########################

# alpha argument of glmnet() function is set to 0 for ridge regression
# grid <- 10^seq(10, -2, length = 100) # value of lambda
# ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)
# 
# dim(coef(ridge.mod)) # ridge coefficients associated with each value of lambda, arranged in a matrix form
# 
# # coefficient estimates in terms of l2 norm are much  smaller for a larger value  of lambda, compared to a smaller value of lambda
# ridge.mod$lambda[50] # lambda = 11498
# coef(ridge.mod)[,50]
# sqrt(sum(coef(ridge.mod)[-1,50]^2)) # l2 norm = 6.36
# 
# ridge.mod$lambda[60] # lambda = 705
# coef(ridge.mod)[,60]
# sqrt(sum(coef(ridge.mod)[-1,60]^2)) # l2 norm = 57.1
# 
# # obtain the ridge coefficients for a new value of lambda = 50
# predict(ridge.mod, s = 50, type = "coefficients")[1:20,]

# we will split the sample into train and test set to obtain the test error
# set.seed(1)
# train <- sample(1:nrow(x) , nrow(x) / 2)
# test <- (-train)
# y.test <- y[test]
# 
# # fit ridge regression model on training data
# ridge.mod <- glmnet(x[train, ], y[train], alpha = 0, lambda = grid, thresh = 1e-12)
# 
# # use the fitted model to get predictions for a test set, using lambda = 4
# ridge.pred <- predict(ridge.mod, s = 4, newx = x[test, ]) # we can use the predict function for various purposes by modifying the argument
# mean((ridge.pred - y.test)^2) # computing test MSE
# 
# # Compare a ridge regression model with lambda = 4 with a model fitting only an intercept
# mean((mean(y[train]) - y.test)^2) # MSE for a model with just an intercept
# 
# # We would get the same result by fitting a ridge regression model with a very large value of lambda
# ridge.pred <- predict(ridge.mod , s = 1e10, newx = x[test, ]) # 1e10 means 10^10
# mean((ridge.pred - y.test)^2)
# # Ridge regression with lambda = 4 leads to a much lower test MSE than fitting a model with just an intercept
# 
# # Compare a ridge regression model with lambda = 4 with a least square regression model
# ridge.pred <- predict(ridge.mod, s = 0, newx = x[test, ], exact = T, x = x[train, ], y = y[train]) # least square regression is equivalent to ridge regression with lambda = 0 
# mean((ridge.pred - y.test)^2)
#  
# lm(y~x, subset = train)
# predict(ridge.mod, s = 0, exact = T, type = "coefficients", x = x[train, ], y = y[train])[1:20, ]

# use cross-validation method to choose the value of lambda
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 0)
bestlam <- cv.out$lambda.min
bestlam

ridge.pred <- predict(ridge.mod, s =  bestlam, newx = x[test, ]) # the best lambda to obtain predictions for a test set
mean((ridge.pred - y.test)^2) # test MSE for best lambda

# This model is a further improvement over the model with lambda = 4 in terms of the test MSE 
# Refit the ridge regression model on full dataset using the the value of lambda obtained by cross-validation approach
out <- glmnet(x, y, alpha = 0)
predict (out, type = "coefficients",  s = bestlam)[1:20, ]

############################ LASSO ###########################

# alpha argument of glmnet() function is set to 1 for lasso
lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda = grid)
plot(lasso.mod)

# perform cross-validation and compute the test error
set.seed(1)
cv.out <- cv.glmnet(x[train, ], y[train], alpha = 1)
bestlam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod, s = bestlam, newx = x[test, ])
mean((lasso.pred - y.test)^2) # MSE is substantially lower than the test MSE of null model and least square regrssion model; it is very similar to the test MSE of ridge regression with lambda chosen by cross validation

# lasso has an advantage over ridge regression in that the resulting coefficient estimates are sparse.
out <- glmnet(x, y, alpha = 1, lambda = grid)
lasso.coef <- predict(out, type = "coefficients", s = bestlam)[1:20, ]
lasso.coef # 8 out of 19 coefficients are exactly 0
lasso.coef[lasso.coef != 0] # model contains only 11 variables


##########################################################
# EXPLORATORY NON-PARAMETRIC REGRESSION IN R WITH GGPLOT #
##########################################################

require(smooth)
install.packages("ggpubr")
library(ggpubr)
library(ggplot2)

############## Creating the data ######################
set.seed(999)
n <- 120
t <- seq(0,3.5*pi/2,length=n)
class <- as.factor(sample(c("A","B","C"),n/3,replace = TRUE))
data <- data.frame(Predictor=t,Output=sin(t)-.5*sin(as.numeric(class)*t)+rnorm(n,0,.35),class=class)

#################################################################
# ggplot do all this stuff for us with the function "geom_smooth"
#################################################################

# Linear regression
gg.lm <- ggplot(data,aes(x=Predictor,y=Output,col=class))+geom_point()+
  geom_smooth(method='lm') + facet_grid(class ~.)
print(gg.lm)

# Quadratic regression
gg.qua <- ggplot(data,aes(x=Predictor,y=Output,col=class))+geom_point()+
  geom_smooth(method='lm',formula= y ~ poly(x,2)) + facet_grid(class ~.)
print(gg.qua)

# Cubic regression
gg.cub <- ggplot(data,aes(x=Predictor,y=Output,col=class))+geom_point()+
  geom_smooth(method='lm',formula= y ~ poly(x,3)) + facet_grid(class ~.)
print(gg.cub)

# Natural splines
gg.ns2 <- ggplot(data,aes(x=Predictor,y=Output,col=class))+geom_point()+
  geom_smooth(method='gam',formula=y ~ splines::ns(x, 2)) + facet_grid(class ~.)
print(gg.ns2)

gg.ns3 <- ggplot(data,aes(x=Predictor,y=Output,col=class))+geom_point()+
  geom_smooth(method='gam',formula=y ~ splines::ns(x, 3)) + facet_grid(class ~.)
print(gg.ns3)

gg.bs3 <- ggplot(data,aes(x=Predictor,y=Output,col=class))+geom_point()+
  geom_smooth(method='gam',formula=y ~ splines::bs(x, 3)) + facet_grid(class ~.)
print(gg.bs3)

gg.ns30 <- ggplot(data,aes(x=Predictor,y=Output,col=class))+geom_point()+
  geom_smooth(method='gam',formula=y ~ splines::ns(x, 30)) + facet_grid(class ~.)
print(gg.ns30)

gg.bs30 <- ggplot(data,aes(x=Predictor,y=Output,col=class))+geom_point()+
  geom_smooth(method='gam',formula=y ~ splines::bs(x, 30)) + facet_grid(class ~.)
print(gg.bs30)

################################# END OF SESSION ###############################