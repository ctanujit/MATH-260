################ Multiple Linear Regression Analysis in RStudio ################

# Installing all the required packages for the R Notebook

# install.packages("car")
# install.packages("pls")
# install.packages("ggplot2")
# install.packages("gplots")
# install.packages("glmnet")
# install.packages("DAAG")
# install.packages("boot")
# install.packages("MASS")
library(broom)
library(DAAG)
library(glmnet)
library(car)
library(MASS)
library(gplots)
library(ggplot2)
library(pls)
library(boot)
library(dplyr)
library(tidyverse)

#### Multiple Linear Regression ####

# Revisiting the Advertising dataset
library(modelr)
dat <- read.csv("Advertising.csv")

# visualize the data
par(mfrow=c(1,3))
plot(dat$TV, dat$sales, xlab = "TV", ylab = "Sales", col = "red")
plot(dat$radio, dat$sales, xlab="Radio", ylab="Sales", col="blue")
plot(dat$radio, dat$newspaper, xlab="Newspaper", 
     ylab="Sales", col="forestgreen")

par(mfrow = c(1,1))
dat <- as_tibble(dat)
#dat <- as_tibble(dat[,-1]) #%>% select(-X) # X represents serial number hence can be removed

# Split the data into training and testing set
set.seed(8885)
dat.split <- resample_partition(dat, c(test = 0.3, train = 0.7))

train <- as_tibble(dat.split$train)
test <- as_tibble(dat.split$test)

# Previously we did SLR model fitting as follows
mod1 <- lm(sales ~ TV, data = train)

# Time for MLR model fitting
mod2 <- lm(sales ~ TV + radio + newspaper, data = train)

summary(mod2)

tidy(mod2)

# Getting 95% confidence intervals
confint(mod2)

glance(mod2, train)

# Comparing the two fitted models
list(`model-1` = glance(mod1,train), `model-2` = glance(mod2,train))

# Visual assessment of model fit
# add model diagnostics to our training data
mod1_results <- augment(mod1, train) %>%
  mutate(Model = "Model-1")

mod2_results <- augment(mod2, train) %>%
  mutate(Model = "Model-2") %>%
  rbind(mod1_results)

ggplot(mod2_results, aes(.fitted, .resid)) +
  geom_ref_line(h = 0) +
  geom_point() +
  geom_smooth(se = FALSE) +
  facet_wrap(~ Model) +
  ggtitle("Residuals vs Fitted")

# predictions
test %>% add_predictions(mod2)

MSE <- mse(mod2, train)
pred.MSE <- mse(mod2, test)
c(Prediction_MSE = pred.MSE, Train_MSE = MSE)

# Using base R approach (gives confidence and prediction interval of fit)
test.predict <- predict(mod2, test, interval = "prediction")
as_tibble(test.predict)

# interaction effect

mod3 <- lm(sales ~ TV + radio + TV:radio + newspaper, data = train)

#Aliter:
mod3 <- lm(sales ~ TV*radio + newspaper, data = train)

summary(mod3)

tidy(mod3)
######################## Multicollinearity ###################

X1 <- runif(100)
X2 <- rnorm(100,10,10)
X3 <- rnorm(100,-20,20)
X4 <- X2 - X3 # exact linear relationship

Y <- 1 + X1 - 0.5*X2 + 0.5*X3 + rnorm(100,0,4)

dat <- as_tibble(data.frame(Y = Y, X1 = X1, X2 = X2, X3 = X3, X4 = X4))
dat

cor(dat)

simu.model <- lm(Y ~ X1 + X2 + X3 + X4)
summary(simu.model)

#Let us re-run this example with near-linear relationship among some of the predictors.
library(tidyverse)
#### Near-linear dependency
set.seed(100)
X1 <- runif(100)
X2 <- rnorm(100,10,10)
X3 <- rnorm(100,-20,20)
X4 <- X2 - X3 + runif(100) # near-linear relationship

Y <- 1 + X1 - 0.5*X2 + 0.5*X3 + rnorm(100,0,4)

dat <- as_tibble(data.frame(Y = Y, X1 = X1, X2 = X2, X3 = X3, X4 = X4))
dat

cor(dat)

simu.model <- lm(Y ~ X1 + X2 + X3 + X4)
summary(simu.model)

library(car)
vif(simu.model)


#dependent variable is regressed upon two or more predictor variables

# Reading the data and variables
data = read.csv('Mult_Reg_Yield.csv',header = T,sep = ",")
mydata= data[,-1] # Removing SL.NO. Column 
attach(mydata)
# Computing Correlation Matrix
cor(mydata)

# Fitting Multiple Linear Regression
model = lm(X.Yield ~ Temperature + Time) 
summary(model)

# X.Yield = -67.88436 - 0.06419*Temperature + 0.90609*Time

# Temperature is NOT a causal variable. 

# Regression Model Performance
anova(model)

# From the ANOVA Table we can say only time is related to % yield as p value < 0.05, so we modify our model
# Fitting Linear Regression Model with Time as the only Predictor variable

model_m = lm(X.Yield ~  Time) 
summary(model_m)

# X.Yield = -81.6205 + 0.9065*Time 

# Regression Model Performance
anova(model_m)

# Residual Analysis
pred = fitted(model_m) 
Res = residuals(model_m) 
plot(Res)
qqnorm(Res)

#Standardizing the Residuals using scale function
#"center" parameter (when set to TRUE) is responsible for subtracting the mean on the numeric object from each observation.
#The "scale" parameter (when set to TRUE) is responsible for dividing the resulting difference by the standard deviation of the numeric object.
Std_Res = scale(Res, center = TRUE, scale = TRUE) 

# Normality Check using Shapiro - Wilk test
shapiro.test(Res) 

# Bonferonni Outlier Test
outlierTest(model_m) # Bonferroni p-value < 0.05 indicates potential outlier

# Leave One Out Cross Validation 
mymodel = glm(X.Yield ~ Time)
valid = cv.glm(mydata, mymodel)
valid$delta[1]

# Multiple Linear Regression (dependent variable is regressed upon two or more predictor variables)

# Reading the data and variables
data = read.csv('Mult_Reg_Conversion.csv',header = T,sep = ",")
mydata = data[,-1] # Removing SL.NO. Column 
attach(mydata)

# Computing Correlation Matrix
cor(mydata)
# High Correlation between Percent_Conversion and Temperature & Time
# High Correlation between Temperature & Time - Multicollinearity

# Fitting Multiple Linear Regression
model = lm(Percent_Conversion ~ Kappa.number + Temperature + Time) 
summary(model)

# Regression ANOVA
anova(model)

# Checking Multi-collinearity using Variance Inflation Factor 
#library(car)
vif(model)

# VIF > 5 indicates multi-collinearity. Hence, multi-collinearity exists between Time and Temperature

# Tackling Multi-collinearity

# Method 1: Removing highly correlated variable - Stepwise Regression

#library(MASS) 
mymodel = lm(Percent_Conversion ~ Temperature + Time + Kappa.number) 
step = stepAIC(mymodel, direction = "both")
summary(step)

# Check for multicollinearity in the new model
vif(step)
# vif values <5 indicates no multicollinearity

# Predicting the values
pred = predict(step) 
res = residuals(step)
cbind(Percent_Conversion, pred, res)
mse = mean(res^2)
rmse = sqrt(mse)

# K Fold Validation

#install.packages("DAAG")
#library(DAAG) 

cv.lm(data = mydata, form.lm = formula(Percent_Conversion ~ Kappa.number + Temperature + Time), m=10, dots = 
        FALSE, seed=29, plotit=TRUE, printit=TRUE)
###################################################################################
# Method 2: Principal Component Regression

#install.packages("pls")
#library(pls) 
mymodel = pcr(Percent_Conversion ~ ., data = mydata, scale = TRUE)
summary(mymodel)
mymodel$loadings
mymodel$scores

pred = predict(mymodel, type = "response", ncomp = 1)
res = Percent_Conversion - pred 
mse = mean(res^2)
prednew = predict(mymodel, type = "response", ncomp = 2)
resnew = Percent_Conversion - prednew 
msenew = mean(resnew^2)

# Since there is not much reduction in MSE by including the second principal component , only PC1 is required for modelling

# Method 3: Partial Least Square  Regression

mymodel = plsr(Percent_Conversion ~ ., data = mydata, scale = TRUE)
summary(mymodel)
ps = mymodel$scores 
score = ps[,1:2]

#Identifying the required number of components in the model
pred = predict(mymodel, data = mydata, scale = TRUE, ncomp = 1)
res = Percent_Conversion - pred 
mse = mean(res^2)

prednew = predict(mymodel, data = mydata, scale = TRUE ,  ncomp = 2)
resnew = Percent_Conversion - prednew 
msenew = mean(resnew^2)
# Not much reduction in MSE by including the second component , only PLS1 is required for modelling

# Method 4: Ridge regression 

#library(glmnet)
set.seed(1)
y = mydata[,4]
x = mydata[,1:3]
x = as.matrix(x)
mymodel = cv.glmnet(x , y, alpha =0)
plot(mymodel)

# Choose the lambda which minimizes the mean square error
bestlambda = mymodel$lambda.min
bestlambda

# Develop the model with best lambda and identify the coefficients
mynewmodel = glmnet(x, y, alpha = 0) 
predict(mynewmodel, type = "coefficients", s = bestlambda)[1:4,]

########## Ridge and lasso implementation in R ########## 
# We shall be working with the Ames Housing data, available in the AmesHousing package.

library(tidyverse)
library(modelr)
library(glmnet)
library(AmesHousing)
library(leaps)
library(broom)

# Create training and test data for Ames Housing data

dat <- make_ames()
set.seed(8885)

ames_split <- resample_partition(dat, c(test = 0.3, train = 0.7))

ames_train <- as_tibble(ames_split$train)
ames_test  <- as_tibble(ames_split$test)

# Create training and testing feature model matrices and response vectors.
# use model.matrix(...)[, -1] to discard the intercept

ames_train_x <- model.matrix(Sale_Price ~ ., ames_train)[, -1]
ames_train_y <- log(ames_train$Sale_Price)

ames_test_x <- model.matrix(Sale_Price ~ ., ames_test)[, -1]
ames_test_y <- log(ames_test$Sale_Price)

# Check dimensions
dim(ames_train_x)
## [1] 2052  308

dim(ames_test_x)
## [1] 878 308

# Apply Ridge regression to ames data
ames_ridge <- glmnet(x = ames_train_x, y = ames_train_y, alpha = 0)

plot(ames_ridge, xvar = "lambda")

# lambdas applied to penalty parameter
ames_ridge$lambda %>% head()

# coefficients for the largest and smallest lambda parameters
coef(ames_ridge)[c("Gr_Liv_Area", "TotRms_AbvGrd"), 100]

coef(ames_ridge)[c("Gr_Liv_Area", "TotRms_AbvGrd"), 1] 

# Tuning the penalty parameter λ.

# Apply CV Ridge regression to ames data
ames_ridge <- cv.glmnet(x = ames_train_x, y = ames_train_y, alpha = 0)

# plot results
plot(ames_ridge)

min(ames_ridge$cvm)       # minimum MSE
## [1] 0.02284662

ames_ridge$lambda.min     # lambda for this min MSE
## [1] 0.2450304

ames_ridge$cvm[ames_ridge$lambda == ames_ridge$lambda.1se]  # 1 st.error of min MSE
## [1] 0.02545676

ames_ridge$lambda.1se  # lambda for this MSE
## [1] 0.7482874

# Prediction performance of the fitted ridge regression model.
ridge.pred <- predict(ames_ridge, s = ames_ridge$lambda.min, newx = ames_test_x)
mean((ridge.pred - ames_test_y)^2)
## [1] 0.01787904

##############  LASSO Regression ####################
# We now apply the ridge regression method to the Ames Housing data.
# Apply lasso regression to ames data
ames_lasso <- glmnet(x = ames_train_x, y = ames_train_y, alpha = 1)

plot(ames_lasso, xvar = "lambda")

# Tuning the penalty parameter λ.
# Apply CV lasso regression to ames data
ames_lasso <- cv.glmnet(x = ames_train_x, y = ames_train_y, alpha = 1)

# plot results
plot(ames_lasso)

min(ames_lasso$cvm)       # minimum MSE
## [1] 0.02828329

ames_lasso$lambda.min     # lambda for this min MSE
## [1] 0.003315378

ames_lasso$cvm[ames_lasso$lambda == ames_lasso$lambda.1se]  # 1st.error of min MSE
## [1] 0.03308106

ames_lasso$lambda.1se  # lambda for this MSE
## [1] 0.03091933

#Prediction performance of the fitted lasso regression model.
lasso.pred <- predict(ames_lasso, s = ames_lasso$lambda.min, newx = ames_test_x)
mean((lasso.pred - ames_test_y)^2)
## [1] 0.02093244

lasso.pred2 <- predict(ames_lasso, s = ames_lasso$lambda.1se, newx = ames_test_x)
mean((lasso.pred2 - ames_test_y)^2)
## [1] 0.02726125

# Advantage of choosing λ with MSE within 1 standard error
ames_lasso_min <- glmnet(
  x = ames_train_x,
  y = ames_train_y,
  alpha = 1
)

plot(ames_lasso_min, xvar = "lambda")
abline(v = log(ames_lasso$lambda.min), col = "red", lty = "dashed")
abline(v = log(ames_lasso$lambda.1se), col = "red", lty = "dashed")

######## Linear Regression with Dummy Variables #########

mydata =  read.csv('Travel_dummy_Reg.csv',header = T,sep = ",")
attach(mydata)
mydata = mydata[,2:4]

# Converting categorical x's to factors
gender = factor(Gender)
income = factor(Income)
# Fitting the model
mymodel =  lm(Attitude ~ gender + income)
summary(mymodel)
# Regression Model Performance
anova(mymodel)

################################################################################

################## Fitting Multiple Linear Regression manually #################

# Read the data
df1<-data.frame(
  x0=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
  x1=c(410,368,357,373,361,385,380,400,407,410,421,446,478,441,454,440,427),
  x2=c(9,11,15,12,9,9,10,12,12,13,12,19,19,18,12,12,12)
)
df1

df2 = data.frame(
  y=c(776,580,539,648,538,891,673,783,571,627,727,867,1042,804,832,764,727)
)
df2

Mat1 = data.matrix (df1)
Mat1
Maty = data.matrix (df2)
Maty

####### Transpose of Matrix Mat1 #########
TMat1 = t(Mat1)
TMat1

############ Matrix multiplication###############
Mat2 = TMat1 %*% Mat1
Mat2

############ Inverse Matrix ############
Mat3 = solve (Mat2)
Mat3
beta = Mat3 %*% TMat1 %*%Maty
beta

haty = Mat1 %*% beta
haty

############# SSResid ################
Residsquare = (Maty - haty)^2
Residsquare
SSResid = sum (Residsquare)
SSResid

### SSResid with Matrix ###
TMaty = t(Maty)
TMaty
Tbeta = t(beta)
Tbeta
SSResid = (TMaty %*% Maty) - (Tbeta %*% TMat1 %*% Maty)
SSResid

##################SSTOTAL#########################
Matyy = Maty^2
Matyy
bary = mean(Maty)
bary
SSTot = sum(Matyy)-17*(bary^2)
SSTot

################ R^2 Calculation ##############
R2 = 1-(SSResid/SSTot)
R2

#########################Sigma Hat #######################
sigmahat2 = SSResid/(17-3)
sigmahat2

#####################Confidence interval for Sigma Square###############
q1 = qchisq (0.025, df = 14)
q2 = qchisq (0.975, df = 14)
CI = c(SSResid/q2 , SSResid/q1)
CI

##################### SSRegression ############
SSReg = SSTot - SSResid
SSReg

##### SSRegression with Matrix ####
SSReg = (Tbeta %*% TMat1 %*% Maty) - (17*(bary^2))
SSReg

############################ F value#########################
MSReg = SSReg/ (3-1)
MSResid = SSResid/ (17-3)
F = MSReg/MSResid
F
qf(p=.025, df1=2, df2=14, lower.tail=FALSE)
################################################################################




