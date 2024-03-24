################# Simple Linear Regression Analysis in RStudio #################

################ Fitting Linear Regression using lm function ###################

# Installing all the required packages for the R Notebook

install.packages("car")
install.packages("pls")
install.packages("ggplot2")
install.packages("gplots")
install.packages("glmnet")
install.packages("DAAG")
install.packages("boot")
install.packages("MASS")
library(DAAG)
library(glmnet)
library(car)
library(MASS)
library(gplots)
library(ggplot2)
library(pls)
library(boot)

#### Simple Linear Regression ####
#(dependent variable is regressed upon one predictor variable)

# Reading the data and variables

mydata = read.csv('DC_Simple_Reg.csv',header = T,sep = ",")
Temp = mydata$Dryer.Temperature 
DContent = mydata$Dry.Content

# Constructing Scatter Plot
plot(Temp, DContent)

# Computing Correlation Matrix
cor(Temp, DContent)
# Correlation between y & x need to be high (preferably 0.8 to 1 to -0.8 to -1.0)

# Fitting Regression Model
model = lm(DContent ~ Temp) 
summary(model)

# DContent = 2.183813 + 1.293432*Temp

# Regression Performance
anova(model)

# Residual Analysis
pred = fitted(model) 
Res = residuals(model) 

# write.csv(pred,"Pred.csv") # Stores the predicted values in 'Pred.csv' file at your working directory
# write.csv(Res,"Res.csv") # Stores the residual values in 'Res.csv' file at your working directory

# Visualizing Actual vs Predicted Values
plot(DContent, pred)

# Checking whether the distribution of the Residuals is bell shaped (Normality Test)
qqnorm(Res) 
qqline(Res)

# Normality Check using Shapiro-Wilk test
shapiro.test(Res) 

# Visualizing the relationship between Residuals and Predicted values
plot(pred,Res)

# Visualizing the relationship between Residuals and Predictor
plot(Temp,Res)

################################################################################

################# Fitting Simple Linear Regression Manually #################

# Reading the data and variables
df = data.frame(DContent = c(73.3,74.6,74,78.5,74.6,74,75.2,77.2,75.9,74.6,73.3,75.9,76,74.6,74.7,74.5,70.7,72,72.1,72.2,70.7,
74.6,75.2,74,75.9,75.3,74,78.5,76.5,74.5,76,76.5,76.7,76,75.8,73.8,73.3,74.6,73.4,76,74,75.2), 
Temp = c(55,56,55.5,59,56,55.5,56.5,58,57,56,55,57,57,56,56,56,53,54,54,54,53,56,56.5,55.5,57,56.5,55.5,59,
57.5,56,57,57.5,57.5,57,57,55.5,55,56,55,57,55.5,56.5))

# Constructing Scatter Plot
plot(Temp, DContent)

# Computing Correlation Matrix
cor(Temp, DContent)
# Correlation between y & x need to be high (preferably 0.8 to 1 to -0.8 to -1.0)

# Fitting Regression Model

# y_hat = beta_0_hat + beta_1_hat*x
# beta_1_hat = S_xy / S_xx
# beta_0_hat = y_bar - beta_1_hat*x_bar

be1_hat = cov(Temp,DContent) / var(Temp)
be0_hat = mean(DContent) - (be1_hat * mean(Temp))

pred = be0_hat + (be1_hat *Temp)

A = data.frame (DContent, Temp, pred)
names(A)=c("Observed DContent","Observed Temp ","Fitted DContent")
print(A)

cat("\n The fitted linear regression model is : DContent=",be0_hat,"+",be1_hat,"Temp\n")
# DContent = 2.183813 + 1.293432*Temp

# Regression Performance

Res = DContent - pred # Calculating the residuals

# Performing ANOVA

# SS_Res = Sum (Res^2) | MS_Res = SS__Res/df_Res
SS_Res =sum(Res^2)
df_Res = length(DContent)-2  # Number of observations - No of predictors - 1
MS_Res = SS_Res / df_Res

# SS_Total = Sum(y - y_bar)^2 | MS_Total = SS_Total/df_total
SS_Total = sum((DContent - mean(DContent))^2)
df_Total = length(DContent) - 1 # Number of observations - 1
MS_Total = SS_Total / df_Total

# SS_Reg = SS_Total - SS_Res | MS_Reg = SS_Reg/df_Reg
SS_Reg = SS_Total - SS_Res
df_Reg = 1 # No of predictors
MS_Reg = SS_Reg / df_Reg

# Calculating F statistic values
F_obs = MS_Reg / MS_Res

# Tabulated F vlaue 
F_pvalue = 1 - pf(F_obs, df_Reg, df_Res)

# ANOVA Table

cat("\n \t\t\t Analysis of Variance Table\t\t\t\t\n")+
  cat("\nSource","\t\t","Df","\t","Sum Sq","\t","Mean Sq","\t","F value","\t","Pr(>F)","\n")+
  cat("\n Temp","\t\t",df_Reg,"\t",SS_Reg,"\t",MS_Reg,"\t",F_obs,"\t",F_pvalue,"\n")+
  cat("\n Residual","\t",df_Res,"\t",SS_Res,"\t",MS_Res,"\n")+
  cat("\n Total","\t\t",df_Total,"\t",SS_Total,"\t",MS_Total,"\n")

# Visualizing Actual vs Predicted Values
plot(DContent, pred)

# Checking whether the distribution of the Residuals is bell shaped (Normality Test)
qqnorm(Res) 
qqline(Res)

# Normality Check using Shapiro-Wilk test
shapiro.test(Res) 

# Visualizing the relationship between Residuals and Predicted values
plot(pred,Res)

# Visualizing the relationship between Residuals and Predictor
plot(Temp,Res)

################################################################################

library(modelr)
dat <- read.csv("Advertising.csv", header = TRUE)

# visualize the data
par(mfrow=c(1,3))
plot(dat$TV, dat$sales, xlab = "TV", ylab = "Sales", col = "red")
plot(dat$radio, dat$sales, xlab="Radio", ylab="Sales", col="blue")
plot(dat$newspaper, dat$sales, xlab="Newspaper", 
     ylab="Sales", col="forestgreen")

library(tidyverse)

# Split the data into training and testing set
dat.split <- resample_partition(dat, c(test = 0.3, train = 0.7))

train <- as_tibble(dat.split$train)
test <- as_tibble(dat.split$test)

# Time for SLR model fitting
mod1 <- lm(sales ~ TV, data = train)

# Summary
summary(mod1)

# Tidy summary (available in 'broom' package)
library(broom)
tidy(mod1)

confint(mod1)

# Checking model accuracy

MSE <- mse(mod1, train)
MSE
## [1] 10.16292

rsquare(mod1, train)
## [1] 0.6067217

# In case of simple linear regression, R2 is exactly equal to the square of the 
# correlation between the response variable y and predictor variable X.

r = cor(train$sales, train$TV)
r^2
## [1] 0.6067217

# Visual assessment of model fit

ggplot(train, aes(x = TV, y = sales)) +
  geom_point() +
  geom_smooth(method = "lm") 

# Predictions

test %>% add_predictions(mod1)

pred.MSE <- mse(mod1, test)
c(Prediction_MSE = pred.MSE, Train_MSE = MSE)
## Prediction_MSE      Train_MSE 
##       11.59707       10.16292

# Confidence intervals and prediction intervals of fit
test.predict <- predict(mod1, test, interval = "prediction")

new_df <- cbind(test, test.predict)

ggplot(new_df, aes(x = TV, y = sales))+
  geom_point() +
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")+
  geom_smooth(method=lm, se=TRUE)

# Residual Analysis
# Equal variance assumptions
# add model diagnostics to our training data
mod1_results <- augment(mod1, train)

p1 <- ggplot(mod1_results, aes(.fitted, .resid)) +
  geom_ref_line(h = 0) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Residuals vs Fitted")

p2 <- ggplot(mod1_results, aes(.fitted, .std.resid)) +
  geom_ref_line(h = 0) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ggtitle("Standardized Residuals vs Fitted")

gridExtra::grid.arrange(p1, p2, nrow = 1)

par(mfrow = c(1,1))

# Normality check of errors
qqnorm(mod1_results$.resid)
qqline(mod1_results$.resid)

# Leverage, Influence, and Outliers

par(mfrow=c(1, 2))

plot(mod1, which = 4, id.n = 5)
plot(mod1, which = 5, id.n = 5)

######################### End of Session #########################################