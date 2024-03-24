
####################### Time series analysis using R ###########################


# Installing all the required packages for the R Notebook

#install.packages(stats)
library(stats)

############### Time series plot ###############
mydata <- read.csv("E15demand.csv")
E15 = ts(mydata$Demand, start = c(2012,4), end = c(2013,10), frequency = 12)
E15
plot(E15, type = "b")

E15 = ts(mydata$Demand)
E15
plot(E15, type = "b")

############# Trend in GDP data ###############
mydata <- read.csv("Trens_GDP.csv")
GDP <- ts(mydata$GDP, start = 1993, end = 2003)
plot(GDP, type = "b")

############# Seasonality in Sales data ###############
mydata <- read.csv("Seasonal_sales.csv")
sales = ts(mydata$Sales, start = c(2002,1), end = c(2005,12), frequency = 12)
plot(sales, type = "b")

############# Trend & Seasonality in TS data ###############
mydata <- read.csv("Trend_&_Seasonal.csv")
sales = ts(mydata$Sales)
plot(sales, type = "b")

################## Stationarity Test ########################
mydata <- read.csv("shipment.csv")
shipments = ts(mydata$Shipments)
plot(shipments, type = "b")

# ADF Test
install.packages("tseries")
library("tseries")
adf.test(shipments)

# KPSS Test
kpss.test(shipments)

############# Stationarity in GDP data ###############
mydata <- read.csv("Trens_GDP.csv")
GDP <- ts(mydata$GDP, start = 1993, end = 2003)
plot(GDP, type = "b")

kpss.test(GDP)

# Differencing

install.packages("forecast")
library(forecast) 
ndiffs(GDP)

mydiffdata = diff(GDP, difference = 1) 
plot(mydiffdata, type = "b")
adf.test(mydiffdata)
kpss.test(mydiffdata)

################### Simple Exponential Smoothing ##################
mydata <- read.csv("Amount.csv")
amount = ts(mydata$Amount) 
plot(amount, type ="b")

# ADF Test & KPSS Test
adf.test(amount)
kpss.test(amount)

#Fitting the model
mymodel = HoltWinters(amount, beta = FALSE, gamma = FALSE) 
mymodel

# Actual Vs Fitted plot
plot(mymodel)

# Computing predicted  values and residuals (errors)
pred = fitted(mymodel)
res = residuals(mymodel)
outputdata = cbind(amount, pred[,1], res) 
write.csv(outputdata, "amount_outputdata.csv")

# Model diagnostics
abs_res = abs(res)
res_sq = res^2
pae = abs_res/ amount
mean(res) # Mean Residuals
mean(abs_res) # Mean Absolute Residuals
mean(res_sq) # Mean Residuals Square
sqrt(mean(res_sq)) # Root Mean Residuals Square
mean(pae)*100 # Mean absolute percentage error

#Normality Test of Errors with zero
qqnorm(res) 
qqline(res)
shapiro.test(res)
mean(res)

#Forecast
forecast = forecast(mymodel, 1) 
forecast
plot(forecast)

######################### Autocorrelation ###########################
mydata <- read.csv("Trens_GDP.csv")
GDP <- ts(mydata$GDP, start = 1993, end = 2003)
acf(GDP, 3)
acf(GDP)

######################## ARIMA Model #################################
mydata <- read.csv("Visits.csv")
mydata <- ts(mydata$Data)
plot(mydata, type = "b")

# Descriptive Statistics
summary(mydata)

# Check whether the series is stationary
library(tseries)
adf.test(mydata)
kpss.test(mydata)
ndiffs(mydata)

#Draw ACF & PACF Graphs
acf(mydata)
pacf(mydata)

# ARIMA Model Fitting
mymodel = auto.arima(mydata)
mymodel

# Identification of model manually
arima(mydata, c(0,0,1))
arima(mydata, c(1,0,0))
arima(mydata, c(1,0,1))

# Model diagnostics
summary(mymodel)
pred = fitted(mymodel)
res = residuals(mymodel)

#Normality check on Residuals
qqnorm(res) 
qqline(res) 
shapiro.test(res)
hist(res, col = "grey")

# Autocorrelation check on Residuals
acf(res)
Box.test(res, lag = 15, type = "Ljung-Box")

# Forecasting the future
forecast = forecast(mymodel, h = 3) 
forecast

############################ End of Session ####################################