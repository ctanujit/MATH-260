################ Non linear Regression in RStudio #################

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
library(car)
crPlots(mymodel)

# Design polynomial model-1
newmodel1 = lm(y ~ poly(x1, 2, raw = TRUE) + x2 + x3, data = mydata) 
summary(newmodel1)
crPlots(newmodel1)

# Design polynomial model-2
newmodel2 = lm(y ~ poly(x1, 3, raw = TRUE) + x2 + x3, data = mydata) 
summary(newmodel2)
crPlots(newmodel2)

# Design Final Polynomial model
finalmodel = lm(y ~ poly(x1, 3, raw = TRUE) + 
                  poly(x2, 2, raw = TRUE) + 
                  sqrt(x3), data = mydata) 
crPlots(finalmodel)
summary(finalmodel)

finalmodel = lm(y ~ poly(x1, 3, raw = TRUE) + 
                  poly(x2, 3, raw = TRUE) + 
                  poly(x3, 3, raw = TRUE), data = mydata) 
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


##################################
design44 = design - 0.44
design44[design44 < 0] = 0
mymodel = lm(coding ~ design + design44)
summary(mymodel)
pred = predict(mymodel)
plot(design, coding)
lines(design, pred, col = "blue")

design44cb = design44^3
mymodel = lm(coding ~ poly(design, 3, raw = TRUE) + design44cb)
summary(mymodel)
pred = predict(mymodel)
plot(design, coding)
lines(design, pred, col = "blue")


################################# END OF SESSION ###############################