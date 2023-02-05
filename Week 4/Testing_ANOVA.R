####################### One sample t-test #################################

set.seed(8885)
Xsamp <- rnorm(n = 25, mean = 2, sd = 2)

mytest <- t.test(Xsamp, alternative = "greater", mu = 3)
mytest

mytest$p.value

# Another alternative hypothesis
t.test(Xsamp, alternative = "greater", mu = 2.1)

# Another alternative hypothesis
t.test(Xsamp, alternative = "greater", mu = 1.9)

# Another alternative hypothesis
t.test(Xsamp, alternative = "greater", mu = 1)
# p-value as a function of μ0 and n

func.p.val <- function(n, mu0){
  set.seed(100)
  Xsamp <- rnorm(n, mean = 2, sd = 2) #generating random sample of size n from N(2,4)
  p.val <- t.test(Xsamp, alternative = "greater", mu = mu0)$p.value #calculate p-value
  return(p.val)
}
n <- seq(5,25, by = 5)
mu0 <- seq(-5,5, by = 0.1)

pvals <- matrix(0, nrow = length(n), ncol = length(mu0))

for(i in 1:length(n)){
  for(j in 1:length(mu0)){
    pvals[i,j] <- func.p.val(n[i],mu0[j])
  }
}

# Plotting
cl <- rainbow(length(n))
plot(mu0, pvals[1, ], type = "l", main = "Plots of p-values", xlab = "Null value",
     ylab = "p-value", ylim = c(min(pvals),1), col = cl[1])

for(i in 2:length(n)){
  lines(mu0, pvals[i, ], type = "l", col = cl[i])
}
abline(h = 0.05)
abline(h = 0.01)
legend("topleft", legend = n, lty = 1, col = cl)

# Let us zoom it in near the true value of 2.

cl <- rainbow(length(n))

plot(mu0, pvals[1, ], type = "l", main = "Plots of p-values", xlab = "Null value",
     ylab = "p-value", ylim = c(0.009,0.052), col = cl[1])

for(i in 2:length(n)){
  lines(mu0, pvals[i, ], type = "l", col = cl[i])
}
abline(h = 0.05)
abline(h = 0.01)
abline(v = 2)
legend("topleft", legend = n, lty = 1, col = cl)

####################### Two Sample t test ############################

Xsamp <- rnorm(25, mean = 0, sd = 2)
Ysamp <- rnorm(20, mean = 1, sd = 1)

boxplot(Xsamp, Ysamp)

#Take null value of difference to be zero.

mytest2 <- t.test(Xsamp, Ysamp, alternative = "g", mu = 0)
mytest2

# In case you had to test whether μ2 is greater than μ1 by 0.5, 
# we can do it in two different ways as follows.

t.test(Xsamp, Ysamp, alternative = "l", mu = -0.5)

t.test(Ysamp, Xsamp, alternative = "g", mu = 0.5)

# Pooled t-test

Xsamp <- rnorm(20, mean = 2, sd = 1)
Ysamp <- rnorm(30, mean = 1, sd = 1)
boxplot(Xsamp, Ysamp)

mytest.pooled <- t.test(Xsamp, Ysamp, alternative = "t", mu = 0, var.equal = TRUE)
mytest.pooled

################### Paired t-test #########################

Xsamp <- rnorm(25, mean = 2, sd = 2)
Ysamp <- 0.5*Xsamp + rnorm(25, mean = 1, sd = 1)
# Notice that the Y samples are linearly related to X samples, 
# and hence a correlation between them are established. 
# You can plot the data to visualize this.

plot(Xsamp, Ysamp)

boxplot(Xsamp, Ysamp)

mytest.paired <- t.test(Xsamp, Ysamp, alternative = "t", mu = 0, paired = TRUE)
mytest.paired
# Note the usage of the argument paired = TRUE in the command above.

####################### Testing of variances ###############################

# We use the command var.test for this.

Xsamp <- rnorm(15, mean = 2, sd = 2)
Ysamp <- rnorm(20, mean = 5, sd = 4)

vartest <- var.test(Xsamp, Ysamp, alternative = "g")
vartest


########  Testing of Hypothesis PO Processing Time dataset ########

# Installing all the required packages for the R Notebook

install.packages("car")
install.packages("ggplot2")
install.packages("gplots")
install.packages("qqplotr")
library(car)
library(gplots)
library(qqplotr)
library(ggplot2)

######## One Sample t Test #######

# Problem: Testing whether the average Processing Time of PO_Processing 
# data set is less than equal to 40.

# Step 1: Reading the data as mydata

# SESSION -> SET WORKING DIRECTORY -> TO SOURCE FILE LOCATION

mydata = read.csv('PO_Processing.csv',header = T,sep = ",")

# IMPORT DATASET -> FROM TEXT -> CHOOSE DATA -> HEADING = YES, RENAME THE DATA

PT = mydata$Processing_Time

# Step 2: Using the t Test function to test our hypothesis

# ?t.test

t.test(PT, alternative = 'greater', mu = 40)

# p-value < 0.05 => Reject H0.

######## Normality Test ########

# Problem: Checking whether the Processing Time Data is Normally Distributed

qqnorm(PT)
qqline(PT)

# Normality Check using Shapiro-Wilk test

shapiro.test(PT) 

########################### ANOVA ######################################

data("InsectSprays")
dat <- InsectSprays
head(InsectSprays)

str(InsectSprays)
 
levels(dat$spray)

library(ggplot2)
ggplot(dat, aes(x=spray,y=count))+ 
  geom_point()

anova.fit <- aov(count ~ spray, data = dat)
summary(anova.fit)

summary.lm(anova.fit)

TukeyHSD(anova.fit)

plot(TukeyHSD(anova.fit))

######## One Way ANOVA Sales Revenue Example ########

# Reading data and variables in R

mydata = read.csv('Sales_Revenue_Anova.csv',header = T,sep = ",")
location = mydata$Location 
revenue = mydata$Sales.Revenue

# Converting location to factor

location = factor(location)

# H0 : location-wise sales figures are equal. 

# Computing ANOVA table

fit = aov(revenue  ~  location)
summary(fit)
# H0 rejected. Sales figures are not equal. 
aggregate(revenue ~ location, FUN = mean)
boxplot(revenue ~ location)
plotmeans(revenue ~ location)

# Tukey's Honestly Significant Difference (HSD) Test

TukeyHSD(fit)

# Bartlett's test

bartlett.test(revenue, location, data = mydata) 
