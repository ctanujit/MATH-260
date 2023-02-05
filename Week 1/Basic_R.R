
####################### Installation of R and RStudio #########################
# (editor comment symbol in R is # )

# Download R software from http://cran.r-project.org/bin/windows/base/
# Run the R set up (exe) file and follow instructions
# Double click on the R icon in the desktop and R window will open
# Download RStudio from http://www.rstudio.com/
# Run R studio set up file and follow instructions
# Click on R studio icon, R Studio IDE Studio will load
# Go to R-Script (Ctrl + Shift + N)
# Write 'Hello World'
# Save & Run (Ctrl + Enter)

################################################################################

'Hello World'

############ Simple Maths in R ####################
#
3+5
12+3/4-5+3*8
(12+3/4-5)+3*8
pi*2^3-sqrt(4)
factorial(4)
log(2,10)
log(2,base=10)
log10(2)
log(2)

x = 3+5
x
y = 12+3/4-5+3*8
y
z = (12+3/4-5)+3*8
z

######## R is case sensitive and no space should be between '<' & '-' ##########

A <- 6+8
a
A

########## Write numeric / text data ################

data1 = c(3, 6, 9, 12, 78, 34, 5, 7, 7) ## numerical data
data1
data1.text = c('Mon', "Tue",'Wed') ##text data
data1.text
data2.text = c(data1.text , 'Thu' , "Fri") ## SIngle or double quote works
data2.text

############ Scan command for Read Data Values ###############
### Double enter in Console to stop ###
data3 = scan(what = 'character')
mon
tue
wed 
thur 
data3
data3[2]
data3[2] = 'mon'
data3
data3[6]= 'sat'
data3
data3[2] = 'tue'
data3[5] = 'fri'
data3

############# Working directory ################

getwd() ### Session - Get Working Directory - To Source File Location
setwd("Path of working directory") ### Session - Set Working Directory - To Source File Location
dir()  ### working directory listing
ls()   ### Work space listing of objects
rm('object')  ### Remove an element "object", if exist
rm(list = ls(all = TRUE)) ### Cleaning


##### Importing Data and accessing it #####

data = read.csv('Logistic_Reg.csv',header = T,sep = ",")
data = read.table('Logistic_Reg.csv',header = T,sep = ",")
data
str(data)
data$Ind_Exp_Act_Score
data$Ind_Exp_Act_Score[1] = 5.2 ###This change has happened in work space only not in the file
data$Ind_Exp_Act_Score  ### Save the changes ###
write.table(data, file = 'Logistic_Reg_mod.csv',row.names = FALSE,sep = ",")
write.csv(data, file = 'Logistic_Reg_mod.csv',row.names = TRUE)

##### Vectors in R #####

x = c(1,2,3,4,56)
x
## [1]  1  2  3  4 56

# Creating double and integer vectors

dbl_var <- c(1, 2.5, 4.5)  # double-precision values
dbl_var
## [1] 1.0 2.5 4.5

int_var <- c(1L, 6L, 10L)  # Integer vectors
int_var
## [1]  1  6 10

# Checking for numeric type

typeof(dbl_var)
## [1] "double"

typeof(int_var)
## [1] "integer"

# Converting between types

# converts integers to double-precision values
as.double(int_var)     
## [1]  1  6 10

# identical to as.double()
as.numeric(int_var)    
## [1]  1  6 10

# converts doubles to integers
as.integer(dbl_var)         
## [1] 1 2 4

mean(x)
x[2]
x = c(3, 4, NA, 5)
mean(x)
mean(x, rm.NA = T)
x = c(3,4, NULL, 5)
mean(x)
length(x)

##### Long Vectors in AP #####

x = 1:20
x
y = seq(2,5,0.3)
y
length(y)

###############  Operation on Vectors  ##################
x = 1:5
x
mean(x)
x^2
x+1
2*x
exp(sqrt(x))
y = c(0,3,4,0)
x+y
y = c(0,3,4,0,9)
x+y

x1 <- c(1,4,5)
y1 <- c(2,3,6)
print(x1+y1)
print(x1 > y1)

################# R Logical Operators ##########################

# ! : Logical NOT 
# & : Element-wise logical AND
# && : Logical AND
# | : Element-wise logical OR
# || : Logical OR

# Operators & and | perform element-wise operation producing result having length of the longer operand.
# But && and || examines only the first element of the operands resulting into a single length logical vector.
# Zero is considered FALSE and non-zero numbers are taken as TRUE. An example run.

x2 <- c(TRUE,FALSE,TRUE)
y2 <- c(FALSE,TRUE,TRUE)
print(!x2)    # [1] FALSE  TRUE FALSE
print(x2&y2)  # [1] FALSE FALSE  TRUE
print(x2&&y2) # [1] FALSE
print(x2|y2)  # [1] TRUE TRUE TRUE
print(x2||y2) # [1] TRUE

############## Comparing numbers in R / Relational Operators in R ##############

x <- 6
y <- 20
# < : Less than
# > : Greater than
# <= : Less than or equal to
# >= : Greater than or equal to
# == : Equal to
# != : Not equal to

print(x<y)  # [1] TRUE
print(x>y)  # [1] FALSE
print(x<=y) # [1] TRUE
print(x>=y) # [1] FALSE
print(x==y) # [1] FALSE
print(x!=y) # [1] TRUE

x <- c(1, 4, 9, 12)
y <- c(4, 4, 9, 13)
x == y
## [1] FALSE  TRUE  TRUE FALSE

x <- c(1, 4, 9, 12)
y <- c(4, 4, 9, 13)
# How many pairwise equal values are in vectors x and y
sum(x == y)    
## [1] 2

# Where are the pairwise equal values located in vectors x and y
which(x == y)
## [1] 2 3

#### Generating Random numbers: Random sampling

# simple random sampling with replacement
x <- c(1:10)
sample(x, size = 5, replace = TRUE)
## [1] 1 1 8 4 3

# simple random sampling without replacement
x <- c(1:10)
sample(x, size = 5, replace = FALSE)
## [1]  4  2 10  3  9


## Generating Random numbers: Discrete and continuous distributions

# random sample of size 10 from a Bin(4, 0.8) distribution
rbinom(n = 10, size = 4, prob = 0.8)
##  [1] 2 4 4 3 4 4 3 1 3 3

# random sample of size 10 from a Poisson(2) distribution
rpois(n = 10, lambda = 2) 
##  [1] 3 3 5 3 0 3 2 1 2 0

# random sample of size 5 from a Uniform(2,4) distribution
runif(n = 5, min = 2, max = 4)
## [1] 2.147157 3.766859 3.293724 3.071439 3.903576

# random sample of size 10 from a Normal(2,4) distribution
rnorm(10, mean = 2, sd = 2) # note the parameter spec. here
##  [1]  2.0745291  0.1719257  2.3249074  3.2447822  1.4329557  0.1173869 0.5231311  1.4754708  1.7591520 -1.8763400

## Setting the seed

rnorm(n = 5, mean = 1, sd = 2)
## [1]  1.7727385 -3.6486144  0.9951339  2.5762444 -0.8415272

rnorm(n = 5, mean = 1, sd = 2)
## [1]  1.22932369 -1.98687014  0.59244070  2.38830593 -0.01179321

set.seed(100)
rnorm(n = 5, mean = 1, sd = 2)
## [1] -0.004384701  1.263062331  0.842165820  2.773569619  1.233942541

set.seed(100)
rnorm(n = 5, mean = 1, sd = 2)

## [1] -0.004384701  1.263062331  0.842165820  2.773569619  1.233942541

#### Rounding ####

x <- runif(5, 2, 4)
x
## [1] 3.249993 3.764331 2.560708 2.796976 3.525102

round(x) # round to nearest integer
## [1] 3 4 3 3 4
round(x,3) # round to 3 places of decimal
## [1] 3.250 3.764 2.561 2.797 3.525

x
## [1] 3.249993 3.764331 2.560708 2.796976 3.525102

floor(x) # round down
## [1] 3 3 2 2 3

ceiling(x) # round up
## [1] 4 4 3 3 4

############# Character strings in R ##################

a <- "This course is on"    # create string a
b <- "Multivariate Data Analysis"     # create string b
# paste together string a & b
paste(a, b)                      
## [1] "This course is on Multivariate Data Analysis"

# paste character and number strings (converts numbers to character class)
paste("I want to eat a", pi)           
## [1] "I want to eat a 3.14159265358979"

# paste multiple strings
paste("Multivariate", " Data Analysis", "with", "R")            
## [1] "Multivariate Data Analysis with R"

# paste multiple strings with a separating character
paste("Data analysis", "applications","with", "R", sep = "-")  
## [1] "Data analysis-applications-with-R"

# use paste0() to paste without spaces btwn characters
paste0("Data analysis", "applications", "with", "R")            
## [1] "Data analysisapplicationswithR"

# paste objects with different lengths
paste("R", 1:5, sep = " v1.")       
## [1] "R v1.1" "R v1.2" "R v1.3" "R v1.4" "R v1.5"

############### Coercing in atomic vectors ##################

c("A",10)
## [1] "A"  "10"

typeof(c("A",10))
## [1] "character"

# Combining logical and numeric creates numeric

x <- c(10, 57, 3, 90)
x > 20
## [1] FALSE  TRUE FALSE  TRUE

sum(x > 20) # numeric function applied on logical
## [1] 2

############### Subsetting vectors in R  ###############

x_1 <- c(1:10)
x_2 <- 4
x_3 <- c(101:110)
x_4 <- c(2,3)
x_5 <- c(1,2,3)

x_1
##  [1]  1  2  3  4  5  6  7  8  9 10

x_1[4] #get the fourth element of vector x
## [1] 4

x_1[c(2,7)] # get elements in position 2 and 7
## [1] 2 7

x_1[-3] # everything except element in position 3
## [1]  1  2  4  5  6  7  8  9 10

x_1[c(-1,-3)] # everything except elements in pos. 1 & 3
## [1]  2  4  5  6  7  8  9 10

names(x_1) <- letters[1: length(x_1)] # assign names to x
x_1
##  a  b  c  d  e  f  g  h  i  j 
##  1  2  3  4  5  6  7  8  9 10

x_1[c("b","j")] # get elements named "b" and "j"
##  b  j 
##  2 10

x_1[x_1 > 6]
##  g  h  i  j 
##  7  8  9 10

x_1[4] #preserves (here, keeps the name)
## d 
## 4

x_1[[4]] # simplifies (here, name is removed)
## [1] 4

############### Matrices in R ###############

#a.matrix <- matrix(vector, nrow = r, ncol = c, byrow = FALSE, dimnames = list(char-vector-rownames, char-vector-col-names))
y <- matrix(1:20, nrow = 4, ncol = 5)
y
A = matrix(c(1,2,3,4),nrow=2,byrow=T)
A
A = matrix(c(1,2,3,4),ncol=2)
A
B = matrix(2:7,nrow=2)
B
C = matrix(5:2,ncol=2)
C
mr <- matrix(1:20, nrow = 5, ncol = 4, byrow = T)
mr
mc <- matrix(1:20, nrow = 5, ncol = 4)
mc

########## Arithmetic Operations on Matrix #############

A <- matrix(c(1:4), 2, 2)
B <- matrix(c(11:14), 2, 2)
A + B # elementwise addition
##      [,1] [,2]
## [1,]   12   16
## [2,]   14   18

A * B # elementwise multiplication
##      [,1] [,2]
## [1,]   11   39
## [2,]   24   56

A%*%B  #Matrix multiplication.
##      [,1] [,2]
##[1,]   47   55
##[2,]   70   82

dim(B) #Dimension
# [1] 2 2

nrow(B)
# [1] 2

ncol(B)
# [1] 2 

t(A)   #Transpose
##      [,1] [,2]
##[1,]    1    2
##[2,]    3    4

rownames(A) = c("a","b")
colnames(A) = c("d", "e")
A
##   d e
## a 1 3
## b 2 4

## Subsetting matrices are similar to atomic vectors

A[1,2] # element in row 1, column 2
## [1] 3

A[2, ] # row 2
## [1] 2 4

A[ ,1] # column 1
## [1] 1 2

B[,-1] # all except column 1
## [1] 13 14


########## Examples of lists in R ###############

x = list(name = 'Tanujit', nationality = 'Indian' , height =5.5 , marks =c(95,45,80))
names(x) 
## [1] "name"        "nationality" "height"      "marks" 

str(x)
## List of 4
## $ name       : chr "Tanujit"
## $ nationality: chr "Indian"
## $ height     : num 5.5
## $ marks      : num [1:3] 95 45 80

x$name
## [1] "Tanujit"

x$hei   #abbreviations are OK
## [1] 5.5

x$marks
## [1] 95 45 80

x$m[2] 
## [1] 45

typeof(x)
## [1] "list"

############## Adding lists ##############

l1 <- list(1:3, "a", c(TRUE, FALSE, TRUE), c(2.3, 5.9))
l2 <- list(l1, c(1:10))
str(l2)

## List of 2
##  $ :List of 4
##   ..$ : int [1:3] 1 2 3
##   ..$ : chr "a"
##   ..$ : logi [1:3] TRUE FALSE TRUE
##   ..$ : num [1:2] 2.3 5.9
##  $ : int [1:10] 1 2 3 4 5 6 7 8 9 10

#To add an additional list component without creating nested lists we use the append() function.
l3 <- append(l1, list(c(1:10)))
str(l3)
## List of 5
##  $ : int [1:3] 1 2 3
##  $ : chr "a"
##  $ : logi [1:3] TRUE FALSE TRUE
##  $ : num [1:2] 2.3 5.9
##  $ : int [1:10] 1 2 3 4 5 6 7 8 9 10

# Alternatively, we can add additional elements by using the $ sign and naming the item.

l1$x <- c(1.4,2.3)
str(l1)
## List of 5
##  $  : int [1:3] 1 2 3
##  $  : chr "a"
##  $  : logi [1:3] TRUE FALSE TRUE
##  $  : num [1:2] 2.3 5.9
##  $ x: num [1:2] 1.4 2.3

###############  Data Frame in R ############### 

d <- c(1,2,3,4)
e <- c("Tanujit", "Subhajit", "Indrajit", NA)
f <- c(TRUE,TRUE,TRUE,FALSE)
myframe <- data.frame(d,e,f)
names(myframe) <- c("ID","Name","Passed") # Variable names
myframe
myframe[1:3,] # Rows 1,2,3 of data frame
myframe[,1:2] # Columns 1,2 of data frame
myframe[c("ID","Name")] #Columns ID and color from data frame
myframe$ID # Variable ID in the data frame

###############  Factors in R ############### 
### Example: variable gender with 20 "male" entries and 30 "female" entries ###

gender <- c(rep("male",20), rep("female", 30))
gender <- factor(gender)   # Stores gender as 20 1s and 30 2s
# 1=male, 2=female internally (alphabetically)
# R now treats gender as a nominal variable
summary(gender)
## [1] female   male 
##       30     20 


section <- rep(c("A","B"), each = 3)
section
## [1] "A" "A" "A" "B" "B" "B"

typeof(section)
## [1] "character"

section <- as.factor(section)
section
## [1] A A A B B B
## Levels: A B

class(section)
## [1] "factor"

###############  Functions in R ############### 

g = function(x,y) (x+2*y)/5
g(5,10)
g(10,5)


###################### Use different packages after installation  ###########

install.packages('MASS')
library('MASS')

################### R Arithmetic operators #############################

# + : Addition
# - : subtraction
# * : multiplication
# / : division
# ^ or ** : exponentiation
# x%%y : Modulus (Remainder from division) (x mod y) : exam; 5%%2 is 1
# x%/%y : integer division 

##### Define variables and array to do the above Arithmetic operations #####

x <- 6
y <- 20
print(x+y)
print(x-y)
print(x*y)
print(y/x)
print(y%%x)
print(y%/%x)
print(y^x)


######### R Assignment Operators #########################

# <-, = : Leftwards assignment
# -> : Rightwards assignment
x3=4
print(x3 <- 3)
print(10->x3)

###########  Matrix Multiplication ##################

?matrix ### Know about functions ###
A = matrix(c(21,57,89,31,7,98), nrow =2, ncol=3, byrow = TRUE)
B = matrix(c(24, 35, 15, 34, 56,25), nrow = 3, ncol = 2, byrow = TRUE)
print(A)
print(B)
C = A%*%B ### Do the multiplication of A and B and stored it into C matrix ###
print(C)
print(det(C))
Inv <- solve(C) ### Do the inverse of a matrix ###
print(Inv)
print(eigen(C)) ### Find Eigenvalues ###

######### alternative way to find inverse of a matrix A1 ##############

## Using the inv() function:
## inv() function is a built-in function in R which is especially used to find the inverse of a matrix
## For that you need to install the matlib package in your environment. and use it using library() 
## It takes time .. Do it later 
## install.packages('matlib')
## library("matlib")
## print(inv(C))

##### R Functions for Probability Distributions #####

# Every distribution that R handles has four functions. There is a root name, for example, 
# the root name for the normal distribution is norm. This root is prefixed by one of the letters

# p for "probability", the cumulative distribution function (c. d. f.)
# q for "quantile", the inverse c. d. f.
# d for "density", the density function (p. f. or p. d. f.)
# r for "random", a random variable having the specified distribution
# For the normal distribution, these functions are pnorm, qnorm, dnorm, and rnorm. 
# For the binomial distribution, these functions are pbinom, qbinom, dbinom, and rbinom. And so forth.
# R has functions to handle many probability distributions like, Beta, Cauchy, Gamma, Poisson, etc.. 

#### Example of Normal Distribution ##############

# Direct Look-Up
# pnorm is the R function that calculates the c. d. f.
# F(x) = P(X <= x) where X is normal. 

print(pnorm(27.4, 50, 20)) # Here it look up P(X < 27.4) when X is normal with mean 50 and standard deviation 20.

# Inverse Look-Up

# qnorm is the R function that calculates the inverse c. d. f. F-1 of the normal distribution The c. d. f. and the inverse c. d. f. are related by

# p = F(x)
# x = F-1(p)

# So given a number p between zero and one, qnorm looks up the p-th quantile of the normal distribution.

# Q: What is F^(-1)(0.95) when X has the N(100, 15^2) distribution?

print(qnorm(0.95, mean=100, sd=15))

### Random Variates

# rnorm is the R function that simulates random variates having a specified normal distribution. 
# As with pnorm, qnorm, and dnorm, optional arguments specify the mean and standard deviation of the distribution.

x <- rnorm(100, mean=10, sd=5)
print(x)

###### below it plots the histogram of the above 100 random points generated from normal distribution with mean=10 and sd=5

print(hist(x, probability=TRUE)) ### hist plots Histogram ###

### Home Task: Do the same for other distributions for hands on study 

##### For any Functional Help write the following #####

# ?rnorm()
# ?pnorm()
# ?dnorm()
# ?qnorm()



################## Functions in R #######################

# Total accumulated value of an investment
compound_amount <- function(P, r, n, t){
  A <- P*(1 + r/n)^(n*t)
  return(A)
} 

# Suppose someone invested a sum of 10000 INR at an annual interest rate of 6.5%, 
# compounded quarterly for a period of 6 years. To calculate the accumulated sum, 
# we can use

P <- 10000; r <- 0.065; n <- 4; t <- 6
compound_amount(P, r, n, t)

formals(compound_amount)

body(compound_amount)

environment(compound_amount)


# Function arguments
# We can call the arguments in several ways

# using argument names
compound_amount(P = 10000, r = 0.065, n = 4, t = 6)
## [1] 14723.58

# same as above but without using names ("positional matching")
compound_amount(10000, .065, 4, 6)
## [1] 14723.58

# if using names you can change the order
compound_amount(r = .065, P = 10000, n = 4, t = 6)
## [1] 14723.58

# if not using names you must insert arguments in proper order
compound_amount(.08, 10000, 4, 6)
## [1] 2.869582e+80

# While writing a function we can also set default values to arguments.

compound_amount <- function(P, r = 0.065, n = 4, t){
  A <- P*(1 + r/n)^(n*t)
  return(A)
}

compound_amount(P = 10000, t = 6) # uses default values of r and n
## [1] 14723.58

compound_amount(P = 10000, r = 0.07, t = 6) # uses specified value of r, default for n
## [1] 15164.43


# Lexical scoping
# What is the output of this code?
x <- 10
g01 <- function() {
  x <- 20
  x
}

g01()

# R uses lexical scoping
# This means that a function will first look inside the function to identify all the variables being called.
# If all variables exist then their is no additional search required to identify variables.

P <- 1
compound_amount <- function(P, r, n, t){
  P <- 10000; r <- 0.065; n <- 4; t <- 6
  A <- P*(1 + r/n)^(n*t)
  return(A)
}
compound_amount(P, r, n, t)
## [1] 14723.58

#If a variable does not exist within the function, R will look one level up to see if the variable exists.
P <- 10000
compound_amount <- function(P, r, n, t){
  r <- 0.065; n <- 4; t <- 6
  A <- P*(1 + r/n)^(n*t)
  return(A)
}
compound_amount(P, r, n, t)
## [1] 14723.58

#The same rule applies for functions within functions
P <- 100
compound_amount <- function(P){
  r <- 0.065; n <- 4; t <- 6
  interest_factor <- function(){
    out <- (1 + r/n)^(n*t)
  }
  A <- P*interest_factor()
  return(A)
}
compound_amount(P)
## [1] 147.2358

# This also applies for functions in which some arguments are called but not all variables used in the body are identified as arguments.
r <- 0.07
compound_amount <- function(P, t){
  n <- 4
  A <- P*(1 + r/n)^(n*t)
  return(A)
}
# n is defined within the function
# r is defined outside the function
compound_amount(P = 10000, t = 6)
## [1] 15164.43

# Functions vs variables
# In R, functions are ordinary objects.
# This means the scoping rules described above also apply to functions.
cool_func <- function(x) x + 10
ubercool_func <- function() {
  cool_func <- function(x) x + 99
  cool_func(10)
}
ubercool_func()
## [1] 109

# Dynamic lookup
# Lexical scoping determines where, but not when to look for values.
# R looks for values when the function is run, not when the function is created.
# Together, these two properties tell us that the output of a function can differ depending on the objects outside the function’s environment.

foo <- function() x + 1
x <- 25
foo()
## [1] 26
x <- 50
foo()
## [1] 51

# Lazy evaluation
# In R, function arguments are lazily evaluated: they are only evaluated if accessed.
# For example, this code does not generate an error because x is never used.
h01 <- function(x) {
  10
}
h01(stop("This is an error!"))
## [1] 10

# the y argument is not used so not included it causes no harm
lazy_func <- function(x, y){
  x^2
}
lazy_func(2)
## [1] 4

# if both arguments are required in the body an error will result if an argument is missing
lazy_func2 <- function(x, y){
  (x + y)^2
}
lazy_func2(2)
## Error in lazy_func2(2): argument "y" is missing, with no default

# Returning multiple outputs
bad_func <- function(x, y) {
  2*x + y
  x + 2*y
  2*x + 2*y
  x/y
}
bad_func(2,3)
## [1] 0.6666667

good_func <- function(x, y) {
  output1 <- 2*x + y
  output2 <- x + 2*y
  output3 <- 2*x + 2*y
  output4 <- x/y
  c(output1, output2, output3, output4)
}
good_func(2, 3)
## [1]  7.0000000  8.0000000 10.0000000  0.6666667

good_list <- function(x, y) {
  output1 <- 2*x + y
  output2 <- x + 2*y
  output3 <- 2*x + 2*y
  output4 <- x/y
  c(list(Output1 = output1, Output2 = output2, 
         Output3 = output3, Output4 = output4))
}
good_list(1, 2)

## $Output1
## [1] 4
## $Output2
## [1] 5
## $Output3
## [1] 6
## $Output4
## [1] 0.5

############## Conditional Executions ##############

# if Statement
# syntax of if statement
if (test_expression) {
  statement
}

x <- 2
if(x < 3){
  y <- x + 2
  y
}
## [1] 4

# second example
coin <- rbinom(1,1,0.5) # tossing a fair coin once
toss_call <- "Heads"
coin # toss outcome
## [1] 1

if(coin == 1){
  print("Heads it is!")
}
## [1] "Heads it is!"

# if else statement
# syntax of if...else statement
if (test_expression) {
  # statement 1
} else {
  # statement 2
}

coin <- rbinom(1,1,0.5) # tossing a fair coin once
toss_call <- "Heads"
coin # toss outcome
## [1] 1

if(coin == 1){
  print("Heads it is!")
} else {
  print("No, it's Tails!")
}
## [1] "Heads it is!"

set.seed(100)
coin <- rbinom(1,1,0.5) # tossing a fair coin once
toss_call <- "Tails"
coin # toss outcome
## [1] 0

if(coin == 1 & toss_call == "Heads"){
  print("Heads it is!")
} else if(coin == 1 & toss_call == "Tails"){
  print("No, it's Heads!")
} else if(coin == 0 & toss_call == "Heads"){
  print("No it's Tails!")
} else if(coin == 0 & toss_call == "Tails"){
  print("Tails it is!")
}
## [1] "Heads it is!"

# FOR LOOP

# syntax of for loop
for(i in 1:100) {
  #  <do stuff here with i>
}

# squaring elements in a vector
x <- seq(from = 0, to = 10, by = 0.5)
xsq <- 0 #initialization

for(i in 1:length(x)){
  xsq[i] <- x[i]*x[i]
}

xsq 
##  [1]   0.00   0.25   1.00   2.25   4.00   6.25   9.00  12.25  16.00  20.25
## [11]  25.00  30.25  36.00  42.25  49.00  56.25  64.00  72.25  81.00  90.25
## [21] 100.00

#Note: This was just for demonstration. We could have just done this:

xsq <- x^2
xsq
##  [1]   0.00   0.25   1.00   2.25   4.00   6.25   9.00  12.25  16.00  20.25
## [11]  25.00  30.25  36.00  42.25  49.00  56.25  64.00  72.25  81.00  90.25
## [21] 100.00

for (i in 2023:2029){
  output <- paste("Good times shall come in ", i) # the "promise"
  print(output)
}
## [1] "Good times shall come in  2023"
## [1] "Good times shall come in  2024"
## [1] "Good times shall come in  2025"
## [1] "Good times shall come in  2026"
## [1] "Good times shall come in  2027"
## [1] "Good times shall come in  2028"
## [1] "Good times shall come in  2029"

print("Good times are yet to come!") # the "reality"
## [1] "Good times are yet to come!"

######### While loop #############

# syntax of while loop
counter <- 1

while(test_expression) {
  statement
  counter <- counter + 1
}
#The primary difference between a for loop and a while loop is:
#  a for loop is used when the number of iterations a code should be run is known 
#  where as a while loop is used when the number of iterations is not known.

wins <- 0; games <- 0
while(wins < 3) {
  x <- rbinom(1,1,0.5)
  wins <- wins + x
  games <- games + 1
  print(c(toss = x, games = games, wins = wins))
}
##  toss games  wins 
##     1     1     1 
##  toss games  wins 
##     0     2     1 
##  toss games  wins 
##     0     3     1 
##  toss games  wins 
##     1     4     2
##  toss games  wins 
##     1     5     3

########### Repeat Statement ###############

# We monitor a captain's toss winning record, and count how many games are required to win at least 3 tosses

games <- 0 # games counter
wins <- 0 # toss wins counter

repeat {
  games <- games + 1 # new game
  x <- rbinom(1,1,0.5) # toss outcome
  wins <- wins + x # wins update
  if(wins == 3){
    break
  }
}

c(wins, games) # toss wins and number of games
## [1] 3 6

################# BREAK STATEMENT #################


# Example: Suppose we want to create a 10 x 10 lower-triangular matrix with elements given by the product of the row and column numbers.
# This means that all matrix elements for which row number <= column number must be zero.

# use of breaks

m <- n <- 10 # matrix dimensions
# Create a 10 x 10 matrix with zeroes 
mymat <- matrix(0, nrow = m, ncol = n)
ctr <- 0 # counter to count the assignment

for(i in 1:m) {
  for(j in 1:n) {   
    if(i == j) { 
      break
    } else {
      mymat[i,j] <- i*j # assign the values only when i > j
      ctr <- ctr+1
    }
  }
}

# Print how many matrix cells were assigned
print(ctr)
## [1] 45

################## Descriptive Statistics #####################

# The monthly credit card expenses of an individual in 1000 rupees is 
# given in the file Credit_Card_Expenses.csv.
# Q1. Read the data set
# Q2. Compute mean, median minimum, maximum, range, variance, standard
# deviation, skewness, kurtosis and quantile of Credit Card Expenses
# Q3. Compute default summary of Credit Card Expenses
# Q4. Draw Histogram of Credit Card Expenses


#### read the csv file using read.csv() function 


Credit_Card_Expenses <- read.csv("Credit_Card_Expenses.csv")
Credit_Card_Expenses
mydata = Credit_Card_Expenses ### load it to another variable
print(mydata) ### print the data frame


### To read a particular column or variable of data set to a new variable Example: 
### Read CC_Expenses to CC

CC=mydata$CC_Expenses
print(CC)

######  Descriptive statistics for variable #####

Mean = mean(CC)
print(Mean)
Median=median(CC)
print(Median)
StandaradDeviation=sd(CC)
print(StandaradDeviation)
Variance=var(CC)
print(Variance)
Minimum=min(CC)
print(Minimum)
Maximum=max(CC)
print(Maximum)
Range=range(CC)
print(Range)
Quantile=quantile(CC)
print(Quantile)

Summary=summary(CC)
print(Summary)

##### Another way to calculate Descriptive statistics #####

# install.packages("psych")
library('psych')
data_descriptive=describe(CC)
print(data_descriptive)

#####  Plotting of the Descriptive Statistics #####

hist(CC)
hist(CC,col="blue")
dotchart(CC)
boxplot(CC)
boxplot(CC, col="dark green")

########################## Data Visualization techniques ##########################

#install.packages("ggplot2")
# Library Call (for use)
library("ggplot2")

# Installing packages
install.packages("tidyverse")
# Library Call (for use)
library("tidyverse")

######### Basics of ggplot2 #########

mpg

# create the blank canvas
ggplot(mpg)

# variables of interest mapped
ggplot(mpg, aes(x = displ, y = hwy))

# type of display
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point()

########## Aesthetics mapping ############

# plot displ vs hwy w.r.t different class of cars in mpg data. 

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))


# alpha aesthetic, which controls the transparency of the points
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

# shape aesthetic, which controls the shape of the points.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))

# You can also set the aesthetic properties of your geom manually. 
# For example, we can make all of the points in our plot red:

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "red")

############## Geometric Objects ###################

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point() + 
  geom_smooth()
# `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

ggplot(data = mpg, aes(x = class)) +
  geom_bar()

ggplot(data = mpg, aes(x = hwy)) +
  geom_histogram()
# `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = "blue") +
  geom_smooth(color = "red")
# `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()
# `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)
# `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

################ Statistical Transformation ################
ggplot(data = mpg, aes(x = class)) + geom_bar() 

ggplot(data = mpg, aes(x = class)) +
  geom_bar(mapping = aes(x = class, y = stat(prop), group = 1))

############### Position Adjustments ###############

# bar chart of class, colored by drive (front, rear, 4-wheel)
ggplot(mpg, aes(x = class, fill = drv)) + 
  geom_bar()

# position = "dodge": values next to each other
ggplot(mpg, aes(x = class, fill = drv)) + 
  geom_bar(position = "dodge")

# position = "fill": percentage/proportions chart
ggplot(mpg, aes(x = class, fill = drv)) + 
  geom_bar(position = "fill") + 
  ylab("proportion")

################ Co-ordinate Systems ###################

# flip x and y axis with coord_flip
ggplot(mpg, aes(x = class)) +
  geom_bar() +
  coord_flip()

################ Labels ##################
# Adding title
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(title = "Fuel efficiency generally decreases with engine size")

# We can also use labs() to replace the axis and legend titles.
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = FALSE) +
  labs(title = "Fuel efficiency generally decreases with engine size", x = "Engine displacement (L)",
       y = "Highway fuel economy (mog)",color = "Car types")


################ Scales ###############
ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class))

ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class)) +
  scale_x_continuous() +
  scale_y_continuous() +
  scale_colour_discrete

# Change the scale of y axis
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  scale_y_continuous(breaks = seq(15, 40, by = 5))

# Change the legend key
base <- ggplot(mpg, aes(displ, hwy)) +
  geom_point(aes(colour = class))
base + theme(legend.position = "left") # 'right' is default

#################### Facets #####################

# Adding another varaible in the plot
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)

# Facet plots with combination of two variables
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)

#################### Saving your plots ##################
ggplot(mpg, aes(displ, hwy)) + geom_point()
ggsave("plot1.png")
# Saving 7.29 x 5.56 in image
################ Big Mart Data Visualization ####################

data_mart = read.csv("Big_Mart_Dataset.csv")
print(data_mart) # Read and Print BigMart dataset

library(ggplot2) # Scatter Plot of Item_Visibility & Item_MRP 
print(ggplot(data_mart, aes(Item_Visibility, Item_MRP)) + geom_point() +
        scale_x_continuous("Item Visibility", breaks = seq(0,0.35,0.05))+
        scale_y_continuous("Item MRP", breaks = seq(0,270,by = 30))+ theme_bw())


# Now, we can view a third variable also in same chart, say a categorical variable (Item_Type) which will give the characteristic (item_type)
# of each data set. Different categories are depicted by way of different color for
# item_type in below chart. Another scatter plot using function ggplot() with geom_point().


print(ggplot(data_mart, aes(Item_Visibility, Item_MRP)) + 
        geom_point(aes(color = Item_Type)) + scale_x_continuous("Item Visibility", breaks = seq(0,0.35,0.05)) + 
        scale_y_continuous("Item MRP", breaks = seq(0,270,by = 30))+ theme_bw() + 
        labs(title="Scatterplot"))

# We can even make it more visually clear by creating separate
# scatter plots for each separate Item_Type as shown below.
# Another scatter plot using function ggplot() with geom_point().

print(ggplot(data_mart, aes(Item_Visibility, Item_MRP)) + 
        geom_point(aes(color = Item_Type)) + 
        scale_x_continuous("Item Visibility", breaks = seq(0,0.35,0.05)) + 
        scale_y_continuous("Item MRP", breaks = seq(0,270, by = 30))+ 
        theme_bw() + labs(title="Scatterplot") + facet_wrap( ~ Item_Type))

########## Histogram Plot ########################

# For Big_Mart_Dataset, if we want to know the count of items on basis of their
# cost, then we can plot histogram using continuous variable Item_MRP as shown below.
# Histogram plot using function ggplot() with geom_ histogram()

print(ggplot(data_mart, aes(Item_MRP)) + geom_histogram(binwidth = 2)+
        scale_x_continuous("Item MRP", breaks = seq(0,270,by = 30))+
        scale_y_continuous("Count", breaks = seq(0,200,by = 20))+ labs(title = "Histogram"))


##############  Bar Chart Plot ###############################

# For Big_Mart_Dataset, if we want to know item weights (continuous variable)
# on basis of Outlet Type (categorical variable) on single bar chart as shown below.
# Vertical Bar plot using function ggplot()

print(ggplot(data_mart, aes(Item_Type, Item_Weight)) + geom_bar(stat = "identity", fill =
                                                                  "darkblue") + scale_x_discrete("Outlet Type")+ 
        scale_y_continuous("Item Weight", breaks = seq(0,15000, by = 500))+ 
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + 
        labs(title = "Bar Chart"))

##################  Stack Bar Chart #########################

# For Big_Mart_Dataset, if we want to know the count of outlets on basis of
# categorical variables like its type (Outlet Type) and location (Outlet Location
# Type) both, stack chart will visualize the scenario in most useful manner.
# Stack Bar Chart using function ggplot()

print(ggplot(data_mart, aes(Outlet_Location_Type, fill = Outlet_Type)) +
        geom_bar()+labs(title = "Stacked Bar Chart", x = "Outlet Location Type", y =
                          "Count of Outlets"))

############ Box Plot  ##########################################

# For Big_Mart_Dataset, if we want to identify each outlet's detailed item sales
# including minimum, maximum & median numbers, box plot can be helpful. In
# addition, it also gives values of outlier of item sales for each outlet as shown
# in below chart.

print(ggplot(data_mart, aes(Outlet_Identifier, Item_Outlet_Sales)) + 
        geom_boxplot(fill = "red")+ 
        scale_y_continuous("Item Outlet Sales", breaks= seq(0,15000, by=500))+
        labs(title = "Box Plot", x = "Outlet Identifier"))

### To save these charts, click on Export - Save as ... ###

##################### Area Chart ####################################

# For Big_Mart_Data set, when we want to analyze the trend of item outlet sales,
# area chart can be plotted as shown below. It shows count of outlets on basis of sales.

print(ggplot(data_mart, aes(Item_Outlet_Sales)) + 
        geom_area(stat = "bin", bins = 30, fill = "steelblue") + 
        scale_x_continuous(breaks = seq(0,11000,1000))+ 
        labs(title = "Area Chart", x = "Item Outlet Sales", y = "Count"))

# Area chart shows continuity of Item Outlet Sales.


######################  Heat Map: ############################################

# For Big_Mart_Dataset, if we want to know cost of each item on every outlet,
# we can plot heatmap as shown below using three variables Item MRP, Outlet
# Identifier & Item Type from our mart dataset

print(ggplot(data_mart, aes(Outlet_Identifier, Item_Type))+ 
        geom_raster(aes(fill = Item_MRP))+ 
        labs(title ="Heat Map", x = "Outlet Identifier", y = "Item Type")+
        scale_fill_continuous(name = "Item MRP"))

# The dark portion indicates Item MRP is close 50. The brighter portion indicates
# Item MRP is close to 250.

##################### Correlogram  ##########################

# For Big_Mart_Dataset, check co-relation between Item cost, weight, visibility
# along with Outlet establishment year and Outlet sales from below plot.
# install.packages("corrgram")

install.packages("corrgram")
library(corrgram)
print(corrgram(data_mart, order=NULL, panel=panel.shade, text.panel=panel.txt,
               main="Correlogram"))

# Darker the color, higher the co-relation between variables. Positive co-
# relations are displayed in blue and negative correlations in red color. Color
# intensity is proportional to the co-relation value.

# We can see that Item cost & Outlet sales are positively correlated while Item
# weight & its visibility are negatively correlated.

################################# END OF SESSION ###############################

