###################### Data Pre-processing ######################

# Installing packages
install.packages("tidyverse")
# Library Call (for use)
library("tidyverse")

### Use Import Dataset - From Text (base) - Missing_Values_Telecom - Heading - Yes ###
mydata = read.csv("Missing_Values_Telecom.csv")
newdata = na.omit(mydata) # Discard all records with missing values
write.csv(newdata, file = 'newdata.csv',row.names = TRUE)

### Compute the means excluding the missing values
cmusage = mydata[,2] 
l3musage = mydata[,3] 
avrecharge = mydata[,4]
cmusage_mean = mean(cmusage, na.rm = TRUE) 
l3musage_mean = mean(l3musage, na.rm = TRUE)
avrecharge_mean = mean(avrecharge, na.rm = TRUE)

### Replace the missing values with mean
cmusage[is.na(cmusage)]=cmusage_mean
l3musage[is.na(l3musage)]= l3musage_mean 
avrecharge[is.na(avrecharge)]=avrecharge_mean

mynewdata = cbind(cmusage, l3musage, avrecharge, mydata[,5],mydata[,6]) 
mydata
mynewdata
write.csv(mynewdata, file = 'Missing_Values_Telecom_mod.csv',row.names = TRUE)

############## Data Normalization and Random Sampling #################

mydata = read.csv("Supply_Chain.csv")
# mydata = Supply_Chain
mystddata = scale(mydata)
mystddata

mydata= read.csv("bank_data.csv")
nrow(mydata)
sample = sample(2, nrow(mydata), replace = TRUE, prob = c(0.750, 0.250))
sample1 = mydata[sample ==1,]
nrow(sample1)
sample2 = mydata[sample ==2,]
nrow(sample2)

################## Playing with the data #######################
library(tidyverse)

# load packages
suppressMessages(library(dplyr))
install.packages("hflights")
library(hflights)

data(hflights)
head(hflights)

# Revert to the original data frame to view all the columns
head(data.frame(hflights))

######## Using 'filter' function ############

## Extract details of flights operating on January 01.

# base R approach to view all flights on January 1
flights[flights$Month==1 & flights$DayofMonth==1, ]

# try the filter function of dplyr for the same question
# dplyr approach
# note: you can use comma or ampersand to represent AND condition
filter(flights, Month==1, DayofMonth==1)

# Check the two answers

## Extract details of flights operated by American Airlines or United Airlines.

# use pipe for OR condition
filter(flights, UniqueCarrier=="AA" | UniqueCarrier=="UA")

# you can also use %in% operator
filter(flights, UniqueCarrier %in% c("AA", "UA"))

# Check the two answers

########## Using 'select' function ###########

# Show flight details with Dep/Arr times alongwith flight numbers only.
# base R approach to select DepTime, ArrTime, and FlightNum columns
flights[, c("DepTime", "ArrTime", "FlightNum")]

# dplyr approach
select(flights, DepTime, ArrTime, FlightNum)
# Check the two answers

# Use colon to select multiple contiguous columns, and use contains to match columns by name
# starts_with, ends_with, and matches (for regular expressions) can also be used to match columns by name
select(flights, Year:DayofMonth, contains("Taxi"), contains("Delay"))

select(flights, Year:DayofMonth, starts_with("Taxi"), ends_with("Delay"))

############## Using 'pipe' operator #############
# Show unique carrier details for flights having departure delays of more than 1 hour.
# nesting method
filter(select(flights, UniqueCarrier, DepDelay), DepDelay > 60)

# chaining method
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  filter(DepDelay > 60)

# Advantage of chaining

# create two vectors and calculate Euclidian distance between them
x1 <- 1:5; x2 <- 2:6
sqrt(sum((x1-x2)^2))

# chaining method
(x1-x2)^2 %>% sum() %>% sqrt()
## [1] 2.236068

############ Using arrange: Reorder rows ############
#Extract flight carrier details and show departure delays only sorted by delay lengths.
# base R approach
flights[order(flights$DepDelay), c("UniqueCarrier", "DepDelay")]

# dplyr approach
flights %>%
  select(UniqueCarrier, DepDelay) %>%
  arrange(DepDelay)# arrange(desc(DepDelay)) for descending order

############ Using mutate: Add new variables ############
# Find mean speed of flights.
# base R approach
flights$Speed <- flights$Distance / flights$AirTime*60
flights[, c("Distance", "AirTime", "Speed")]

# dplyr approach (prints the new variable but does not store it)
flights %>%
  select(Distance, AirTime) %>%
  mutate(Speed = Distance/AirTime*60)

flights <- flights %>% mutate(Speed = Distance/AirTime*60)
flights

############## Using summarise #############

# Finding the average delay to each destination

# dplyr approach: create a table grouped by Dest, and then summarise each group by taking the mean of ArrDelay
flights %>%
  group_by(Dest) %>%
  summarise(avg_delay = mean(ArrDelay, na.rm=TRUE))

# For each day of the year, count the total number of flights and sort in descending order.
flights %>%
  group_by(Month, DayofMonth) %>%
  summarise(flight_count = n()) %>%
  arrange(desc(flight_count))

## `summarise()` has grouped output by 'Month'. You can override using the
## `.groups` argument.

########### Uisng n_distinct(vector) ##########
# For each destination, count the total number of flights and the number of distinct planes that flew there.
flights %>%
  group_by(Dest) %>%
  summarise(flight_count = n(), plane_count = n_distinct(TailNum))

########## Using Window function ############

# For each carrier, calculate which two days of the year they had their longest departure delays.
# Note: smallest (not largest) value is ranked as 1,
# so you have to use `desc` to rank by largest value
flights %>%
  group_by(UniqueCarrier) %>%
  select(Month, DayofMonth, DepDelay) %>%
  filter(min_rank(desc(DepDelay)) <= 2) %>%
  arrange(UniqueCarrier, desc(DepDelay))

# rewrite more simply with the `top_n` function
flights %>%
  group_by(UniqueCarrier) %>%
  select(Month, DayofMonth, DepDelay) %>%
  top_n(2) %>%
  arrange(UniqueCarrier, desc(DepDelay))

######## Useful Convenience Functions #############
# randomly sample a fixed number of rows, without replacement
flights %>% sample_n(5)

# randomly sample a fraction of rows, with replacement
flights %>% sample_frac(0.25, replace=TRUE)

# base R approach to view the structure of an object
str(flights)

# dplyr approach: better formatting, and adapts to your screen width
glimpse(flights)


############################ EDA Email Case Study ###########################

# Loading email dataset
library(tidyverse)
library(openintro) #for loading our case study dataset
data(email)
email

# Exploring number of characters in spam vs non-spam emails
email <- email %>%
  mutate(spam = as.factor(spam)) %>%
  mutate(spam = recode(spam, "0" = "Not_spam", "1" = "Spam"))

email

# Compute summary statistics
email %>%
  group_by(spam) %>%
  summarize(median(num_char),
            IQR(num_char))

# Visualize
email %>%
  ggplot(aes(x = spam, y = num_char)) +
  geom_boxplot() + 
  labs(x = "Spam status", y = "No. of characters")

# It might be a good idea to change the scale of the y-axis.

# Visualize
email %>%
  mutate(log_num_char = log(num_char)) %>%
  ggplot(aes(x = spam, y = log_num_char)) +
  geom_boxplot()+
  labs(x = "Spam status", y = "No. of characters(log-scale)")

# exclaim_mess vs spam
email %>%
  group_by(spam) %>%
  summarise(MED = median(exclaim_mess),
            IQR = IQR(exclaim_mess), SD = sd(exclaim_mess))

# Visualize
email %>%
  mutate(log_exclaim_mess = log(exclaim_mess + 0.001)) %>%
  mutate(custom_fill = as.factor(exclaim_mess > 0)) %>%
  ggplot(aes(x = log_exclaim_mess, fill = custom_fill)) +
  geom_histogram() + theme(legend.position = "none") + facet_wrap(~ spam)

# Visualize: Box-plots
email %>%
  mutate(log_exclaim_mess = log(exclaim_mess + 0.001)) %>%
  ggplot(aes(x = spam, y = log_exclaim_mess)) +
  geom_boxplot()

# Tackling inflated Zeroes
email %>%
  mutate(zero_exclaim = (exclaim_mess == 0)) %>%
  ggplot(aes(x = zero_exclaim)) +
  geom_bar() + 
  facet_wrap(~spam)

email %>%
  mutate(zero_exclaim = (exclaim_mess == 0)) %>%
  ggplot(aes(x = zero_exclaim, fill = spam)) +
  geom_bar(position = "fill")

# Tackling inflated zeroes: example with the image variable
email %>% 
  count(image)

# There are 3811 images with no images attached. This poses a zero inflation problem.
email %>%
  mutate(image_present = (image > 0)) %>%
  ggplot(aes(x = image_present, fill = spam)) +
  geom_bar(position = "fill")


################################# END OF SESSION ###############################





