##########Principal Components Analysis##########

library(readr)

# Reading the excel file
mydata<- read_csv("PCA_Factor_Analysis_Example.csv")
# View(mydata)
mydata = mydata[,2:7]

#Transforming the variables
myzdata = scale(mydata)

#Checking correlation
cor(myzdata)
#The results show high correlation between X1, X3 & X5 and good correlation between X2, X4 & X6

#Fitting & Summarizing the model
mymodel = princomp(myzdata)
summary(mymodel)

#Determine the number of Factors
plot(mymodel)

#Reduced Data Sets
pc = mymodel$scores
cbind(pc[,1],pc[,2] )


############Factor Analysis#############

# Reading the excel file
mydata<- read_csv("PCA_Factor_Analysis_Example.csv")
# View(mydata)
mydata = mydata[,2:7]

#Transforming the variables
myzdata = scale(mydata)

#Checking correlation
cor(myzdata)
#The results show high correlation between X1, X3 & X5 and good correlation between X2, X4 & X6

#Check for Sampling (factor) adequacy

install.packages("psych")

library(psych)
KMO(myzdata)

# Identifying the number of factors  
#Compute eigenvalues
#Choose the factors with eigenvalues > 1

s = cov(myzdata)
s_eigen = eigen(s)
variance = s_eigen$values

#Determine the number of Factors
barplot(variance, xlab = "Factor", ylab = 'Variance', main = "Scree Plot")

#Calculate Factor Scores
mymodel = factanal(myzdata, 2)

#Calculate Factor Scores Rotation
myrotatedmodel = factanal(myzdata, 2, rotation = "varimax", scores = "regression")
myrotatedmodel

#Reduced Data Set
output = myrotatedmodel$scores
output

############Cluster Analysis############

# Reading the excel file
mydata <- read_csv("Cluster_Analysis_Example.csv")
# View(mydata)

#Computing the distance
distance = dist(mydata, method = 'euclidean')

#Generate Cluster
mymodel = hclust(distance, method = "ward.D")
#Plot Dendogram
plot(mymodel)

#Deciding the number of clusters
groups = cutree(mymodel, k = 3)
rect.hclust(mymodel, k = 3, border = "red")

#Identification of cluster membership for each record
output = cbind(mydata, groups)
print(output)

#Cluster Profile
aggregate(mydata, by = list(groups), FUN = mean)

## Cluster Analysis using k-mean method ##

mynewmodel = kmeans(mydata,3)
mynewmodel
cluster = mynewmodel$cluster
output = cbind(mydata, cluster)
print(output)

# To find optimum k : Elbow Method
# function to compute total within-cluster sum of square 

wss <- function(k) 
  {
  kmeans(mydata, k, nstart = 1)$tot.withinss
  }
k.values <- 1:10
wss_values <- purrr::map_dbl(k.values, wss)
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

################### END ##############################


