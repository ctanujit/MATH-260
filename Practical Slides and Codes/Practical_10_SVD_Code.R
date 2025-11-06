rm(list = ls())
############### Compute SVD for a matrix ##############
A = matrix(rnorm(16), nrow = 4)
svd(A)

################ Applications of SVD ################

############### Moore-Penrose inverse ###############
# Generate a 5*3 matrix
set.seed(123)
X = matrix(rnorm(15),nrow = 5)

# Obtain the inverse of the matrix
solve(X) # Not possible as X is not a square matrix

# Computing the inverse of X using SVD 
# X  = USt(V) => X^(-1) = VS^(-1)t(U)
svd_1 = svd(X)
sig = diag(svd_1$d)
s_inverse = svd_1$u %*% solve(sig) %*% t(svd_1$v)
print(s_inverse)

# Available functions
library(MASS)
#Generalised inverse
g_inverse = ginv(X) 
print(g_inverse)

library(pracma)
#moore - Penrose Pseudoinverse
p_inverse = pinv(X)
print(p_inverse)
# All the methods calculate inverse using the Singular Value Decomposition method

####################### Image Compression ########################
install.packages("imager")
library(imager)
image = load.image("Test.png")
plot(image,axes=FALSE,main="Original image")

# Plot the greyscale image
megray<-grayscale(image)
plot(megray,axes=FALSE,main="Grayscale image")

# Computing svd of grayscale image
svd1<-svd(megray)
str(svd1)

#matrix multiplication of U,D and V*

#including only first 5 singular vectors
approx5<-svd1$u[,1:5] %*% diag(svd1$d[1:5]) %*% t(svd1$v[,1:5])

#including only first 10 singular vectors
approx10<-svd1$u[,1:10] %*% diag(svd1$d[1:10]) %*% t(svd1$v[,1:10])

#including only first 25 singular vectors
approx25<-svd1$u[,1:25] %*% diag(svd1$d[1:25]) %*% t(svd1$v[,1:25])

#including only first 25 singular vectors
approx35<-svd1$u[,1:35] %*% diag(svd1$d[1:35]) %*% t(svd1$v[,1:35])

#including only first 50 singular vectors
approx50<-svd1$u[,1:50] %*% diag(svd1$d[1:50]) %*% t(svd1$v[,1:50])

#including only first 100 singular vectors
approx100<-svd1$u[,1:100] %*% diag(svd1$d[1:100]) %*% t(svd1$v[,1:100])

#including only first 150 singular vectors
approx150<-svd1$u[,1:150] %*% diag(svd1$d[1:150]) %*% t(svd1$v[,1:150])

#including only first 180 singular vectors
approx180<-svd1$u[,1:180] %*% diag(svd1$d[1:180]) %*% t(svd1$v[,1:180])


par(mfrow=c(3,3),mar=c(1,1,1,1))
#plotting for reduced images
plot(as.cimg(approx5),main="5 singular Vectors",axes=FALSE)
plot(as.cimg(approx10),main="10 singular Vectors ",axes=FALSE)
plot(as.cimg(approx25),main="25 singular Vectors",axes=FALSE)
plot(as.cimg(approx35),main="35 singular Vectors",axes=FALSE)
plot(as.cimg(approx50),main="50 singular Vectors",axes=FALSE)
plot(as.cimg(approx100),main="100 singular Vectors",axes=FALSE)
plot(as.cimg(approx150),main="150 singular Vectors",axes=FALSE)
plot(as.cimg(approx180),main="180 singular Vectors",axes=FALSE)
plot(as.cimg(megray),main="Full image",axes=FALSE)
dev.off()
# Difference between original image and compressed image

library(Matrix)
width = function(x) dim(x)[1]
height = function(x) dim(x)[2]
rank = function(x)  rankMatrix(x)
w = c(width(approx5), width(approx10), width(approx25),width(approx35),
      width(approx50), width(approx100), width(approx150), 
      width(approx180),width(megray[,]))
r = c(rank(approx5), rank(approx10), rank(approx25),rank(approx35), rank(approx50), 
      rank(approx100), rank(approx150), rank(approx180),rank(megray[,]))
h = c(height(approx5), height(approx10), height(approx25),height(approx35),
      height(approx50), height(approx100), height(approx150), 
      height(approx180),height(megray[,]))
comp <- data.frame("Width" = w , "Height" = h , "Rank" = r, row.names =
                     c("5 Singular values","10 Singular values","25 Singular values",
                       "35 Singular values","50 Singular values","100 Singular values",
                       "150 Singular values","180 Singular values","Original Image"))
print(comp)

# Compressing the original RGB image

plot(image,axes=FALSE,main="Original image")
dim(image)[1:2] # Height and width of image

svd_gr = svd(scale(megray))
plot(svd_gr$d/sum(svd_gr$d), pch = 19, xlab ="Number of Singular Values", ylab = " " ,main = "Variance explained by sigular values")

R = image[,,1]
G = image[,,2]
B = image[,,3]

par(mfrow=c(1,3),mar=c(1,1,1,1))
cscale <- function(v) rgb(v,0,0)
grayscale(image) %>% plot(colourscale=cscale,rescale=FALSE,axes=FALSE)

cscale <- function(v) rgb(0,v,0)
grayscale(image) %>% plot(colourscale=cscale,rescale=FALSE,axes=FALSE)

cscale <- function(v) rgb(0,0,v)
grayscale(image) %>% plot(colourscale=cscale,rescale=FALSE,axes=FALSE)
dev.off()
svdR<-svd(R)
svdG<-svd(G)
svdB<-svd(B)
svdRGB<-list(svdR,svdG,svdB)
str(svdRGB)

par(mfrow=c(3,3),mar=c(1,1,1,1))
for(j in c(5,10,20,35,50,75,100,150)){
  comp <- sapply(svdRGB, function(i){
    compressed = i$u[,1:j] %*% diag(i$d[1:j]) %*% t(i$v[,1:j])
  }, simplify = 'array')
  comp<-as.cimg(comp)
  plot(comp,axes=FALSE,main=paste("Rank=",j))
}
plot(image,axes=FALSE,main="Original image")
dev.off() # Reset par function

################################################################################