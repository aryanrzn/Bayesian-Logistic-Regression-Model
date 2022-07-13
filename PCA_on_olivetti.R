require(dplyr)
# X: features
X_df <- read.csv("olivetti_X.csv",header = F) %>% as.matrix()
# y: labels
y_df <- read.csv("olivetti_y.csv",header = F) %>% as.matrix()

# Function to plot image data
plt_img <- function(x){ image(x, col=grey(seq(0, 1, length=256)))}
# Visualize Images
par(mfrow=c(1,2))
par(mar=c(0.1,0.1,0.1,0.1))
# Display first image vector
b <- matrix(as.numeric(X_df[1, ]), nrow=64, byrow=T)
plt_img(b)
# Rotate first Image 90 degree for display
c <- t(apply(matrix(as.numeric(X_df[1, ]), nrow=64, byrow=T), 2, rev))
plt_img(c)

newdata<-NULL
# Rotate every image and save to a new file for easy display in R
for(i in 1:nrow(X_df))
{
  # Rotated Image 90 degree
  c <- as.numeric((apply(matrix(as.numeric(X_df[i, ]), nrow=64, byrow=T), 2, rev)))
  # Vector containing the image
  newdata <- rbind(newdata,c)
}

df=as.data.frame(newdata)
write.csv(df,'train_faces.csv',row.names=FALSE)

# Next time you can start from loading train_faces.csv
# df = read.csv('D:\\sbu\\Educational\\MultivariateII\\MultII\\faces\\train_faces.csv',header = T)
################################################
#Take average of each person's photo and Display the average faces image

# Average face: Mean Values

par(mfrow=c(2,2))
par(mar=c(0.1,0.1,0.1,0.1))

AV1=colMeans(data.matrix(df[1:10,]))
plt_img(matrix(AV1,nrow=64,byrow=T))

AV2=colMeans(data.matrix(df[11:20,]))
plt_img(matrix(AV2,nrow=64,byrow=T))

AV39=colMeans(data.matrix(df[381:390,]))
plt_img(matrix(AV39,nrow=64,byrow=T))

AV40=colMeans(data.matrix(df[391:400,]))
plt_img(matrix(AV40,nrow=64,byrow=T))
c <- t(apply(matrix(as.numeric(X_df[400, ]), nrow=64, byrow=T), 2, rev))
plt_img(c)


#---------------------------------------------------------------------------
#Principal Components Analysis (PCA)
#PCA is a statistical procedure that uses an orthogonal transformation to convert a set of observations of possibly correlated variables (entities each of which takes on various numerical values) into a set of values of linearly uncorrelated variables called principal components.

#This transformation is defined in such a way that the first principal component has the largest possible variance (that is, accounts for as much of the variability in the data as possible), and each succeeding component in turn has the highest variance possible under the constraint that it is orthogonal to the preceding components.

#PCA was invented in 1901 by Karl Pearson, as an analogue of the principal axis theorem in mechanics. it was later independently developed and named by Harold Hotelling in the 1930s. Depending on the field of application, it is also named the discrete Karhunen-Lo?ve transform (KLT) in signal processing, the Hotelling transform in multivariate quality control, proper orthogonal decomposition (POD) in mechanical engineering, singular value decomposition (SVD) of X (Golub and Van Loan, 1983)

#Subtract the mean image from the data, and get the covariance matrix.

#Calculate the eigenvectors and eigenvalues of the covariance matrix.

#Find the optimal transformation matrix by selecting the principal components (eigenvectors with largest eigenvalues).

library(RSpectra)

# df <- read.csv("C:/Python36/my_project/project_test/train_faces.csv") 
D <- data.matrix(df)

## Let's look at the average face, and need to be substracted from all image data
average_face=colMeans(df)
AVF=matrix(average_face,nrow=1,byrow=T)
plt_img(matrix(average_face,nrow=64,byrow=T))
#-------------------------------------------------------------------------------
# Perform PCA on the data

# Step 1: scale data
# Scale as follows: mean equal to 0, stdv equal to 1
D <- scale(D)

# Step 2: calculate covariance matrix
A <- cov(D)
A_ <- t(D) %*% D / (nrow(D)-1)
# Note that the two matrices are the same
max(A - A_) # Effectively zero
rm(A_)

# Note: diagonal elements are variances of images, off diagonal are covariances between images
identical(var(D[, 1]), A[1,1])
identical(var(D[, 2]), A[2,2])
identical(cov(D[, 1], D[, 2]), A[1,2])
#-------------------------------------------------------------------------------
# Step 3: calculate eigenvalues and eigenvectors

# Calculate the largest 40 eigenvalues and corresponding eigenvectors 
eigs <- eigs(A, 40, which = "LM")
# Eigenvalues
eigenvalues <- eigs$values
# Eigenvectors (also called loadings or "rotation" in R prcomp function: i.e. prcomp(A)$rotation)
eigenvectors <- eigs$vectors

par(mfrow=c(1,1))
par(mar=c(2.5,2.5,2.5,2.5))
y=eigenvalues[1:40]
# First 40 eigenvalues dominate
plot(1:40, y, type="o", log = "y", main="Magnitude of the 40 biggest eigenvalues", xlab="Eigenvalue #", ylab="Magnitude")
# Check that the 40 biggest eigenvalues account for 85 % of the total variance in the dataset

#sum(eigenvalues)/sum(eigen(A)$values)


# New variables (the principal components, also called scores, and called x in R prcomp function: i.e. prcomp(A)$x)
D_new <- D %*% eigenvectorsrs   #400*4096 %*% 4096*40 = 400*40
#-------------------------------------------------------------------------------
#Plot of Eigenfaces (Eigenvectors)
#Eigenvectors are a special set of vectors associated with a linear system of equations (i.e., a matrix equation) that are sometimes also known as characteristic vectors, proper vectors, or latent vectors
# Plot of first 6 eigenfaces
par(mfrow=c(3,2))
par(mar=c(0.2,0.2,0.2,0.2))
for (i in 1:6){
  plt_img(matrix(as.numeric(eigenvectors[, i]),nrow=64,byrow=T))   #Jahate davaran ro be ma neshan midahad va mibinim az haman jense ax ast va etelate khubi darad
}
#-------------------------------------------------------------------------------
#Projection of the photo onto the eigenvector space
par(mfrow=c(2,2))
par(mar=c(2,2,2,2))
c <- t(apply(matrix(as.numeric(X_df[11, ]), nrow=64, byrow=T), 2, rev))
plt_img(c)
# projection of 1st photo into eigen space
# reduce the dimension from 4096 to 40
PF1 <- data.matrix(df[1,]) %*% eigenvectors
barplot(PF1,main="projection coefficients in eigen space", col="blue",ylim = c(-40,10))
legend("topright", legend = "1st photo")

# projection of 11th photo into eigen space
# reduce the dimension from 4096 to 40
PF2 <- data.matrix(df[2,]) %*% eigenvectors
barplot(PF2,main="projection coefficients in eigen space", col="blue",ylim = c(-40,10))
legend("topright", legend = "2nd photo")

# projection of 1st photo into eigen space
# reduce the dimension from 4096 to 40
PF3 <- data.matrix(df[3,]) %*% eigenvectors
barplot(PF3,main="projection coefficients in eigen space", col="blue",ylim = c(-40,10))
legend("topright", legend = "3rd photo")

# projection of 4th photo into eigen space
# reduce the dimension from 4096 to 40
PF4 <- data.matrix(df[4,]) %*% eigenvectors
barplot(PF4,main="projection coefficients in eigen space", col="blue",ylim = c(-40,10))
legend("topright", legend = "4th photo")
#-------------------------------------------------------------------------------
par(mfrow=c(2,2))
par(mar=c(2,2,2,2))

# projection of 1st photo into eigen space
# reduce the dimension from 4096 to 40
PF1 <- data.matrix(df[1,]) %*% eigenvectors
barplot(PF1,main="projection coefficients in eigen space", col="blue",ylim = c(-40,10))
legend("topright", legend = "1st photo")

# projection of 11th photo into eigen space
# reduce the dimension from 4096 to 40
PF2 <- data.matrix(df[11,]) %*% eigenvectors
barplot(PF2,main="projection coefficients in eigen space", col="red",ylim = c(-40,10))
legend("topright", legend = "11th photo")

# projection of 1st photo into eigen space
# reduce the dimension from 4096 to 40
PF3 <- data.matrix(df[21,]) %*% eigenvectors
barplot(PF3,main="projection coefficients in eigen space", col="green",ylim = c(-40,10))
legend("topright", legend = "21st photo")

# projection of 11th photo into eigen space
# reduce the dimension from 4096 to 40
PF4 <- data.matrix(df[31,]) %*% eigenvectors
barplot(PF4,main="projection coefficients in eigen space", col="grey",ylim = c(-40,10))
legend("topright", legend = "31st photo")
#-------------------------------------------------------------------------------
#Reconstruction of the photo from the eigenvector space
#reconstructed from eigen space
# Every face has different projection on eigenvector space.
# We can use these new fewer values for a classification task.
par(mfrow=c(2,2))
par(mar=c(1,1,1,1))

# 1st person 1st photo
plt_img(matrix(as.numeric(df[1, ]), nrow=64, byrow=T))

# 1st person project into eigen space and reconstruct
PF1 <- data.matrix(df[1,]) %*% eigenvectors
RE1 <- PF1 %*% t(eigenvectors)
plt_img(matrix(as.numeric(RE1),nrow=64,byrow=T))

# 2nd person 1st photo
plt_img(matrix(as.numeric(df[11, ]), nrow=64, byrow=T))

# 2nd persoon project into eigen space and reconstruct
PF2 <- data.matrix(df[11,]) %*% eigenvectors
RE2 <- PF2 %*% t(eigenvectors)
plt_img(matrix(as.numeric(RE2),nrow=64,byrow=T))
#-------------------------------------------------------------------
#Add the average face
#up left = original
#up right = average face
#lower left = reconstructed from eigen space
#lower right = reconstructed + average face
par(mfrow=c(2,2))
par(mar=c(1,1,1,1))

# 1st person 1st photo
plt_img(matrix(as.numeric(df[1, ]), nrow=64, byrow=T))

# average face
average_face=colMeans(df)
AVF=matrix(average_face,nrow=1,byrow=T)
plt_img(matrix(average_face,nrow=64,byrow=T))

# project into eigen space and back
PF1 <- data.matrix(df[1,]) %*% eigenvectors
RE1 <- PF1 %*% t(eigenvectors)
plt_img(matrix(as.numeric(RE1),nrow=64,byrow=T))

# add the average face
RE1AVF=RE1+AVF
plt_img(matrix(as.numeric(RE1AVF),nrow=64,byrow=T))

par(mfrow=c(2,2))
par(mar=c(1,1,1,1))

# 3rd person 31st photo
plt_img(matrix(as.numeric(df[31, ]), nrow=64, byrow=T))

# average face
average_face=colMeans(df)
AVF=matrix(average_face,nrow=1,byrow=T)
plt_img(matrix(average_face,nrow=64,byrow=T))

# project into eigen space and back
PF1 <- data.matrix(df[31,]) %*% eigenvectors
RE1 <- PF1 %*% t(eigenvectors)
plt_img(matrix(as.numeric(RE1),nrow=64,byrow=T))

# add the average face
RE1AVF=RE1+AVF
plt_img(matrix(as.numeric(RE1AVF),nrow=64,byrow=T))
#--------------------------------------------------------
#Simple Classifier based on the Euclidean distance
#Finding the minimum euclidean distance to classify the photos

# New photo under test, say, 142nd photo
# Transform onto eigen space to find the coefficients
PF1 <- data.matrix(df[142,]) %*% eigenvectors

# Transform all the traning photos onto eigen space and get the coefficients
PFall <- data.matrix(df) %*% eigenvectors

# Find the simple difference and multiplied by itself to avoid negative value
test <- matrix(rep(1,400),nrow=400,byrow=T)
test_PF1 <- test %*% PF1
Diff <- PFall-test_PF1
y <- (rowSums(Diff)*rowSums(Diff))

# Find the minimum number to match the photo in the files
x=c(1:400)
newdf=data.frame(cbind(x,y))

the_number = newdf$x[newdf$y == min(newdf$y)]

par(mfrow=c(1,1))
par(mar=c(1,1,1,1))
barplot(y,main = "Similarity Plot: 0 = Most Similar")
cat("the minimum number occurs at row = ", the_number)
## the minimum number occurs at row =  142
plt_img(matrix(as.numeric(df[the_number, ]), nrow=64, byrow=T))
cat("The photo match the number#",the_number,"photo in the files")
## The photo match the number# 142 photo in the files
