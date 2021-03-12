

load("Forbes.RData")
head(Forbes1995)

# Correlation Matrix
cor(Forbes1995[,5:8])

# Fitting PCA model
PCA.model.cor<-prcomp(Forbes1995[,5:8], center = TRUE, scale. = TRUE)
PCA.model.cor

# Summary
summary(PCA.model.cor)

## If we want a cumulative contribution rate of more than 80%, we must select three principal components. Based on the output, the third cumulative contribution rate is 0.9197. which exceeds 80 percent.This means that the third principal component accounts for about 92% of the four variables. 


# Biplot
biplot(PCA.model.cor)


PCA.model.cov<-prcomp(Forbes1995[,5:8], center = TRUE, scale. = FALSE)
PCA.model.cov

# To check for zero when orthogonal

sum(PCA.model.cov$rotation[,1]*PCA.model.cov$rotation[,2])
sum(PCA.model.cov$rotation[,1]*PCA.model.cov$rotation[,3])
sum(PCA.model.cov$rotation[,1]*PCA.model.cov$rotation[,4])
sum(PCA.model.cov$rotation[,2]*PCA.model.cov$rotation[,3])
sum(PCA.model.cov$rotation[,2]*PCA.model.cov$rotation[,4])
sum(PCA.model.cov$rotation[,3]*PCA.model.cov$rotation[,4])


#To check that the unit eigenvector is 1

sum(PCA.model.cov$rotation[,1]^2)
sum(PCA.model.cov$rotation[,2]^2)
sum(PCA.model.cov$rotation[,3]^2)
sum(PCA.model.cov$rotation[,4]^2)

# Eigenvector(C)

# Eigenvalue(B)

PCA.model.cov$sdev^2

# Make B a Matrix  

diag(PCA.model.cov$sdev^2)

# Calculate CBC'

PCA.model.cov$rotation%*%diag(PCA.model.cov$sdev^2)%*%t(PCA.model.cov$rotation)

# Compare with covariance matrix (A)

cov(Forbes1995[,5:8])




