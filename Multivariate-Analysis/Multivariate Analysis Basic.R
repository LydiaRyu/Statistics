
performance<-read.table(file="D:/EUN/performance.txt",header=T)
head(performance)

# mean/ variance/ sd
mean<-apply(performance, 2, mean)
mean

variance<-apply(performance, 2, var)
variance

standard_deviation<-apply(performance, 2, sd)
standard_deviation


# Histogram
hist(performance$X1)

# Box plot
boxplot(performance[,1:6], main="Boxplot of Performance")

pairs(performance[,1:6], main="Performance Scatterplot Matrix")

# Scatterplot Matrix
library(car)
scatterplotMatrix(performance[,1:6], main="ScatterplotMatrix of Performance")

# Correlation
cor(performance[,1:6])

# Covariance
cov(performance[,1:6])

# Dimension
dim(performance)


# Chernoff Face
#install.packages("aplpack")
library(aplpack)

faces(performance[1:6])


# Profile chart
#install.packages("profileR")
library(profileR)

profileplot(performance[,1:6])

# Save with R.data
perform_standard <- scale(performance[,1:6], center = TRUE, scale = TRUE)
save(perform_standard, file = "D:/EUN/perform_standard.RData")

  
  
