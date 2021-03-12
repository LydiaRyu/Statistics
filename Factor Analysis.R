#install.packages("sas7bdat")
library(sas7bdat)

my.data<-read.sas7bdat("nmtwins.sas7bdat")

head(my.data)
dim(my.data)

# Remove the first to third rows.
my.data<-my.data[,4:11]
dim(my.data)

# Number of rows with missing values
length(which(apply(my.data,1,mean)=="NaN"))

#Remove rows with missing values
NaN.row <- which(apply(my.data,1,mean)=="NaN")
my.data2<-my.data[-NaN.row,]
dim(my.data2)

# Kaiser Rule
eigen(cor(my.data2))$value

# Factor Analysis
my.model<-factanal(my.data2, factors = 2, method="mle")
my.model

# Factor loadings
my.model$loadings

# Factor Score
scores<- factanal(my.data2, factors = 2, method = "mle",scores = "regression")$scores

plot(scores[1:60,1],scores[1:60,2],pch=".",xlab="factor 1", ylab="factor 2", main="Factor scoring")
text(scores[1:60,1],scores[1:60,2])
abline(v=0,h=0,lwd=2)

# Promax
promax(loadings(my.model))


pro.load<- promax(loadings(my.model))$loadings[] 

plot(pro.load[,1],pro.load[,2],xlab="factor 1", ylab="factor 2", main="Promax Rotation")
text(x=pro.load[,1]+0.03,y=pro.load[,2])
abline(v=0,h=0,lwd=2)
