
setwd("D:/EUN/Multivariate Data Analysis")
load("Fuel.RData")
head(data.mpg)

# Kaiser Rules
PCA.model.cor<-prcomp(data.mpg[,], center = TRUE, scale. = TRUE)
PCA.model.cor


PCA.model.cor$sdev^2

# contribution Chart
100*PCA.model.cor$sdev^2/sum(PCA.model.cor$sdev^2)

# Screeplot
PCA.model.cov<-prcomp(data.mpg[,], center = TRUE, scale. = FALSE)
PCA.model.cov

screeplot(PCA.model.cov, type = "lines", lwd=2)

# First principal component
PCA.model.cor$rotation[,1]

## PC1 = (-0.612mpg+0.552engine+0.392hp+0.407weight)

# Biplot
biplot(PCA.model.cor)
