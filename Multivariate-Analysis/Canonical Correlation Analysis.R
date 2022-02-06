
setwd("D:/EUN/Multivariate Data Analysis")
load("mydata2.RData")

head(my.data2)
dim(my.data2)


#install.packages("CCA")
library(CCA)

x<-my.data2[,1:3]
y<-my.data2[,4:8]

matcor_xy<-matcor(x,y)

cor(my.data2)

# Canonical Correlation Analysis
my.result<-cc(my.data2[,1:3],my.data2[,4:8])

# Scree plot
plot(my.result$cor, type = 'b')

## From the second point, the slope is sharply bent, and the number of canonical variables is one.


# Canonical Coefficient
my.result$xcoef
my.result$ycoef


# Standardization
x.std<-scale(my.data2[,1:3], center = TRUE, scale = TRUE)
y.std<-scale(my.data2[,4:8], center = TRUE, scale = TRUE)

# Standardized Canonical Correlation Analysis
my.result2<-cc(x.std,y.std)

# Standardized Canonical Coefficient
my.result2$xcoef[,1]
my.result2$ycoef[,1]


## v=-0.1122482moed -0.4162026faed -0.2675276faminc

## w=-0.01266545english -0.08824282math + 0.02126201socsci + 0.03687454natsci-0.16235915vocab



