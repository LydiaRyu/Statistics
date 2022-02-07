# Lorenz curve & Gini coefficient

income<-read.csv(file="D:/EUN/income_data.csv", header=T)
head(income)

# Mean and standard deviation of log income
log_grp1<-log(income$grp_1)
mean(log_grp1)
sd(log_grp1)


log_grp2<-log(income$grp_2)
mean(log_grp2)
sd(log_grp2)

## The means of the two variables are the same, but differ in their variances. The variance of 'grp_1' is 2.15 and the variance of "grp_2' is 0.54, indicating that the distribution of income of A is more widespread.
 

# The ratio of the top 10% to the bottom 10% quantile
quantile(income$grp_1, probs = c(0.1)) / quantile(income$grp_1, probs = c(0.9))
quantile(income$grp_2, probs = c(0.1)) / quantile(income$grp_2, probs = c(0.9))


# Lorenz Curve

library(dplyr)

income_sort<-income %>%
  mutate(grp_1_sort = sort(grp_1),
         grp_2_sort = sort(grp_2))



income_cum<-apply(income_sort[,3:4],2,cumsum)  

n1<-length(income_sort$grp_1_sort)
x1<-c(1:n1)/n1 *100
y1<-income_cum[,1]/sum(income_sort$grp_1_sort) * 100


n2<-length(income_sort$grp_2_sort)
x2<-c(1:n2)/n2 *100
y2<-income_cum[,2]/sum(income_sort$grp_2_sort) * 100


plot(x = x1, y = y1, type="l", ann=FALSE, col="blue")
lines(x=x1, y=x1)
polygon(x=x1, y=y1, density = 15, col="blue")
lines(x=x2, y=y2, col="red")
polygon(x=x2, y=y2, density = 15, col="red")
title(main = "Lorenz Curve", xlab = "Household(%)", ylab = "Income(%)")
legend(x=1, y=100, legend = c("grp_1", "grp_2"), fill =c("blue", "red"), density = c(50,50))

# Compare with using 'Lorez Curve function'

#install.packages("ineq")
library(ineq)     

plot(Lc(income$grp_1),col="blue")
par(new=TRUE)
plot(Lc(income$grp_2), col="red")


#Gini's coefficient

 gini_function<-function(x,y)
 {
     n<-length(x)
     i<-1:(n - 1)
     s<-sum((y[i] + y[i + 1])*(x[i + 1] - x[i])/2)
     s<-(5000 - s)/5000
     return(s)
 }

gini_function(x1,y1)
gini_function(x2,y2)


#Compare with using 'Gini's coefficient function'

#install.packages("reldist")
library(reldist)

gini(income$grp_1,  weights=rep(1,length=length(income$grp_1)))
gini(income$grp_2,  weights=rep(1,length=length(income$grp_2)))

