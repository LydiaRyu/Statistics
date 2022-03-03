
#install.packages("readxl")
library(readxl)

COVID<-read_excel("COVID19_data.xlsx")
head(COVID)

# calculate prior distribution, likelihood and posterior distribution
mu=223
sig=15
a1=mu^2/sig^2
b1=mu/sig^2
y=COVID[,2]
n=nrow(y)
ss=sum(y)
aa=a1+ss
bb=b1+n
mm=aa/bb
sd=sqrt(aa/bb^2)
print(round(cbind(a1,b1,n,ss,aa,bb,mm,sd),3))

# Tri-plot
xa=seq(150,350,10)
plot(xa,dgamma(xa,shape = a1, rate = b1), type = "l", lty=2, xlab = "data", ylab = "")
title("Poisson-Gamma Distribution")
lines(xa,dgamma(xa, shape = 221.018, rate = 22603.24),type = "l", lty=3, xlab="", ylab="")
lines(xa,dgamma(xa, shape = 7990.018, rate = 30.991),type = "l",  xlab="", ylab="")
legend("topright", c("prior","likelihood","posterior"), lty=c(2,3,1))

# Highest posterior density interval(95%)
la=qgamma(c(0.025,0.975),7990.018,30.991)
lb=qgamma(c(0.025,0.975), 221.018, 22603.24 )

plot(xa,dgamma(xa,shape = a1, rate = b1), type = "l", lty=2, xlab = "data", ylab = "")
abline(v=c(la[1],la[2]), lty=1)
abline(v=c(lb[1], lb[2]), lty=2)
legend("topright", c("Bayesian", "Classic"), lty=c(1,2))
title("Gamma Dist. : 95% HPD")

# Predictive distribution
y=COVID[,2]
a=a1
b=b1
n=nrow(y)
s=sum(y)
xx=c(200:325)
pp1<-dnbinom(xx,size = a+s, prob = (b+n)/(b+n+1))
plot(xx,pp1,type = "h", ylab = "Density")
title("True Prediction by nbinom")


N=10000
theta=rgamma(N,a+s,b+n)
pp2<-rpois(N,theta)
plot(table(pp2)/N,type = "h", xlab = "z", ylab = "Density")
title("Prediction by Monte Carlo")
print(paste(c("mean="),c(sum(xx*pp1))))
print(paste(c("mean="),c(mean(pp2)),c("variance="),c(var(pp2))))

#install.packages("MCMCpack")
library(MCMCpack)

yy=rpois(30, lambda = 7769/30)
posterior<-MCpoissongamma(yy, alpha = 221.018, beta = 0.991, mc=5000)
summary(posterior)

plot(posterior)
xx<-seq(200,300,10)
plot(xx,dgamma(xx,221.018,0.991), type = "l", lty=2, ylim = c(0,0.15), 
     xlab = "mu", ylab = "density")
title("Monte Carlo Simulation : Gamma Prior")
lines(density(posterior), lty=1)
legend("topright", c("prior", "posterior"), lty=c(2,1))

