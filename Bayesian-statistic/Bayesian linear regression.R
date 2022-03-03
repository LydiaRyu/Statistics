
# bayesian linear regression in Jags library

x1<-rep(1,12); x2<-c(0,0,0,0,0,0,1,1,1,1,1,1)
x3<-c(23,22,22,25,27,20,31,23,27,28,22,24);
x4<-x2*x3

y<-c(-0.87,-10.74,-3.27,-1.97,7.50,-7.25,17.05,4.96,10.40,11.05,0.26,2.51)
plot(y~x3,pch=16,xlab="age",ylab="change in maximal oxygen uptake",
col=c("black","gray")[x2+1]);
legend(27,0,legend=c("aerobic","running"),
pch=c(16,16),col=c("gray","black"))


n<-length(y); X<-cbind(x1,x2,x3,x4); p<-dim(X)[2]
beta.ols<- solve(t(X)%*%X)%*%t(X)%*%y
fit.ls<-lm(y~-1+ X)

summary(fit.ls)

#install.packages("rjags")
library(rjags)
jags_code ="model{
for(i in 1:n) {
Y[i] ~ dnorm(mu[i] , tau)
mu[i] <- inprod(beta[],X[i,])
}
tau ~ dgamma(nu0/2, nu0*s20/2)
sigma <- 1/sqrt(tau)
beta[1:p] ~ dmnorm(beta0[1:p], tau*(t(X)%*%X)/g) # precision
}"


jags_data = list(X=X, Y=y, n=nrow(X), p=ncol(X),
g=nrow(X), nu0=1, s20=8.54, beta0=c(0,0,0,0))
jags_model = jags.model(textConnection(jags_code), data=jags_data)


update(jags_model, 10000)
samp <- coda.samples(jags_model,variable.names=c("beta","sigma"),
n.iter=5000)
summary(samp)


plot(samp)


