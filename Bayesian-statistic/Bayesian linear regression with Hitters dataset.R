# bayesian linear regression with Hitters dataset

#install.packages("ISLR")
library(ISLR)

dim(Hitters)
head(Hitters)

# EDA
#Data Cleansing

sum(is.na(Hitters))


Hitters<-na.omit(Hitters)

#install.packages("MASS")
library(MASS)
# Fit the full model 
full.model <- lm(Salary ~., data = Hitters)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "both", 
                      trace = FALSE)
summary(step.model)

#Plot

par(mfrow=c(2,2))
plot(step.model)


yy<-Hitters$Salary
xx<-cbind(Hitters$AtBat, Hitters$Hits, Hitters$Walks,
          Hitters$CAtBat, Hitters$CRuns, Hitters$CRBI, Hitters$CWalks,
          Hitters$DivisionW, Hitters$PutOuts, Hitters$Assists)


n<-length(yy); p<-dim(xx)[2]
beta.ols.2<- solve(t(xx)%*%xx)%*%t(xx)%*%yy
fit.ls.2<-lm(yy~-1+ xx)

summary(fit.ls.2)

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

jags_data = list(X=xx, Y=yy, n=nrow(xx), p=ncol(xx),
                 g=nrow(xx), nu0=1, s20=summary(lm(yy~-1+xx))$sigma^2, beta0=c(0,0,0,0,0,0,0,0,0,0))
jags_model = jags.model(textConnection(jags_code), data=jags_data)

update(jags_model, 10000)
samp <- coda.samples(jags_model,variable.names=c("beta","sigma"),
                     n.iter=5000)
summary(samp)

plot(samp)
