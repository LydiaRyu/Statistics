

data<-read.csv("wdbc.csv") 
y.train <- data[1:450,1]
x.train <- data[1:450,-1]
y.test <- data[-c(1:450),1]
x.test <- data[-c(1:450),-1]

library(dplyr)

# M =1 , B = 0

y.train <- ifelse(y.train == 'M', 1, 0)
y.test <- ifelse(y.test == 'M', 1, 0)


model_glm <- glm(factor(y.train) ~ ., family =  binomial("logit") ,data = x.train)

dim(x.train)

# install.packages("kernlab")
library(kernlab)

C.try = seq(0.005, 3, 0.05)
i = 0
res_cross <- c()

for (c in C.try){
  model_LSVM_cv <- ksvm(y.train~. , data = x.train, kernel = 'vanilladot', type = 'C-svc', 
                        cross = 5, C= c)
  
  i <- i + 1
  res_cross[i] <- cross(model_LSVM_cv)
}


C.opt <- C.try[which.min(res_cross)]
C.opt

# Fit the training data for Linear SVM

model_LSVM <- ksvm(y.train~. , data = x.train, kernel = 'vanilladot', type = 'C-svc',
                   C= C.opt)

model_LSVM

sigma.try = seq(0.0001, 0.05, length.out = 50)
i = 0
res_GRSVM_cross <- c()

for (s in sigma.try){
  model_GRSVM_cv <- ksvm(y.train~. , data = x.train, kernel = 'rbfdot',
                         kpar = list(sigma = s), type = 'C-svc', cross = 5)
  i <- i + 1
  res_GRSVM_cross[i] <- cross(model_GRSVM_cv)
}

sigma.opt <- sigma.try[which.min(res_cross)]
sigma.opt

# Fit the training data for Gaussian/Radial  kernel  SVM

model_GRSVM <- ksvm(y.train~. , data = x.train, kernel = 'rbfdot', type = 'C-svc', 
                    kpar = list(sigma = sigma.opt))

model_GRSVM

library(forecast)

# Logistic Regression

pred_glm <- predict(model_glm , newdata = x.test, type = 'response')
hat.y_GLM <- ifelse(pred_glm>0.5,1,0)

table(hat.y_GLM, y.test)
accuracy(hat.y_GLM, y.test)

# Linear SVM

hat.y_LSVM <- predict(model_LSVM, x.test)

table(hat.y_LSVM, y.test)
accuracy(hat.y_LSVM, y.test)

# Gaussian/Radial  kernel  SVM

hat.y_GRSVM <- predict(model_GRSVM, x.test)

table(hat.y_GRSVM, y.test)
accuracy(hat.y_GRSVM, y.test)
