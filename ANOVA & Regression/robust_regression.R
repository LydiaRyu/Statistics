

heart.data <- read.csv("heart.csv")
head(heart.data)

dim(heart.data)

pairs(~heart.disease + biking + smoking, data = heart.data, 
      main = "The scatter plots of the heart.data")

model.one <- lm(heart.disease ~ ., data = heart.data)
summary(model.one)

# install.packages("MASS")
library(MASS)

model.two <- rlm(heart.disease ~ ., data = heart.data)
summary(model.two)

# P-value of the robust regression
# install.packages("sfsmisc")
library(sfsmisc)

f.robftest(model.two, var = "smoking")

# install.packages("quantreg")
library(quantreg)

model.three <- rq(heart.disease ~ ., tau = 0.5 ,data = heart.data)
model.three$coefficients

model.one$coefficients

model.two$coefficients

model.three$coefficients

boxplot(model.one$residuals, main = "Residuals of model.one")

q1 <- quantile(model.one$residuals, probs = 0.25)
q3 <- quantile(model.one$residuals, probs = 0.75)

iqr <- q3 - q1

# Find the values above q3.
outliers_q3 <- q3 + 1.5 * iqr <= model.one$residuals

model.one$residuals[which(outliers_q3, arr.ind = TRUE)]

heart.data.modi <- heart.data[-c(2, 9, 18), ]
dim(heart.data.modi)

model.four <- lm(heart.disease ~ ., data = heart.data.modi)
summary(model.four)

model.one$coefficients[3]

model.four$coefficients[3]


