
df <- read.csv("data.csv", header = TRUE)
head(df)

df$SES <- factor(df$SES)

fit <- glm(Impair ~ SES + Life, data = df, family = binomial("logit"))
summary(fit)

exp(fit$coefficients)

anova(fit, test = "Chisq")

fitted(fit)

predict(fit, newdata = df[c(21, 22),], type = "response")

fit0 = glm(Impair ~ 1, family=binomial, data=df)
anova(fit0, fit, test="Chisq")

fit_step = step(fit, direction="both")
summary(fit_step)

library(ROCR)
pred <- prediction(fitted(fit), df$Impair)
perf <- performance(pred, "tpr", "fpr")
plot(perf, main = "ROC Curve")

AUC <- performance(pred, "auc")@y.values
AUC
