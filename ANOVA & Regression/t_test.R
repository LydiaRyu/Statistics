
parent.data <- read.csv("parent.csv")
head(parent.data)
dim(parent.data)

attach(parent.data)

# 등분산 검정(F-test)

is.numeric(group)
factor(group)

var.test(score ~ group)


# F test to compare two variances
# 
# data:  score by group
# F = 0.93031, num df = 7, denom df = 9, p-value = 0.9459
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.2216591 4.4871044
# sample estimates:
#   ratio of variances 
# 0.9303136 


# H0: sigma1 / sigma2 = 1 vs H1: singma1/ sigma2 not equal to 1
# p-value is 0.9459 so, cannot reject 

# 등분산 검정(Bartlett test)

bartlett.test(score ~ group)


# Bartlett test of homogeneity of variances
# 
# data:  score by group
# Bartlett's K-squared = 0.009625, df = 1, p-value = 0.9218

# T-test

t.test(score ~ group)

# Welch Two Sample t-test
# 
# data:  score by group
# t = 3.926, df = 15.373, p-value = 0.001291
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   6.18632 20.81368
# sample estimates:
#   mean in group 1 mean in group 2 
# 84.5            71.0 

t.test(score ~ group, var.equal = TRUE)

# Two Sample t-test
# 
# data:  score by group
# t = 3.9094, df = 16, p-value = 0.001249
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   6.179417 20.820583
# sample estimates:
#   mean in group 1 mean in group 2 
# 84.5            71.0 

# H0: mu = 80 #
# One sample t-test
t.test(score, mu = 80) 


# One Sample t-test
# 
# data:  score
# t = -1.2888, df = 17, p-value = 0.2147
# alternative hypothesis: true mean is not equal to 80
# 95 percent confidence interval:
#   72.08893 81.91107
# sample estimates:
#   mean of x 
# 77 
