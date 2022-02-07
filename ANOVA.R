setwd("D:/EUN/KU/2021_2/Research Methods2/CH1")
parent.data <- read.csv("parent.csv")

install.packages("BSDA")
library(BSDA)

# Sign test 

? SIGN.test
SIGN.test(score, md = 80)


# One-sample Sign-Test
# 
# data:  score
# s = 7, p-value = 0.6291
# alternative hypothesis: true median is not equal to 80
# 95 percent confidence interval:
#   70 84
# sample estimates:
#   median of x 
# 76.5 
# 
# Achieved and Interpolated Confidence Intervals: 
#   
#   Conf.Level L.E.pt U.E.pt
# Lower Achieved CI     0.9037     70     84
# Interpolated CI       0.9500     70     84
# Upper Achieved CI     0.9691     70     84

score > 80

# [1] FALSE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
# [16] FALSE FALSE FALSE


# Wilcoxon sign rank test

wilcox.test(score, mu = 70) #mu is median


# Wilcoxon signed rank test with continuity correction
# 
# data:  score
# V = 116, p-value = 0.01399
# alternative hypothesis: true location is not equal to 70
# 
# Warning messages:
#   1: In wilcox.test.default(score, mu = 70) :
#   cannot compute exact p-value with ties
# 2: In wilcox.test.default(score, mu = 70) :
#   cannot compute exact p-value with zeroes


wilcox.test(score + rnorm(length(score), 0, 0.1), mu = 70)

# Wilcoxon signed rank test
# 
# data:  score + rnorm(length(score), 0, 0.1)
# V = 140, p-value = 0.01593
# alternative hypothesis: true location is not equal to 70


# One-way ANOVA

learning <- read.csv("learning.csv")
learning
attach(learning)


boxplot(score ~ factor(method))

oneway.test(score ~ factor(method))

# One-way analysis of means (not assuming equal variances)
# 
# data:  score and factor(method)
# F = 7.4757, num df = 3.000, denom df = 10.953, p-value = 0.005353

oneway.test(score ~ factor(method), var.equal = T)

# One-way analysis of means
# 
# data:  score and factor(method)
# F = 5.4063, num df = 3, denom df = 20, p-value = 0.006876

# P-value
1 - pf(5.4063, 3, 20)

aov.out <- aov(score ~ factor(method))
summary(aov.out)


pairwise.t.test(score, factor(method), p.adjust = "non", pool.sd = T)

# Pairwise comparisons using t tests with pooled SD 
# 
# data:  score and factor(method) 
# 
# 1       2       3      
# 2 0.03649 -       -      
#   3 0.49829 0.13637 -      
#   4 0.10008 0.00076 0.02548
# 
# P value adjustment method: none 

pairwise.t.test(score, factor(method), p.adjust = "bonferroni", pool.sd = T)

# Pairwise comparisons using t tests with pooled SD 
# 
# data:  score and factor(method) 
# 
# 1      2      3     
# 2 0.2189 -      -     
#   3 1.0000 0.8182 -     
#   4 0.6005 0.0046 0.1529
# 
# P value adjustment method: bonferroni 

pairwise.t.test(score, factor(method), p.adjust = "BH", pool.sd = T)

# Pairwise comparisons using t tests with pooled SD 
# 
# data:  score and factor(method) 
# 
# 1      2      3     
# 2 0.0730 -      -     
#   3 0.4983 0.1636 -     
#   4 0.1501 0.0046 0.0730
# 
# P value adjustment method: BH


rbdex <- read.csv("rbdex.csv")
attach(rbdex)
head(rbdex)


par(mfrow=c(1, 2))
plot(pctloss~factor(blend) + factor(block))

lm(pctloss~factor(blend) + factor(block))
# Call:
#   lm(formula = pctloss ~ factor(blend) + factor(block))
# 
# Coefficients:
#   (Intercept)  factor(blend)B  factor(blend)C  factor(blend)D  factor(blend)E  factor(block)2  
# 16.5267          1.2000          0.5000         -0.6667          1.8333          0.5200  
# factor(block)3  
# 0.8000  

twowayout <- lm(pctloss ~ factor(blend) + factor(block))
summary.aov(twowayout)


# Df Sum Sq Mean Sq F value Pr(>F)  
# factor(blend)  4 11.556   2.889   3.305 0.0705 .
# factor(block)  2  1.648   0.824   0.943 0.4289  
# Residuals      8  6.992   0.874                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



