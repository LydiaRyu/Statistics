

ad_data <- read.csv("advertisement.csv")
attach(ad_data)
ad_data


# Oneway test
oneway.test(News ~ factor(Day))

oneway.test(News ~ factor(Day), var.equal = T)


# pairwise t-test
pairwise.t.test(News, factor(Day), p.adjust = "bonferroni", pool.sd = T)


lm_ad <- lm(News~factor(Day))
summary.aov(lm_ad)
summary(lm_ad)

ad_data2 <- read.csv("advertisement2.csv")
attach(ad_data2)
ad_data2

twowayout_ad <- lm(Counts ~ factor(Day) + factor(Section))
summary.aov(twowayout_ad)
summary(twowayout_ad)

