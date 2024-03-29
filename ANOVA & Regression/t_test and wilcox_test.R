##### Data Importing #####

ad.data <- read.csv("D:/EUN/advertisement.csv")

attach(ad.data)
head(ad.data)


# Ho: average(News) is equal to average(Business) vs Ha: average(News) is not equal to average(Business)

var.test(ad.data$News, ad.data$Business)

##### t-test #####

# We can reject Ho, because p-value(0.01836) is less than 0.05. 
# It means that the variance between News and Business is not the same.

t.test(ad.data$News, ad.data$Business, var.equal = FALSE)

# Assuming that two variances are not the same, p-value(0.8012) is more than 0.05. 
# So, we can't reject Ho. It means that the average between News and Business is the same.

t.test(ad.data$News, ad.data$Business, var.equal = TRUE, conf.level = 0.99)

# This is a similar result as when the two variances are not the same.
# Since the difference between the two means, the value 0, is included between the confidence intervals(-2.34, 1.94), we can't reject Ho.

par(mfrow = c(1,2))

hist(ad.data$News, main = "Histogram of News", xlab = "News")
hist(ad.data$Business, main = "Histogram of Business", xlab = "Business")

boxplot(ad.data$News, ad.data$Business, names = c("News", "Business"),
        main = "Boxplots of News and Business ")

##### wilcox.test #####

wilcox.test(ad.data$News, ad.data$Business)
# We can't reject Ho, because p-value(0.0.9021) is more than 0.05. 
# It means that the median between News and Business is the same.

set.seed(2021)
wilcox.test(ad.data$News  + rnorm(length(ad.data$News), 0, 0.1),
            ad.data$Business + rnorm(length(ad.data$Business), 0, 0.1))

set.seed(2022)
wilcox.test(ad.data$News  + rnorm(length(ad.data$News), 0, 0.1), 
            ad.data$Business + rnorm(length(ad.data$Business), 0, 0.1))

# We can't reject Ho. When considering tie, the results show that the median values of the two groups are the same.