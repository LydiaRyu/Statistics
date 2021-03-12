# Chi-Square test
right_left<-matrix(c(923,113,20,1070,92,8), ncol = 2, nrow = 3)
right_left

chisq.test(right_left, correct = FALSE)

# Fisher's Exact test
tea<-matrix(c(3,1,1,3), ncol = 2, nrow = 2)
tea

fisher.test(tea, alternative = "greater")


