
A<-c(0.7,-1.6,-0.2,-1.2,-0.1,3.4,3.7,0.8,0.0,2.0)
B<-c(1.9,0.8,1.1,0.1,-0.1,4.4,5.5,1.6,4.6,3.4)

#Homogeneity of variance test
var.test(A,B)

# T-test
t.test(A,B,var.equal = T, alternative = "less")

mean(A)
mean(B)

sd(A)
sd(B)

m<-10
n<-10

sp<-sqrt(1/(m+n-2)*((m-1)*sd(B)^2+(n-1)*sd(A)^2))

# t(18,0.05)=1.734
(mean(B)-mean(A))-1.734*sp*sqrt(1/m+1/n)

t.test(B,A,var.equal = T, conf.level = 0.90)

#wilcoxon rank-sum test
wilcox.test(A,B,alternative = "less", exact = T)

t.test(A, mu=0, alternative = "greater")

t.test(B, mu=0, alternative = "greater")

t.test(B,A,paired = T, conf.level = 0.90)

#wilcoxon signed rank test

wilcox.test(A,B, alternative = "less", paired=TRUE, exact = T)


