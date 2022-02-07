# Relative Risk analysis with secondhandsmoking dataset
#install.packages("readxl")
library(readxl)

smoking<-read_excel("D:/EUN/secondhandsmoking.xlsx")

fisher<-smoking[,3]
w<- -2*sum(log(fisher))
w

pvalue<-1-pchisq(w,38)
pvalue

logr<-log(smoking$risk)

r_mean<-(smoking$lower+smoking$upper)/2
r_sigma_lower<-((r_mean-smoking$lower)*sqrt(19))/1.96
r_sigma_upper<-((smoking$upper-r_mean)*sqrt(19))/1.96

var_logr<-r_sigma_lower^2*(1/r_mean^2)

psi<-sum(1/var_logr*logr)/sum(1/var_logr)
psi

-----------------------------------------------------------------------------------

# Relative Risk analysis with survive dataset
install.packages("readxl")
library(readxl)

survive<-read_excel("D:/EUN/survive.xlsx")

surv_survive<-surv(time=survive$Time, event=survive$Event)
?surv

install.packages("survival")
library(survival)

surv_fit<-survfit(Surv(survive$Time, survive$Event)~1)
summary(surv_fit)
plot(surv_fit)

logr<-log(collins[1:9,]$trt_pos/collins[1:9,]$trt_num/collins[1:9,]$con_pos/collins[1:9,]$con_num)

pnorm(-4.15714)