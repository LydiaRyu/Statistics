
#install.packages("MASS")
library(MASS)
data(Pima.tr)
attach(Pima.tr)

head(Pima.tr)

# Prior probability
No<-length(which(Pima.tr$type=="No"))/200
No

Yes<-length(which(Pima.tr$type=="Yes"))/200
Yes

# Linear Discriminant Analysis
lda.model<-lda(type~.,data = Pima.tr,prior=c(0.66,0.34))
lda.model

# fitted value and confusion matrix
type.fit<-predict(lda.model)$class
table(type.fit,type)

(29+17)/200

# cross-validation
lda.model.cv<-lda(type~.,data = Pima.tr, CV=TRUE)
type.pred<-lda.model.cv$class
table(type.pred,type)

(31+18)/200

# Calculate other variables (without 'skin')
lda.model.cv.skin<-lda(type~ . -skin,data = Pima.tr, CV=TRUE)
type.pred.skin<-lda.model.cv.skin$class
table(type.pred.skin,type)
```

(30+18)/200

# Calculate other variables (without 'bp', skin')
lda.model.cv.bp<-lda(type~ . -skin -bp,data = Pima.tr, CV=TRUE)
type.pred.bp<-lda.model.cv.bp$class
table(type.pred.bp,type)

(30+18)/200

# # Calculate other variables (without 'bp', 'skin', 'age')
lda.model.cv.age<-lda(type~ . -skin -bp -age,data = Pima.tr, CV=TRUE)
type.pred.age<-lda.model.cv.age$class
table(type.pred.age,type)

(30+14)/200


# Decision Tree

#install.packages("ISLR")
#install.packages("tree")
library(ISLR)
library(tree)
attach(Carseats)

High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats, High)
Carseats.data<-Carseats[,-1]

head(Carseats.data)

Carseats.data<-Carseats.data[,-12]

car.tr=tree(High ~ .,data=Carseats.data)
summary(car.tr)

plot(car.tr, lwd=2)
text(car.tr, all=T, cex=0.6)

plot(prune.misclass(car.tr))

## Size=12 or Size=22

final.tr.1<-prune.misclass(car.tr, best = 12)
plot(final.tr.1, lwd=2)
text(final.tr.1, all=T, cex=0.8)

final.tr.2<-prune.misclass(car.tr, best = 22)
plot(final.tr.2, lwd=2)
text(final.tr.2, all=T, cex=0.6)
