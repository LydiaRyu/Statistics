
setwd("D:/EUN/Multivariate Data Analysis")
load("mydata3.RData")
head(clust.data)

# Standardization
clust.std.data<-scale(clust.data, center = TRUE, scale = TRUE)
dim(clust.std.data)

# Dendrogram with minkowski distance
dist1<-dist(clust.std.data, method = "minkowski", p=1)
cust1<-hclust(dist1, method = "average")
plot(cust1)

dist2<-dist(clust.std.data, method = "minkowski", p=2)
cust2<-hclust(dist2, method = "centroid")
plot(cust2)


# Dendrogram with maxium distance
dist3<-dist(clust.std.data, method = "maximum")
cust3<-hclust(dist3, method = "ward.D2")
plot(cust3)

# K-means clustering
kmeans.clust<-kmeans(clust.data,centers = 4, iter.max = 10, nstart = 2)
plot(clust.data, col=kmeans.clust$cluster)
points(kmeans.clust$centers,col=1:3,pch=9)

#install.packages("factoextra")
#install.packages("ggplot2")
library(factoextra)
library(ggplot2)

fviz_cluster(kmeans.clust, data = clust.data)


