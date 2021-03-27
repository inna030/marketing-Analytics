set.seed(1990)

library(NbClust)
library(mclust)
library(gmodels)
data=read.csv(file='SelfStatedData.csv',row.names=1)
head(data)
data <- scale(data)
dist <- dist(data, method = "euclidean") #compute the distance between two clusters. Ward’s criterion which aims to minimize the total variance within-cluster.
as.matrix(dist)[1:5,1:5]
clust=hclust(dist, method = "ward.D2")#We obtain the dendogram below which can help us decide the number of clusters to retain. 
plot(clust)
h_cluster <- cutree(clust, 2)
rect.hclust(clust, k=2, border="red")
table(h_cluster)
hclust_summary = aggregate(data,by=list(h_cluster),FUN=mean)
hclust_summary
a=NbClust(data=data[,1:5], min.nc=2, max.nc=15, index="all", method="ward.D2")
nb_summary = aggregate(data,by=list(a),FUN=mean)
set.seed(1990)
car_Cluster3 <-kmeans(data, 3, iter.max=100,nstart=100)
car_Cluster3
a=NbClust(data=data[,1:5],min.nc=2, max.nc=15, index="all", method="kmeans")

set.seed(1990)
mclustBIC(data[,1:5],verbose=F)#find the optimal model
lca_clusters <- lca_clust$classification
lca_clust_1 <- Mclust(data[,1:5],verbose = FALSE, modelNames ="EEI") 
summary(lca_clust_1)
lca_clusters_1 <- lca_clust_1$classification
lca_clust_1_summary <-aggregate(data[,1:5],by=list(lca_clusters_1),FUN=mean) 
lca_clust_1_summary
