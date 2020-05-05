crimedata <- read.csv(file.choose())
View(crimedata)
crmdt <- scale(crimedata[,2:5])
plot(crmdt)

####----hierarchical clustering---####
###Compute the distance matrix
dstmtrx <- dist(crmdt,method = "euclidean")
dstmtrx
##Building the algorithm
fit <- hclust(dstmtrx,method = "average") ###can try centroid too
### Display dendogram
plot(fit)
### The hierarchical clustering plot is indistinguishable so we must apply k-means

##K-means clustring

####Scree plot
wss <- c()
for(i in 2:20) wss[i] <- sum(kmeans(crmdt,centers = i)$withinss)
plot(1:20,wss,type = "b",xlab = "no of clusters",ylab = "Avg Distance")
###There is a drastic fall till 5th cluster and 8th cluster
#### After which the plot becomes nearly flat so take k=5 or 8

k.crmdt <- kmeans(crmdt,centers = 5) #Do kmean clustring with 5 centres
k.crmdt$centers ##check the centers of clusters
k.crmdt$cluster ##show the clusters
plot(crmdt[k.crmdt$cluster==1,],
     col="red",
     xlim = c(min(crmdt[,1]),max(crmdt[,1])),
     ylim = c(min(crmdt[,2]),max(crmdt[,2]))
     )
points(crmdt[k.crmdt$cluster==2,],col='blue')
points(crmdt[k.crmdt$cluster==3,],col='gray0')
points(crmdt[k.crmdt$cluster==4,],col='cyan4')
points(crmdt[k.crmdt$cluster==5,],col='chocolate4')

groups <- cutree(fit,k=5)
###plot the centres on the plot
points(k.crmdt$centers,pch=20,col="chartreuse")
## Cut the dendogram with colored borders & 3clusters
rect.hclust(fit, k=5, border="red",plot(fit))


k.crmdt <- kmeans(crmdt,centers = 8) #Do kmean clustring with 5 centres
k.crmdt$centers ##check the centers of clusters
k.crmdt$cluster ##show the clusters
plot(crmdt[k.crmdt$cluster==1,],
     col="red",
     xlim = c(min(crmdt[,1]),max(crmdt[,1])),
     ylim = c(min(crmdt[,2]),max(crmdt[,2]))
)
points(crmdt[k.crmdt$cluster==2,],col='blue')
points(crmdt[k.crmdt$cluster==3,],col='gray0')
points(crmdt[k.crmdt$cluster==4,],col='cyan4')
points(crmdt[k.crmdt$cluster==5,],col='chocolate4')
points(crmdt[k.crmdt$cluster==5,],col='darkred')
points(crmdt[k.crmdt$cluster==5,],col='deepskyblue3')
points(crmdt[k.crmdt$cluster==5,],col='goldenrod')
###plot the centres on the plot
points(k.crmdt$centers,pch=20,col="chartreuse")


groups <- cutree(k.crmdt,k=8)
###plot the centres on the plot
points(k.crmdt$centers,pch=20,col="chartreuse")
## Attach cluster no to city name
clusters=data.frame('city'=crimedata[,1],'cluster' =groups)
View(clusters)
result <- as.matrix(groups)
final <- data.frame('city'=crimedata[,1],result)
final
#### Clustring Animations
install.packages("animation")
library(animation)
windows()
km <- kmeans.ani(crmdt,5)

windows()
km <- kmeans.ani(crmdt,8)

