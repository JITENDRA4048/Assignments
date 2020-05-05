crimedata <- read.csv(file.choose())
View(crimedata)
crmdt <- scale(crimedata[,2:5])


###Compute the distance matrix
dstmtrx <- dist(crmdt,method = "euclidean")
dstmtrx
##Building the algorithm
fit <- hclust(dstmtrx,method = "average") ###can try centroid too
### Display dendogram
plot(fit)
####Scree plot
wss <- c()
for(i in 2:15) wss[i] <- sum(kmeans(crmdt,centers = i)$withinss)
plot(1:15,wss,type = "b",xlab = "no of clusters",ylab = "Avg Distance")

groups <- cutree(fit,k=5)
## Cut the dendogram with colored borders & 3clusters
rect.hclust(fit, k=5, border="red",plot(fit))
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



