n1 <- c(4,4)
n2 <- c(8,4)
d <- sqrt(sum((n1-n2)^2))
d

c <- c(3,4,1,5,7,9,5,4,6,8,4,5,9,8,7,8,6,7,2,1)
row <- c("A","B","C","D","E","F","G","H","I","J")
col <- c("X","Y")
data <- matrix( c, nrow= 10, ncol=2, byrow=TRUE, dimnames=list(row,col))
data

plot(data)

#install.packages("stats")
library(stats)

km <- kmeans(data,2) 
km

km$centers
km$cluster

cbind(data, km$cluster)

plot(round(km$center), col=km$center, pch=22, bg=km$center, xlim=range(0:10),ylim=range(0:10))

par(new=T)
plot( data, col=km$cluster, xlim=range(0:10), ylim=range(0:10) )

#install.packages("factoextra")
library(factoextra)

fviz_cluster( km, data = data, stand=F)
