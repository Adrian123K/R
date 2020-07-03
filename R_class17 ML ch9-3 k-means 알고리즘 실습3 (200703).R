aca <- read.csv('academy.csv')
aca <- aca[,c(3,4)]
km <- kmeans(aca, 4)
cbind(aca[ ,c(1,3,4)], km$cluster) 

km

dev.new()
fviz_cluster(km, data=aca, stand=F)

aca <- read.csv('academy.csv')
aca <- aca[,c(3,4)]
km <- kmeans(aca, 9)
plot(round(km$center), col=km$center, pch=22,  bg=km$center, xlim=range(0:100),ylim=range(0:100))
par(new=T)  # 그래프 겹치기 
plot(aca, col=km$cluster+1, xlim=range(0:100), ylim=range(0:100), pch=22, bg=km$cluster+1 )
fviz_cluster(km, data=aca, stand=F)

library(class)
library(stats)

wbcd <- read.csv("wisc_bc_data.csv", header=T,  stringsAsFactors=FALSE)

wbcd_shuffle <- wbcd[sample(nrow(wbcd)), ]
wbcd2 <- wbcd_shuffle[-1]

normalize <- function(x) {
  return ( (x-min(x)) / (max(x) - min(x))  )
}

wbcd_n  <- as.data.frame(lapply(wbcd2[2:31],normalize))

train_num <- round(0.9*nrow(wbcd_n),0)
wbcd_train <- wbcd_n[1:train_num,]
wbcd_test <- wbcd_n[(train_num+1):nrow(wbcd_n),]

wbcd_train_labels <- wbcd2[1:train_num,1]
wbcd_test_labels <- wbcd2[(train_num+1):nrow(wbcd_n),1]


knn_rs <- knn(train=wbcd_train, test=wbcd_test, cl=wbcd_train_labels, k=51)
km_rs <- kmeans(wbcd_test,2)
km_rs

knn_rs2 <- ifelse(wbcd_test_labels=='M',2,1)
sum(knn_rs2==km_rs$cluster)
fviz_cluster(km_rs, data=, stand=F)
