# 1. 사과, 베이컨, 바나나, 당근, 셀러리, 치즈, 토마토 데이터를 준비한다. x축은 단맛, y축은 아삭한 정도

c <- c(10,9,1,4,10,1,7,10,3,10,1,1,6,7)
row <- c("APPLE","BACON","BANANA","CARROT","SAL","CHEESE","TOMATO")
col <- c("X","Y")
data <- matrix( c, nrow= 7, ncol=2, byrow=TRUE, dimnames=list(row,col))
data

plot(data)

# 2. 야채, 과일, 단백질 3가지를 k-means 가 잘 분류 했는지 시긱화 해서 확인한다. 

#install.packages("stats")
#library(stats)

km <- kmeans(data,  3) 
km
cbind(data, km$cluster)

plot(round(km$center), col=km$center, pch=22,  bg=km$center, xlim=range(0:10),ylim=range(0:10))
par(new=T)  # 그래프 겹치기 
plot( data, col=km$cluster+1, xlim=range(0:10), ylim=range(0:10), pch=22, bg=km$cluster+1 )

# 3. 야채, 과일, 단백질 3가지를 k-means 가 잘 분류 했는지 시각화 해서 확인한다. 

#install.packages("factoextra")
#library(factoextra)

km <- kmeans(data,3)
fviz_cluster( km, data = data, stand=F)

