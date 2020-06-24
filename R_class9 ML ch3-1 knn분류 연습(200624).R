a<-c(0,3,2)
b<-c(2,0,0)
sqrt(sum((a-b)^2))

#215
dist<-function(a,b){
  sqrt(sum((a-b)^2))
}
a<-c(0,3,2)
b<-c(2,0,0)
dist(a,b)

#216
a <- c(1,5)
b <- c(2,6)
c <- c(4,5)
d <- c(5,2)
e <- c(6,3)
f <- c(1,7)

x <- c(1,2,4,5,6,1)
y <- c(5,6,5,2,3,7)

temp <- c()
for (i in 1:length(x)){
  temp <- append(temp,dist(c(x[i],y[i]),c(4,4)))
}
temp

min(temp)

#218
fruits <- data.frame(재료=c('사과','베이컨','당근','바나나','셀러리','치즈'),
                       단맛=c(10,1,10,7,3,1),
                       아삭한맛=c(9,4,1,10,10,1),
                       음식종류=c('과일','단백질','과일','채소','채소','단백질'))
토마토 <- c(6,4)
temp <- c()
dist<-function(a,b){
  return (sqrt(sum((a-b)^2)))
}
for (i in 1:length(fruits$재료)){
  temp <- append(temp,dist(c(fruits$단맛[i],fruits$아삭한맛[i]),토마토))
}
fruits$dist <- temp
min(fruits$dist)

library(dplyr)
fruits$rnk <- dense_rank(fruits$dist)
fruits

fruits[fruits$rnk<=3,'음식종류']

rs<-fruits[fruits$rnk<=3,'음식종류']
names(table(rs)[table(rs)==max(table(rs))])

