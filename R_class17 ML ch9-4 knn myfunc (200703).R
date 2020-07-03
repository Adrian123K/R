myknn <- function(train, test, cl, k) {
  pred <- c()
  for (i in 1:nrow(test)) {
    temp <- t(t(train)-c(t(test[i,])))^2         
    temp <- sqrt(rowSums(temp))
    table <- data.frame(train, kind=cl, d=temp)
    table$rnk <- rank(table$d, ties.method = 'min')
    top <- table[table$rnk <= k,'kind']
    pred <- append(pred, names(which.max(table(top))))
  }
  return(pred)
}

# 1. 유방암 데이터를 로드 한다.
wbcd <-  read.csv("wisc_bc_data.csv", header=T,  stringsAsFactors=FALSE)

# 2. diagnosis 를 factor 로 변환한다
wbcd$diagnosis <- factor(wbcd$diagnosis,
                         levels =c("B","M"),
                         labels = c("Benign","Maliganant"))

# 3. 데이터를 shuffle 시킨다.
wbcd_shuffle <- wbcd[sample(nrow(wbcd)), ]

# 4. 데이터에서 id 를 제외 시킨다
wbcd2 <- wbcd_shuffle[-1]

# 5. 데이터를 정규화 한다.
normalize <- function(x) {
  return ( (x-min(x)) / (max(x) - min(x))  )
}

wbcd_n  <- as.data.frame(lapply(wbcd2[2:31],normalize))

# 6. train 데이터와 test 데이터로 9 대 1로 나눈다
train_num<-round(0.9*nrow(wbcd_n),0)
wbcd_train<-wbcd_n[1:train_num,]
wbcd_test<-wbcd_n[(train_num+1):nrow(wbcd_n),]

# 7. train 데이터를 데이터와 라벨로 나누고 test  데이터를 데이터와 라벨로 나누시오
wbcd_train_label <- wbcd2[1:train_num,1]
wbcd_test_label <- wbcd2[(train_num+1):nrow(wbcd_n),1]

# 8. 직접 만든 my_knn 모델로 훈련시켜서 모델을 만들고 바로 그 모델에 test 데이터를 넣어서 정확도를 확인한다
result1 <- myknn(train=wbcd_train, test=wbcd_test, cl=wbcd_train_label, k=21)
result1

library(gmodels)
CrossTable(wbcd_test_label,result1)

a <- matrix(c(4,3,2,5,2,4,2,9,8,2,3,3,9,3,2,4,3,2,1,2),nrow=5,byrow=T)
b <- matrix(c(3,4,3,2,2,1,3,2,3,2,3,2,1,2,2,1,2,1,4,3),nrow=5,byrow=T)
a-b

c <- matrix(c(3,4,3,2),nrow=1,byrow=T)
c
rs <- matrix(c(rep(0,20)),nrow=5)

for (i in 1:nrow(a)){
  for (j in 1:ncol(a)){
    rs[i,j] <- a[i,j]-c[j]
  }
}
rs

a <- matrix(c(4,3,2,5,2,4,2,9,8,2,3,3,9,3,2,4,3,2,1,2),nrow=5,byrow=T)
c <- matrix(c(3,4,3,2),nrow=1,byrow=T)
rs <- c()
for (i in 1:nrow(a)){
  rbind(rs,a[i]-c)
}
rs
