wbcd <- read.csv("wisc_bc_data.csv", header=T,  stringsAsFactors=FALSE)

table(wbcd$diagnosis)
# 라벨 column의 데이터 분포를 막대그래프로 시각화
head(wbcd)

library(outliers)
grubbs.flag <- function(x) {
  outliers <- NULL
  test <- x
  grubbs.result <- grubbs.test(test)
  pv <- grubbs.result$p.value
  while(pv < 0.05) {
    outliers <- c(outliers,as.numeric(strsplit(grubbs.result$alternative," ")[[1]][3]))
    test <- x[!x %in% outliers]
    grubbs.result <- grubbs.test(test)
    pv <- grubbs.result$p.value
  }
  return(data.frame(X=x,Outlier=(x %in% outliers)))
}

rs <- grubbs.flag(wbcd$radius_mean)
rs2 <- rs[rs$Outlier=='TRUE',]
rs2
length(rs2$Outlier)

#223
cn <- colnames(wbcd)[c(-1,-2)]
cn
for (i in cn){
  rs <- grubbs.flag(wbcd$i)
  rs2 <- rs[rs$Outlier=='TRUE',]
  print(rs2)
  #print(paste(i,'-->',length(rs2$Outlier)))
}

for (i in 4:length(colnames(wbcd))){
  a = grubbs.flag(wbcd[,colnames(wbcd)[i]])
  b = a[a$Outlier==TRUE,"Outlier"]
  print(paste(colnames(wbcd)[i],'-->',length(b)))
}

# 양성과 악성의 비율 확인
round(prop.table(table(wbcd$diagnosis))*100,digit=1)

#3. 데이터를 shuffle 시킨다.
# wbcd[sample(10),] # 1번~10번까지의 데이터가 섞여서 출력이됨
wbcd_shuffle <- wbcd[sample(nrow(wbcd)), ]
wbcd_shuffle

#4. 데이터에서 id 를 제외 시킨다
wbcd2 <- wbcd_shuffle[-1]
head(wbcd2)

#5. 데이터를 정규화 한다.
normalize <- function(x) {
  return ( (x-min(x)) / (max(x) - min(x))  )
}
# 서로 단위가 다른 데이터를 전부 0-1사이의 데이터로 맞춰준다

wbcd_n  <- as.data.frame(lapply(wbcd2[2:31],normalize))
summary(wbcd_n)

nrow(wbcd_n)
train_num <- round(0.9*nrow(wbcd_n),0)
wbcd_train <- wbcd_n[1:train_num,]
wbcd_test <- wbcd_n[(train_num+1):nrow(wbcd_n),]

wbcd_train_labels <- wbcd2[1:train_num,1]
wbcd_test_labels <- wbcd2[(train_num+1):nrow(wbcd_n),1]

rs <- knn(train=wbcd_train, test=wbcd_test, cl=wbcd_train_labels, k=51)
rs

x <- data.frame(실제=wbcd_test_labels, 예측=rs)
x <- data.frame(실제=wbcd_test_labels, 예측=rs)
table(x)

par(new=T)
g2 <- CrossTable(x=wbcd_test_labels, y=rs,chisq=F)
g2$prop.tbl[1]+g2$prop.tbl[4] # 정확도

temp<-c()
for ( i in 1:200 ) {
  if  ( i%%2 != 0  ) { 
    wbcd_test_pred <- knn(train=wbcd_train, test=wbcd_test,
                          cl = wbcd_train_labels,  k=i )
    g2 <- CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, chisq=FALSE)
    g3 <- g2$prop.tbl[1] + g2$prop.tbl[4]
    temp<-append(temp, g3 )
  }
}
temp
plot(temp, type='l', col='red')

#226
temp<-c()
for ( i in 1:200 ) {
  if  ( i%%2 != 0  ) { 
    wbcd_test_pred <- knn(train=wbcd_train, test=wbcd_test,
                          cl = wbcd_train_labels,  k=i )
    g2 <- CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, chisq=FALSE)
    g3 <- g2$prop.tbl[1] + g2$prop.tbl[4]
    temp<-append(temp, g3 )
  }
}
