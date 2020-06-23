#1. 데이터를 로드 한다.
wbcd <- read.csv("wisc_bc_data.csv", header=T,  stringsAsFactors=FALSE)


#2. diagnosis 를 factor 로 변환한다
wbcd$diagnosis <- factor(wbcd$diagnosis,
                         levels =c("B","M"),
                         labels = c("Benign","Maliganant"))

#3. 데이터를 shuffle 시킨다.
wbcd_shuffle <- wbcd[sample(nrow(wbcd)), ]

#4. 데이터에서 id 를 제외 시킨다
wbcd2 <- wbcd_shuffle[-1]

#5. 데이터를 정규화 한다.
normalize <- function(x) {
  return ( (x-min(x)) / (max(x) - min(x))  )
}
wbcd_n  <- as.data.frame(lapply(wbcd2[2:31],normalize))

#6. train 데이터와 test 데이터로 9 대 1로 나눈다
train_num<-round(0.9*nrow(wbcd_n),0)
wbcd_train<-wbcd_n[1:train_num,]
wbcd_test<-wbcd_n[(train_num+1):nrow(wbcd_n),]

#7. train 데이터를 데이터와 라벨로 나누고 test  데이터를 데이터와 라벨로 나누시오 ~
wbcd_train_label <- wbcd2[1:train_num,1]
wbcd_test_label <- wbcd2[(train_num+1):nrow(wbcd_n),1]

#8. knn 모델로 훈련시켜서 모델을 만들고 바로 그 모델에 test 데이터를 넣어서 정확도를 확인한다
result1 <- knn(train=wbcd_train, test=wbcd_test, cl=wbcd_train_label, k=21)
result1
