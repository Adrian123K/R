knn_fun<-function(){
  
  x <-  "d:/R/wisc_bc_data.csv"
  y <-"diagnosis"
  k_n <- 21
  
  data1 <- read.csv(x, stringsAsFactors=FALSE)
  
  normalize<-function(x) {
    return( (x-min(x))/ ( max(x)-min(x)))
  }
  
  data1 <- data1[-1]
  ncol1 <- which(colnames(data1)==y)
  
  data1_n <- as.data.frame(lapply(data1[,-ncol1], normalize) )   
  mm<-round(nrow(data1_n)*2/3)
  
  data1_train <- data1_n[1:mm, ]
  data1_test  <- data1_n[(mm+1):nrow(data1_n), ]
  
  data1_train_label <- data1[1:mm,y]
  data1_test_label  <- data1[(mm+1):nrow(data1_n),y]
  
  library(class) 
  
  result1 <- knn(train=data1_train, test=data1_test,  cl= data1_train_label, k = k_n ) 
  prop.table( table(ifelse(data1[(mm+1):nrow(data1_n),y]==result1,"o","x" )))
  
  # 윈도우 탐색기로 테스트환자를 불러옵니다. 
  fname <- file.choose() 
  table <- read.csv(fname, header=T, stringsAsFactor=F )
  
  # 환자번호를 제외합니다.  
  table <- table[-1]  
  
  #라벨 컬럼을 지정합니다.  
  ncol1 <- which(colnames(table)==y)
  
  # 데이터 정규화를 위해 기존 데이터와 묶어줍니다.   
  data1 <- rbind(data1,table)
  data1_n <- as.data.frame(lapply(data1[,-ncol1], normalize) ) 
  
  #맨 마지막 환자를 선택합니다.   
  data2_test <- data1_n[nrow(data1_n),]  
  result2 <- knn(train=data1_train, test=data2_test,  cl= data1_train_label, k = k_n )  
  
  if (result2=='M'){
    print('ML 모델이 암환자로 예측 했습니다.')
  } else {
    print('ML모델이 정상환자로 예측 했습니다.')
  }
}

knn_fun()
