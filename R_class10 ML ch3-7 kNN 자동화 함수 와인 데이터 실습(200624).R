knn_fun<-function(){
  
  data1 <- read.csv("wine.csv", stringsAsFactors=FALSE)
  k_n<-21
  
  data1$Type <- factor(data1$Type,
                           levels =c("t1","t2","t3"),
                           labels = c("t1","t2","t3"))
  
  data1 <- data1[sample(nrow(data1)), ]
  
  normalize<-function(x) {
    return( (x-min(x))/ ( max(x)-min(x)))
  }
  
  #data1 <- data1[-1]
  #ncol1 <- which(colnames(data1)==y)
  
  data1_n <- as.data.frame(lapply(data1[,-1], normalize) )  
  
  mm<-round(nrow(data1_n)*9/10) 
  
  data1_train <- data1_n[1:mm, ]
  data1_test  <- data1_n[(mm+1):nrow(data1_n), ]
  
  data1_train_label <- data1[1:mm,y]
  data1_test_label  <- data1[(mm+1):nrow(data1_n),y]
  
  library(class)
  
  result1 <- knn(train=data1_train, test=data1_test,  cl= data1_train_label, k = k_n )
  
  x <- data.frame(실제=data1_test_label, 예측=result1)
  table(x)

}

knn_fun()
