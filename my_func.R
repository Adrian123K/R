my_func <- function() {
  
  library(data.table)
  
  my_scatter <- function() {
    
    fname <- file.choose()
    
    table <- read.csv(fname, header=T, stringsAsFactor=F )       
    print(data.table(colnames(table)))
    
    xcol_num <- as.numeric(readline('x축 컬럼 번호: '))    
    ycol_num <- as.numeric(readline('y축 컬럼 번호: '))      
    
    xcol <- colnames(table[xcol_num])
    ycol <- colnames(table[ycol_num])
    
    xcol2 <- table[,xcol]
    ycol2 <- table[,ycol]
    
    plot(xcol2,ycol2,
         main=paste(xcol,'과',ycol,'의 추정회귀직선 및 산포도'),lwd=2,             
         xlab=xcol,ylab=ycol,col='red',pch=21,bg='red')
    model_scatter <- lm( ycol2 ~ xcol2, data=table)
    abline( model_scatter,  col="red") 
    
  }
  
  my_hist <- function() {
    
    fname <- file.choose()    
    
    table <- read.csv(fname, header=T, stringsAsFactor=F )    
    print(data.table(colnames(table)))
    
    xcol_num <- as.numeric(readline('x축 컬럼 번호: '))
    xcol <- colnames(table[xcol_num])
    
    xcol2 <- table[,xcol]  
    class1 <- sort(xcol2)
    
    hist(class1 , col="yellow", density=80,      main="히스토그램 정규분포 그래프" )
    
    par(new=T)
    
    plot( class1, dnorm( class1, mean=mean(class1),                        
                         sd=sd(class1)),type='l', axes=FALSE, ann=FALSE,   col="red") 
  }
  
  my_box <- function() {
    
    fname <- file.choose()  
    
    table <- read.csv(fname, header=T, stringsAsFactor=F )    
    print(data.table(colnames(table)))  
    
    xcol_num <- as.numeric(readline('x축 컬럼 번호: '))      
    xcol <- colnames(table[xcol_num])  
    xcol2 <- table[,xcol]  
    
    boxplot(xcol2, col="green", density=80 )
  }
  
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
    
    fname <- file.choose() 
    table <- read.csv(fname, header=T, stringsAsFactor=F )
    
    table <- table[-1]  
    
    ncol1 <- which(colnames(table)==y)
    
    data1 <- rbind(data1,table)
    data1_n <- as.data.frame(lapply(data1[,-ncol1], normalize) ) 
    
    data2_test <- data1_n[nrow(data1_n),]  
    result2 <- knn(train=data1_train, test=data2_test,  cl= data1_train_label, k = k_n )  
    
    if (result2=='M'){
      print('ML 모델이 암환자로 예측 했습니다.')
    } else {
      print('ML 모델이 정상환자로 예측 했습니다.')
    }
    
  }
  
  naive_fun<-function(){
    library(e1071)  
    
    flu <- read.csv("flu.csv", header=T, stringsAsFactors=TRUE)
    
    train <- flu[1:nrow(flu),-1]  
    model <- naiveBayes(train[,1:4], train$flue , laplace=0)
    
    fname <- file.choose()   
    test <- read.csv(fname, header=T, stringsAsFactor=F )
    
    result <- predict( model, test, type='raw')  
    print(paste('독감 환자일 확률이',round(result[2]*100,digits=1),'% 입니다.'))
  }
  
  
  x1 <- menu( c('산포도 그래프','히스토그램 그래프','사분위수 그래프', '유방암 진단', '독감 진단') ,            
              title='숫자를 선택하세요: ' )  
  
  switch ( x1,            
           san1 = {   my_scatter()       },          
           san2 = {   my_hist()          },          
           san3 = {   my_box()           },
           san4 = {   knn_fun()          },
           san5 = {   naive_fun()        }
  )  
}

my_func()
