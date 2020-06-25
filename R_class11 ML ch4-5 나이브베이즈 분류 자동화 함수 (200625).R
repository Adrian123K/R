naive_fun<-function(){
  library(e1071)  
  
  movie <- read.csv("movie.csv", header=T, stringsAsFactors=TRUE)
  colnames(movie) <- c("age","gender","job","marry","friend","m_type")
  
  train <- movie[1:39,]  
  model <- naiveBayes(train[ ,1:5], train$m_type , laplace=0)
  
  fname <- file.choose()   
  test <- read.csv(fname, header=T, stringsAsFactor=F )
  
  result <- predict( model, test)  
  print(result)
}

naive_fun()

flu <- read.csv("flu.csv", header=T, stringsAsFactors=TRUE)
flu2 <- flu[1:nrow(flu),-1]
flu2

naive_fun<-function(){
  library(e1071)  
  
  flu <- read.csv("flu.csv", header=T, stringsAsFactors=TRUE)

  train <- flu[1:nrow(flu),-1]  
  model <- naiveBayes(train[,1:4], train$flue , laplace=0)
  
  fname <- file.choose()   
  test <- read.csv(fname, header=T, stringsAsFactor=F )
  
  result <- predict( model, test)  
  result
}

naive_fun()

naive_fun<-function(){
  library(e1071)  
  
  flu <- read.csv("flu.csv", header=T, stringsAsFactors=TRUE)
  
  train <- flu[1:nrow(flu),-1]  
  model <- naiveBayes(train[,1:4], train$flue , laplace=0)
  
  fname <- file.choose()   
  test <- read.csv(fname, header=T, stringsAsFactor=F )
  
  result <- predict( model, test)  
  if (result=='Y'){
    print('이 환자는 독감 환자 입니다.')
  } else {
    print('이 환자는 독감 환자가 아닙니다.')
  }
  print(model)
}

naive_fun()

naive_fun<-function(){
  library(e1071)  
  
  flu <- read.csv("flu.csv", header=T, stringsAsFactors=TRUE)
  
  train <- flu[1:nrow(flu),-1]  
  model <- naiveBayes(train[,1:4], train$flue , laplace=0)
  
  q1 <- readline(prompt = '오한이 있습니까? Y/N ')
  q2 <- readline(prompt = '콧물이 있습니까? Y/N ')
  q3 <- readline(prompt = '두통이 있습니까? MILD/STRONG/NO ')
  q4 <- readline(prompt = '오한이 있습니까? Y/N ')
  
  test <- data.frame(chills=q1,runny_nose=q2,headache=q3,fever=q4)
  
  result <- predict( model, test)  
  if (result=='Y'){
    print('이 환자는 독감 환자 입니다.')
  } else {
    print('이 환자는 독감 환자가 아닙니다.')
  }
}

naive_fun()

naive_fun<-function(){
  library(e1071)  
  
  flu <- read.csv("flu.csv", header=T, stringsAsFactors=TRUE)
  
  train <- flu[1:nrow(flu),-1]  
  model <- naiveBayes(train[,1:4], train$flue , laplace=0)
  
  fname <- file.choose()   
  test <- read.csv(fname, header=T, stringsAsFactor=F )
  
  result <- predict( model, test, type='raw')  
  print(paste(round(result[2]*100,digits=1),'% 입니다.'))
}

naive_fun()
