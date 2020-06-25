naive_fun<-function(){
  library(e1071)  
  
  movie <- read.csv("movie.csv", header=T, stringsAsFactors=TRUE)
  colnames(movie) <- c("age","gender","job","marry","friend","m_type")
  
  train <- movie[1:39,]  
  model <- naiveBayes(train[ ,1:5], train$m_type , laplace=0)
  
  fname <- file.choose()   
  test <- read.csv(fname, header=T, stringsAsFactor=F )
  
  result <- predict( model, test)  
  result
}

naive_fun()