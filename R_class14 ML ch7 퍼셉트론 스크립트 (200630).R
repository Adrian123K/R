
and_pcn <- function(data,target,r){
  cnt <- 0
  weight <- matrix(c(0.3,0.4,0.1),nrow=3)
  while(cnt<5){
    for (i in 1:nrow(data)){
      k <- data[i,]%*%weight
      ste <- ifelse(k>=0,1,0) #f(k)
      rs <- target[i]-ste
      if (rs!=0){
        for (j in 1:nrow(weight)){
          weight[j] <- weight[j]+r*data[i,j]*rs
        }
        cnt <- 0
      }else{
        cnt <- cnt+1
      }
    }
  }
  print(weight)
}

inputs<-matrix(c(0,0,1,0,0,1,1,1),nrow=4,byrow=T)
target<-matrix(c(0,0,0,1),nrow=4)

data<-cbind(matrix(c(-1,-1,-1,-1),nrow=4),inputs)
weight <- matrix(c(0.3,0.4,0.1),nrow=3)
l_rate <- 0.05

and_pcn(data,target,l_rate)