
and_pcn <- function(data,target,r){
  cnt <- 1
  while(cnt!=4){
    for (i in 1:nrow(data)){
      error <- target[i]-data[i,]%*%weight
      ste <- ifelse(error>0,1,0)
      if (ste==1){
        for (j in 1:ncol(data)){
          weight[j] <- weight[j]+r*data[i,j]*ste
        }
      } else{
        cnt <- cnt+1
      }
    }
  }
  print(weight)
}

inputs<-matrix(c(0,0,1,0,0,1,1,1),nrow=4,byrow=T)
target<-matrix(c(0,0,0,1),nrow=4)
new_inputs<-cbind(matrix(c(-1,-1,-1,-1),nrow=4),inputs)

weight <- matrix(c(0.3,0.4,0.1),nrow=3)

l_rate <- 0.05

and_pcn(new_inputs,target,l_rate)
