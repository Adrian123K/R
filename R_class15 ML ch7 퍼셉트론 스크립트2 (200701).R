inputs1 <- matrix(c(0,1,0,1,0,0,1,1),nrow=4)
targets1 <- matrix(c(0,0,0,1),nrow=4)
input <- cbind(c(1,1,1,1),inputs1)
r <- 0.05

and_pcn <- function(input,taget,r){
  w <- matrix(c(0,0,0),nrow=3) 
  for (i in 1:3){ 
    w[i] <- runif(1)
  }
  step <- function(x){
    ifelse(x>=0,1,0)
  }
  cnt <- 1
  while(cnt<5){
    for (i in 1:nrow(input)){
      k <- input[i,] %*% w
      tfk <- target[i]-step(k)
      if (tfk!=0){
        for (j in 1:ncol(input)){
          w[j] <- w[j]+tfk*r*input[i,j]        
        }
        cnt <- 1
      }else{
        cnt <- cnt + 1
        cat('\n row:',i, w)
      }
    }
  }
  print(w)
}

and_pcn(input,target,r)