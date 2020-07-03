km <- function(data){
  k <- round(sqrt(nrow(data)/2))
  c <- matrix(data[sample(nrow(data),k),],nrow=k)
  rs <- matrix(c(rep(0,nrow(data))),nrow=nrow(data),ncol=ncol(data))
}

raw <- matrix(c(1,1,1,2,2,3,2,5,3,4,5,1,6,8,7,7,9,8,10,6),nrow=10,ncol=2,byrow=T)
raw

c1_n <- sample(nrow(raw),2,replace = F)

c1 <- raw[1:min(c1_n),]
c2 <- raw[(min(c1_n)+1):nrow(raw),]
ctr1 <- colMeans(c1)
ctr2 <- colMeans(c2)



rs <- data.frame(x=raw[,1],y=raw[,2])

for (i in 1:nrow(raw)){
  d <- sqrt(sum((raw[i,]-center[1,])^2))
  rs$d1[i] <- d
}
rs$d1
rowMeans(t(rs[1]))
rowMeans(t(rs[2]))


wbcd <- read.csv("wisc_bc_data.csv", header=T,  stringsAsFactors=FALSE)

wbcd_shuffle <- wbcd[sample(nrow(wbcd)), ]

wbcd2 <- wbcd_shuffle[-1]

normalize <- function(x) {
  return ( (x-min(x)) / (max(x) - min(x))  )
}

wbcd_n  <- as.data.frame(lapply(wbcd2[2:31],normalize))

train_num <- round(0.9*nrow(wbcd_n),0)
train <- wbcd_n[1:train_num,]
test <- wbcd_n[(train_num+1):nrow(wbcd_n),]

wbcd_train_label <- wbcd2[1:train_num,1]
wbcd_test_label <- wbcd2[(train_num+1):nrow(wbcd_n),1]

c1_n <- sample(nrow(raw),2,replace = F)

c1 <- raw[1:min(c1_n),]
c2 <- raw[(min(c1_n)+1):nrow(raw),]
ctr1 <- colMeans(c1)
ctr2 <- colMeans(c2)

km <- function(train, test, cl, k) {
  pred <- c()
  for (i in 1:nrow(test)) {
    temp <- t(t(train)-c(t(test[i,])))^2         
    temp <- sqrt(rowSums(temp))
    table <- data.frame(train, kind=cl, d=temp)
    #table$rnk <- rank(table$d, ties.method = 'min')
    while(1){
      
    }
    pred <- append(pred, names(which.max(table(top))))
  }
  return(pred)
}
