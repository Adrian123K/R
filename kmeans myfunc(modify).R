km <- function(data){
  k <- round(sqrt(nrow(data)/2))
  c <- matrix(data[sample(nrow(data),k),],nrow=k)
  rs <- matrix(c(rep(0,nrow(data))),nrow=nrow(data),ncol=ncol(data))
}

raw <- matrix(c(1,1,1,2,2,3,2,5,3,4,5,1,6,8,7,7,9,8,10,6),nrow=10,ncol=2,byrow=T)
raw

center <- raw[sample(nrow(raw),1),]
center <- raw[sample(nrow(raw),2),]
t(center)

sqrt(sum(center[1,]^2))


rs <- data.frame(x=raw[,1],y=raw[,2])

for (i in 1:nrow(raw)){
  d <- sqrt(sum((raw[i,]-center[j,])^2))
  rs$d1[i] <- d
}

rowMeans(t(rs[1]))
rowMeans(t(rs[2]))




myknn <- function(train, test, cl, k) {
  pred <- c()
  for (i in 1:nrow(test)) {
    temp <- t(t(train)-c(t(test[i,])))^2         
    temp <- sqrt(rowSums(temp))
    table <- data.frame(train, kind=cl, d=temp)
    table$rnk <- rank(table$d, ties.method = 'min')
    top <- table[table$rnk <= k,'kind']
    pred <- append(pred, names(which.max(table(top))))
  }
  return(pred)
}
