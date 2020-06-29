A <- matrix(c(1,2,3,4,5,6), nrow = 3, ncol = 2, byrow=T)
A
t(A)

b <- matrix(c(1,2,3,2,3,4),nrow=2,ncol=3, byrow=T)
i <- diag(3)
b%*%i

A <- matrix(c(1,2,3,4),nrow=2,ncol=2, byrow=T)
solve(A)
round(A %*% solve(A))

reg <- function(y, x){
  x <- as.matrix(x)
  x <- cbind(intercept=1,x)
  b <- solve(t(x)%*%x)%*%t(x)%*%y
  print(b)
}

launch <- read.csv('challenger.csv',head=T)
lnch2 <- launch[,-1]
lnch2

reg(y=launch$distress_ct,lnch2)

