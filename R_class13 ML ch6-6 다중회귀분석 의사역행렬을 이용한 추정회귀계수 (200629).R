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

smp <- read.csv('multi_hg.csv',head=T)
head(smp)
reg(smp$만족감,smp[,1:3])

attach(smp)
smp_model <- lm(만족감~외관+편의성+유용성,data=smp)
smp_model

#262
subj <- read.csv('sports.csv',head=T)
head(subj)
reg(y=subj$acceptance,x=subj[,-1])
lm(acceptance~academic+sports+music,data=subj)

#263
normalize<-function(x) {
  return( (x-min(x))/ ( max(x)-min(x)))
}
subj2 <- subj[,-1]
rs <- lapply(subj2,normalize)
rs <- as.data.frame(rs)
head(rs)
lm(acceptance~.,data=rs)

