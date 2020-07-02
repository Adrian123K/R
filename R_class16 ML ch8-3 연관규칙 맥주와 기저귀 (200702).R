# 1. 데이터를 로드한다. 

x <- data.frame(beer=c(0,1,1,1,0),
                bread=c(1,1,0,1,1),
                cola=c(0,0,1,0,1),
                diapers=c(0,1,1,1,1),
                eggs=c(0,1,0,0,0),
                milk=c(1,0,1,1,1) )
x 

#   beer bread cola diapers eggs milk
# 1    0     1    0       0    0    1
# 2    1     1    0       1    1    0
# 3    1     0    1       1    0    1
# 4    1     1    0       1    0    1
# 5    0     1    1       1    0    1

# 2. arules 패키지를 설치한다. 
install.packages("arules")  
library(arules)

trans <-  as.matrix( x, "Transaction") 
trans 

# 3. apriori 함수를 이용해서 연관관계를 분석한다. 
rules1 <- apriori(trans, parameter=list(supp=0.2, conf=0.6, target="rules") )
rules1

inspect(sort(rules1)) 

# 4. 위의 맥주와 기저귀 연관 관계를 시각화 하기 
install.packages("sna")
install.packages("rgl")
library(sna)
library(rgl)

b2 <- t(as.matrix(trans)) %*% as.matrix(trans) 

library(sna)
library(rgl)

b2.w <- b2 - diag(diag(b2))

gplot(b2.w , displaylabel=T , 
      vertex.cex=sqrt(diag(b2)) , 
      vertex.col = "green" , 
      edge.col="blue" , 
      boxed.labels=F , 
      arrowhead.cex = .3 , 
      label.pos = 3 , 
      edge.lwd = b2.w*2) 