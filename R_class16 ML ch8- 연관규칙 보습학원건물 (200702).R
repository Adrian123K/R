# 1. 데이터를 로드합니다.
build <- read.csv("building.csv" , header = T)

# 2. na 를 0 으로 변경합니다. 
build[is.na(build)] <- 0  

# 3. 필요한 변수만 선별합니다.
build <- build[-1]
build 

# 4. 연관규칙 패키지를 다운로드 받습니다. 
install.packages("arules")
library(arules) 

# 5. 연관규칙 모델을 생성합니다.
trans <- as.matrix(build , "Transaction")

rules1 <- apriori(trans , parameter = list(supp=0.2 , conf = 0.6 , target = "rules"))
rules1 

# 6. 연관규칙을 확인합니다. 
inspect(sort(rules1))

# 7. 시각화를 합니다. 
rules2 <- subset(rules1 , subset = lhs %pin% '보습학원' & confidence > 0.7)

inspect(sort(rules2)) 

rules3 <- subset(rules1 , subset = rhs %pin% '편의점' & confidence > 0.7)
rules3

inspect(sort(rules3)) 

#visualization
b2 <- t(as.matrix(build)) %*% as.matrix(build) # 희소 행렬 생성

#install.packages("sna")
#install.packages("rgl")
#library(sna)
#library(rgl)

b2.w <- b2 - diag(diag(b2)) # 대각성분 제외

#rownames(b2.w) 
#colnames(b2.w) 
dev.new()

gplot(b2.w , 
      displaylabel=T , 
      vertex.cex=sqrt(diag(b2)) , 
      vertex.col = "green" , 
      edge.col="blue" , 
      boxed.labels=F , 
      arrowhead.cex = .3 , 
      label.pos = 3 , 
      edge.lwd = b2.w*2) 
