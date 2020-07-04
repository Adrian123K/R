# 1. 이번주에 친구를 만난 횟수를 사회행렬로 구성함
paper <- read.csv("paper1.csv" , header = T)
paper[is.na(paper)] <- 0 
paper 

rownames(paper) <- paper[,1] 
paper <- paper[-1]
paper2 <- as.matrix(paper) 
paper2

# 2. 이번주에 개별적으로 책을 읽은 시간 데이터를 로드한다.
book <- read.csv("book_hour.csv" , header = T)
paper2
book

library(sna) 
x11()

gplot(paper2 , displaylabels = T, boxed.labels = F , 
      vertex.col = "blue" , vertex.sides = 20 , edge.lwd = paper2*2 , edge.col = "green" , label.pos = 3)

paper <- read.csv("11_meal_m.csv" , header = T)
paper[is.na(paper)] <- 0 
paper 

rownames(paper) <- paper[,1] 
paper <- paper[-1]
paper2 <- as.matrix(paper) 
paper2
