install.packages("networkD3")
install.packages("dplyr")

library(networkD3)
library(dplyr)


# data set 소설 레미제라블 인물 관계도
# 1. 소설 장발장 데이터를 가져온다.
data(MisLinks, MisNodes)

head(MisNodes)  # 책 읽는 시간같은 데이터
# name group size
# 1          Myriel     1   15
# 2        Napoleon     1   20
# 3 Mlle.Baptistine     1   23
# 4    Mme.Magloire     1   30
# 5    CountessdeLo     1   11
# 6        Geborand     1    9

head(MisLinks) # 인물끼리 몇번 만났는지 데이터 

# source target value
# 1      1      0     1
# 2      2      0     8
# 3      3      0    10
# 4      3      2     6
# 5      4      0     1
# 6      5      0     1

# plot

D3_network_LM<-forceNetwork(Links = MisLinks, Nodes = MisNodes, 
                            Source = 'source', Target = 'target', 
                            NodeID = 'name', Group = 'group',opacityNoHover = TRUE,
                            zoom = TRUE, bounded = TRUE,
                            fontSize = 15,
                            linkDistance = 75,
                            opacity = 0.9)

D3_network_LM

# html 발사
networkD3::saveNetwork(D3_network_LM, "D3_LM.html", selfcontained = TRUE)