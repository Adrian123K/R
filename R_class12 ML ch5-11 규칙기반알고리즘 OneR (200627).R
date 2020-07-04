install.packages('FSelector')
library(FSelector)

weights <- information.gain(mushroom)
weights

install.packages('doBy')
library(doBy)

orderBy(~-attr_importance, weights)


# 1. 버섯 데이터를 R 로 로드한다.
mushroom <- read.csv("mushrooms.csv", stringsAsFactors=T)

# 2. mushroom 데이터를 훈련 데이터와 테스트 데이터로 나눈다 ( 훈련 데이터 75%,  테스트 데이터 25% ) 

set.seed(11)
dim(mushroom) 

train_cnt <- round( 0.75 * dim(mushroom)[1])

train_index <- sample(1:dim(mushroom)[1], train_cnt, replace=F)


mushroom_train <- mushroom[train_index,  ]

mushroom_test  <- mushroom[-train_index, ]


# 3. 규칙기반 알고리즘인 oneR 을 이용해서 독버섯과 일반버섯을 분류하는 모델을 생성한다.

install.packages("OneR")
library(OneR)

model1 <- OneR(type~. ,  data=mushroom_train)

model1

summary(model1)

# 4. 위에서 생성한 모델을 가지고 테스트 데이터로 결과를 확인한다.

result1 <- predict( model1, mushroom_test[   , -1] )

library(gmodels)

CrossTable( mushroom_test[ , 1],  result1)  