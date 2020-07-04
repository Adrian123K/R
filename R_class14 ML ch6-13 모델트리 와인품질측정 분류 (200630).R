# 1. 회귀트리 모델 생성하는 작업의 1번 ~ 3번까지 다시 반복 

wine <- read.csv("whitewines.csv")

wine_train <- wine[1:3750,  ]
wine_test <- wine[3751:4898, ]

install.packages('Cubist')
library(Cubist)

model_tree <- cubist(wine_train[-12], wine_train$quality) 
model_tree

# 4. 만든 모델과 테스트 데이터로 예측을 한다.
p.cubist <- predict( model_tree, wine_test) 

# 5. 예측값(p.m5p) 과 테스트 데이터의 라벨간의 상관관계를 확인한다
cor( p.cubist , wine_test$quality )

#6. 예측값(p.m5p) 과 테스트 데이터의 라벨간의 평균절대오차를 확인 한다.
MAE <-  function( actual, predicted) {
  mean(abs(actual - predicted)) 
}
MAE(wine_test$quality, p.cubist)

