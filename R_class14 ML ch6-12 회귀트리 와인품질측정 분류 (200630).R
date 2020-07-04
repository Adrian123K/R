wine <- read.csv('whitewines.csv')
View(wine)
unique(wine$quality)

wine <- read.csv("whitewines.csv")

#fixed.acidity       : 고정 산도
#volatile.acidity    : 휘발성 산도
#citric.acid         : 시트르산
#residual.sugar      : 잔류 설탕
#chlorides           : 염화물
#free.sulfur.dioxide : 자유 이산화황
#total.sulfur.dioxide: 총 이산화황
#density             : 밀도
#pH                  : pH
#sulphates           : 황산염
#alcohol             : 알코올
#quality             : 품질

# 2. 와인의 quality 데이터가 정규분포에 속하는 안정적인 데이터 인지 확인
hist(wine$quality)

# 3. wine 데이터를 train 데이터와 test 데이터로 나눈다.
wine_train <- wine[1:3750,  ]

wine_test <- wine[3751:4898, ]

# 4. train 데이터를 가지고 model 을 생성한다. 
library(rpart)

model <- rpart( quality ~ . , data=wine_train)
model 

# 5. 위에서 나온 모델로 트리를 시각화 하시오 !
library(rpart.plot)

rpart.plot(model, digits=3)
rpart.plot(model, digits=3, fallen.leaves=T, type=3, extra=101)

# 6. 위에서 만든 모델로 테스트 데이터의 라벨을 예측하시오 !
result <- predict(model, wine_test) 
result

# 7. 테스트 데이터의 실제 라벨(품질) 과 예측결과(품질) 을 비교한다
cbind(round(result), wine_test$quality)

# 8. 테스트 데이터의 라벨과 예측 결과와 상관관계가 어떻게 되는지  확인한다.
cor(result, wine_test$quality)

# 9. 두 데이터간의 오차율을 확인 
MAE <-  function( actual, predicted) {
  mean(abs(actual - predicted)) 
}  

MAE(result, wine_test$quality)