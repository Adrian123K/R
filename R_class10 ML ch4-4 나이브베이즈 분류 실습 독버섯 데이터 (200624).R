########################## 1단계

# 1. 버섯 데이터를 R 로 로드한다. 
mushroom <- read.csv("mushrooms.csv", header=T, stringsAsFactors=T)
View(mushroom)

########################## 2단계

# 명목형 변수이기 때문에 이상치 X

########################## 3단계

# 2. 8124 독버섯 데이터만 따로 빼서 mush_test.csv 로 저장한다. 

mush_test <- mushroom[8123, ]
mush_test 
write.csv( mush_test, "mush_test.csv",row.names=FALSE )

# 3. 8124 독버섯 데이터를 훈련 데이터에서 제외 시키시오 !

nrow(mushroom)
mushrooms <- mushroom[ -8123,  ] 
nrow(mushrooms)

# 4. mushrooms 데이터를 훈련 데이터와 테스트 데이터로 나눈다 ( 훈련 데이터는 75%,  테스트 데이터는 25% )

set.seed(1)
dim(mushrooms) # 차원 확인

train_cnt <- round( 0.75*dim(mushrooms)[1] )
train_cnt  # 6092

train_index <- sample( 1:dim(mushrooms)[1], train_cnt, replace=F) # 6092개 랜덤 생성하여 index 생성
train_index 

mushrooms_train <- mushrooms[ train_index,  ]
mushrooms_test <- mushrooms[- train_index,  ] # train에 들어간 index번호를 제외한 나머지만 test로 부여

nrow(mushrooms_train)  #  6092
nrow(mushrooms_test)    #  2031 

str(mushrooms_train)

# 5. 나이브 베이즈 알고리즘으로 독버섯과 일반 버섯을 분류하는 모델을 생성한다.
library(e1071) #        모든 컬럼들
#                          ↓
model1 <- naiveBayes(type~ . ,  data=mushrooms_train)
#                     ↑
#                   라벨 컬럼명 
model1 # 우도표 생성

# 6. 위에서 만든 모델과 테스트 데이터를 가지고 독버섯과 일반버섯을 잘 분류하는지 예측해 본다.
result1 <- predict( model1, mushrooms_test[  , -1] ) # label 제외
result1

# 7. 이원 교차표를 그려서 최종 분류 결과를 확인한다. 
library(gmodels)
CrossTable( mushrooms_test[  ,1], result1) 
#                   ↑                ↑
#                  실제            예측 

########################## 5단계 모델 성능 향상

# 8. 위의 모델의 성능을 올리시오 !
model2 <- naiveBayes(type~ . ,  data=mushrooms_train, laplace=0.0004)

result2 <- predict( model2, mushrooms_test[ , -1] )

CrossTable( mushrooms_test[ ,1], result2) 

# 위의 모델에  별도로 구분해 놓은 테스트 데이터 한개(독버섯) 8123 번 데이터를 넣어서 독버섯인지 정상인지 확인하시오 ! 

result3 <- predict( model2, mush_test )
result3

temp<-c()
for ( i in 1:100 ) {
  model_test_pred <- naiveBayes(type~ . ,  data=mushrooms_train, laplace=i/10000)
  rs <- predict( model_test_pred, mushrooms_test[,-1])
  g2 <- CrossTable(x=mushrooms_test[,1], y=rs, chisq=FALSE)
  g3 <- g2$prop.tbl[1] + g2$prop.tbl[4]
  temp<-append(temp, g3 )
}
temp
plot(temp, type='l', col='red')
