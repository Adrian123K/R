# 1. 데이터를 로드한다.

credit <- read.csv("credit.csv",head=T,stringsAsFactors = T)

str(credit) 


# 2. 데이터에 각 컬럼들을 이해한다. 
#라벨 컬럼 :  default  --->  yes : 대출금 상환 안함 no  : 대출금 상환 

prop.table( table(credit$default)  )

#no   yes 
#0.7   0.3 

#-  계좌 소개 :  
#checking_balance --> 예금계좌
#saving_balance   --> 적금계좌 

#-  amount    :  대출 금액 250마크르 ~ 18424 마르크 ( 100 마르크가 우리나라돈 6~7만원 )

summary( credit$amount)


#은행의 목표 :  과거의 데이터를 분석해보니 대출금 상환 불이행자가 30%나 되어서 앞으로는 30% 이내로 떨어뜨리는게  은행의 목표
#그에 맞는 model 을 생성해야한다.

# 3. 데이터가 명목형 데이터인지 확인해본다.

str(credit) 

# 4. 데이터를 shuffle 시킨다.

set.seed(31)

credit_shuffle <-  credit[ sample( nrow(credit) ),  ]

# 5. 데이터를 9 대 1로 나눈다.

train_num <- round( 0.9 * nrow(credit_shuffle), 0) 

credit_train <- credit_shuffle[1:train_num ,  ]

credit_test  <- credit_shuffle[(train_num+1) : nrow(credit_shuffle),  ]

# 6. C5.0 패키지와 훈련 데이터를 이용해서 모델을 생성한다.

library(C50)

credit_model <- C5.0( credit_train[ ,-17] , credit_train[  , 17] )
credit_model
summary(credit_model)
# 7. 위에서 만든 모델을 이용해서 테스트 데이터의 라벨을 예측한다.

credit_result <-  predict( credit_model, credit_test[  , -17] )

# 8. 이원 교차표로 결과를 확인한다.

library(gmodels)

CrossTable(  credit_test[   , 17], credit_result )
#                ↑                     ↑
#              실제                   예측
rs <- CrossTable(  credit_test[   , 17], credit_result )

#문제. 부스팅 기법을 이용해서 위의 의사결정트리 모델의 정확도를 올리시오 !
credit_model <- C5.0( credit_train[ ,-17] , credit_train[  , 17] , trials=10)
credit_result <-  predict( credit_model, credit_test[  , -17] )
CrossTable(  credit_test[   , 17], credit_result )
rs2 <- CrossTable(  credit_test[   , 17], credit_result )
rs2