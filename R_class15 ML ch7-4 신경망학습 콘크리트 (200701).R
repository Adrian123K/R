concrete <- read.csv("concrete.csv")
str(concrete)

hist(concrete$strength)

# 3. 정규화 함수로 데이터를 정규화 작업
normalize <- function(x) {
  return ( (x-min(x)) / (max(x) - min(x) ) )
}

concrete_norm <- as.data.frame(lapply(concrete,normalize) ) 

# 4. 0~1사이로 데이터가 잘 변경되었는지 확인 

summary( concrete_norm$strength)

# 본래 데이터의 최소값, 최대값과 비교 
summary( concrete$strength)

# 5. 훈련 데이터,테스트 데이터를 나눈다 (8:2)

concrete_train <- concrete_norm[1:773, ]
concrete_test  <- concrete_norm[774:1030, ]

# 6. neuralnet 패키지를 설치한다.

install.packages("neuralnet")
library(neuralnet) 

# 7. neuralnet 패키지에 콘크리트 훈련 데이터를 넣어서 모델을 생성한다.

concrete_model <- neuralnet(formula=strength ~ cement + slag + ash  +water +superplastic + coarseagg  + fineagg  + age,
                            data =concrete_train)   

# 9. 모델(신경망) 을 시각화

plot(concrete_model )

# 10. 만든 모델로 테스트 데이터를 가지고 테스트 한다

model_results <-  compute(concrete_model, concrete_test[1:8])
predicted_strength <-  model_results$net.result

# 11.  예측값과 실제값간의 상관관계를 확인 

cor(predicted_strength, concrete_test$strength) # 수치형 데이터를 예측한 것이므로 상관관계로 모델 성능을 확인
# 0.806285848

# 12. 모델의 성능 개선 

concrete_model2 <- neuralnet(formula=strength ~ cement + slag + ash  +
                               water +superplastic + coarseagg  + fineagg  + age, data =concrete_train , hidden=c(5,2) )  

hidden=  c(5,  2) 
#           ↑   ↑
#  은닉1층 5개  은닉2층 2개 

plot(concrete_model2)

# 13. 위에서 만든 모델로 테스트를 수행한다. 

model_results <-  compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <-  model_results$net.result

# 14.  예측값과 실제값간의 상관관계를 확인 

cor(predicted_strength2, concrete_test$strength)
