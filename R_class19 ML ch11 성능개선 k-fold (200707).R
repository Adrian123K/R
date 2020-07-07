# ■ 홀드 아웃 과 k 홀드 아웃 
## Estimating Future Performance ----
# partitioning data

#install.packages("caret") 
library(caret)
credit <- read.csv("credit.csv",stringsAsFactors = T) # 독일 은행의 채무 불이행자를 예측하기 위한 데이터 

# Holdout method
# using random IDs

nrow(credit)

random_ids <- order(runif(1000)) # 난수 1000개 생성 

credit_train <- credit[random_ids[1:500],]  # 훈련 50%
credit_validate <- credit[random_ids[501:750], ] # 검정 25%
credit_test <- credit[random_ids[751:1000], ]  # 테스트 25%

nrow(credit_train)
nrow(credit_validate)
nrow(credit_test)

# using caret function

in_train <- createDataPartition(credit$default, p = 0.75, list = FALSE)

credit_train <- credit[in_train, ] # 훈련 데이터 구성
credit_test <- credit[-in_train, ] # 테스트 데이터 구성 

nrow(credit_train)
nrow(credit_test)

# 10-fold CV

folds <- createFolds(credit$default, k = 10)
str(folds)

credit01_test <- credit[folds$Fold01, ]
credit01_train <- credit[-folds$Fold01, ]

# 전체 10폴드 교차검증을 수행하려면 이 단계는 10회 반복되어야한다. 

## Automating 10-fold CV for a C5.0 Decision Tree using lapply() ----

library(caret)
library(C50)
library(irr)

credit <- read.csv("credit.csv")

set.seed(123)

in_train <- createDataPartition(credit$default,p = 0.75, list = FALSE) 
credit_train <- credit[in_train, ] # 훈련 데이터 구성
credit_test <- credit[-in_train, ] # 테스트 데이터 구성

cv_results <- lapply(folds, function(x) {
  credit_train <- credit[-x, ]
  credit_test <- credit[x, ]
  credit_model <- C5.0(default ~ ., data = credit_train)
  credit_pred <- predict(credit_model, credit_test)
  credit_actual <- credit_test$default
  kappa <- kappa2(data.frame(credit_actual, credit_pred))$value
  return(kappa)
})

str(cv_results)
mean(unlist(cv_results))

############################################# 점심
library(caret)
library(C50)
library(irr)

credit <- read.csv("credit.csv",stringsAsFactors = T)

#set.seed(123)
#random_ids <- order(runif(800))
#credit <- credit[random_ids[1:800],]

in_train <- createDataPartition(credit$default,p = 0.75, list = FALSE) 
credit_train <- credit[in_train, ] 
credit_test <- credit[-in_train, ] 

nrow(credit_train) #601
nrow(credit_test) #199

folds <- createFolds(credit$default, k = 10)

cv_results <- lapply(folds, function(x) {
  credit_train <- credit[-x, ]
  credit_test <- credit[x, ]
  credit_model <- C5.0(default ~ ., data = credit_train)
  credit_pred <- predict(credit_model, credit_test)
  credit_actual <- credit_test$default
  kappa <- kappa2(data.frame(credit_actual, credit_pred))$value
  return(kappa)
})

str(cv_results)

mean(unlist(cv_results))

# 296
credit <- read.csv("credit.csv",stringsAsFactors = T)

set.seed(123)
folds <- createFolds(credit$default, k = 10)

cv_results <- lapply(folds, function(x) {
  credit_train <- credit[-x, ]
  credit_test <- credit[x, ]
  credit_model <- C5.0(default ~ ., data = credit_train)
  credit_pred <- predict(credit_model, credit_test)
  credit_actual <- credit_test$default
  
  x <- data.frame(credit_actual, credit_pred)
  rs <- sum(x$credit_actual==x$credit_pred)/length(x$credit_actual==x$credit_pred)
  return(rs)
})
str(cv_results)


# caret 패키지 사용
credit <- read.csv('credit.csv',stringsAsFactors = T)
set.seed(300)
m <- train(default~., data=credit, method='C5.0')
p <- predict(m,credit)
table(p,credit$default)

m <- train(default~., data=credit,method='C5.0')
ctrl <- trainControl(method='cv',number=10,selectionFunction='oneSE')
grid <- expand.grid(.model='tree',
                    .trials=c(1,5,10,15,20,25,30,35), # trials를 8개로 제한하겠다
                    .winnow='FALSE')

m <- train(default~., data=credit, method='C5.0',
           metric='kappa',
           trControl=ctrl,
           truneGrid=grid)
p <- predict(m, credit)
table(p,credit$default)
