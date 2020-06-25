
# 2. 관련된 패키지를 설치 합니다.
install.packages("e1071")
library(e1071)

# 3. 데이터를 로드 합니다.
movie <- read.csv("movie.csv", header=T, stringsAsFactors=TRUE)
str(movie)
nrow(movie)

# 4. 컬럼명을 영어로 변경합니다. 
View(movie)
colnames(movie) <- c("age","gender","job","marry","friend","m_type")
View(movie)

# 5. 38개의 행으로 훈련시키고 나머지 39번째 하나의 행으로 테스트 합니다. 
train <- movie[1:38,]
test <- movie[39,]
train
test

# 6. 훈련 데이터로 나이브 베이즈 모델을 생성합니다. 
model <- naiveBayes(train[ ,1:5], train$m_type , laplace=0.0001)
model

#7. 테스트 데이터를 잘 예측하는지 확인합니다. 
result <- predict( model, test)
result

test2 <- data.frame(age='20대', gender='남', job='학생', marry='NO',friend='NO')
result <- predict( model, test2)
result 
