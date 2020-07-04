install.packages("car")
library(car)
test <- read.csv('test_vif1.csv')
test

cor(test[,c('아이큐','공부시간')])

test <- test[,-1]
m1 <- lm(test$시험점수~., data=test)
summary(m1)

vif(m1) # 다중공선성 확인

test2 <- read.csv('test_vif2.csv')
test2 <- test2[,-1]
test2

test_m <- lm(test2$시험점수~., data=test2)
test_m

summary(test_m)
vif(test_m)
vif(test_m) > 15 # 아이큐, 등급평균

