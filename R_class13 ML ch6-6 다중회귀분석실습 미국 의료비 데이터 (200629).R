insurance <- read.csv("insurance.csv")
head(insurance) 
#2. 정규화 작업을 수행한다. 
normalize <- function(x) {
  return ( (x-min(x)) / (max(x) - min(x))  ) }
insurance_n  <- as.data.frame(lapply(insurance[,c(1,3,4,7)]  ,normalize))

# 3. 독립변수와 종속변수간의 상관관계 분석 
cor( insurance[    , c("age","bmi","children","expenses")] ) 

# 4. 회귀함수인 lm 을 이용해서 독립변수들의 회귀모수를 확인한다 (기울기)
m5 <- lm( expenses ~ age + children + bmi +smoker +region,  data=insurance)

#또는
attach(insurance)
lm(expenses ~ .,  data= insurance)  