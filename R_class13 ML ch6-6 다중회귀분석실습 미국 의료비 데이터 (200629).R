insurance <- read.csv("insurance.csv")
head(insurance) 

colSums(is.na(insurance)) # 결측치 확인

grubbs.flag <- function(x) {
  outliers <- NULL
  test <- x
  grubbs.result <- grubbs.test(test)
  pv <- grubbs.result$p.value
  while(pv < 0.05) {
    outliers <- c(outliers,as.numeric(strsplit(grubbs.result$alternative," ")[[1]][3]))
    test <- x[!x %in% outliers]
    grubbs.result <- grubbs.test(test)
    pv <- grubbs.result$p.value
  }
  return(data.frame(X=x,Outlier=(x %in% outliers)))
}
grubbs.flag(insurance$bmi)
hist(insurance$expenses)

normalize <- function(x) {
  return ( (x-min(x)) / (max(x) - min(x))  ) }

insurance_n  <- as.data.frame(lapply(insurance[,c(1,3,4,7)]  ,normalize))

cor( insurance[    , c("age","bmi","children","expenses")] ) 
# 어떤 상관관계도 강하지 않은 것으로 볼 수 있지만 일부 눈에 띄는 연관성이 있음. age와 bmi는 약한 양의 상관관계

# psych 패키지를 이용해서 위의 상관관계를 시각화할 수 있다
install.packages('psych')
library(psych)
dev.new()
pairs.panels(insurance[c('age','bmi','children','expenses')])

m5 <- lm( expenses ~ age + children + bmi +smoker +region,  data=insurance)
m5

#또는
attach(insurance)
lm(expenses ~ .,  data= insurance)  

rs_m <- lm(expenses~., data=insurance)
summary(rs_m)
