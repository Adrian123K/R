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

# 파생변수 age2 추가
insurance$age2 <- insurance$age^2
insurance
cor( insurance[    , c("age","bmi","children","expenses",'age2')] ) 
dev.new()
pairs.panels(insurance[c('age','bmi','children','expenses','age2')])


attach(insurance)
rs_m2 <- lm(expenses ~ .,  data= insurance)
summary(rs_m2)

# 파생변수 bmi30 추가
insurance$bmi30 <- ifelse(insurance$bmi>=30,1,0)
head(insurance)

cor( insurance[    , c("age","bmi","children","expenses",'age2','bmi30')] ) 
dev.new()
pairs.panels(insurance[c('age','bmi','children','expenses','age2','bmi30')])

attach(insurance)
rs_m2 <- lm(expenses ~ .,  data= insurance)
summary(rs_m2)

# 상호작용 파생변수 추가
rs_m4 <- lm(expenses ~ age+age2+children+bmi+sex+bmi30*smoker+region,data=insurance)
summary(rs_m4)

#264
dev.new()
pairs.panels(insurance[c('age','bmi','children','','age2','bmi30','region','expenses')])
summary(insurance$age)
insurance$age3 <- ifelse(insurance$age>=39,1,0)
insurance$child2 <- ifelse(insurance$children==2,1,0)
insurance$region2 <- ifelse(insurance$region=='southeast',1,0)
insurance$sex2 <- ifelse(insurance$sex=='male',1,0)
cor( insurance[    , c('child2','age2','bmi30','region2','sex2','expenses')] ) 

rs_m5 <- lm(expenses ~ age+age2+children+bmi+sex+smoker*age3*child2+smoker*bmi30*child2*age3*region2,data=insurance)
summary(rs_m5)


insurance$region2 <- ifelse(insurance$region == "northeast", 1, 0)
insurance

model_yj <- lm(expenses ~ age + age2 + children + bmi + sex + bmi30*smoker+region2*age, data=insurance)
summary(model_yj)
