# 1. 데이터를 로드한다. 
adv <- read.csv("simple_hg.csv", header=T) 
adv
str(adv)

#  2. 데이터를 시각화 한다. 
attach(adv)
plot(input~cost, data = adv, pch=21, col='blue', bg='red')
#       y ~ x 

# 3. 회귀분석을 해서 회귀 계수인 기울기와 절편을 구하시오 !
adv_m <- lm( input ~ cost, data=adv)

adv_m # 회귀계수-> 직선의 기울기

# 4. 위의 산포도 그래프에 회귀 직선을 그린다. 
abline( adv_m ,  col='red')  

# 5. 그래프 제목을 회귀 직선의 방정식으로 출력되게한다. 
title(paste( '증가율=', round(adv_m$coefficients[2], 4), "*광고비+ ", round(adv_m$coefficients[1], 4)))

# 6. 위의 그래프에 잔차를 그린다. 
y_hat <- predict( adv_m, cost=cost) # 
y_hat

join <- function(i){
  lines( c(cost[i], cost[i]), c( input[i],y_hat[i]), col="green")
}
sapply(1:nrow(adv), join) # 
