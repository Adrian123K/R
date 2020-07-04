buy <- data.frame(
    cust_name=c('SCOTT','SMITH','ALLEN','JONES','WARD'),
    card_yn=c('Y','Y','N','Y','Y'),
    intro_yn=c('Y','Y','N','N','Y'),
    before_buy_yn=c('Y','Y','Y','N','Y'),
    buy_yn=c('Y','Y','N','Y','Y') )

rs <- information.gain(buy_yn~., buy, unit='log2')
print(rs)

# 1. 데이터를 로드한다. 
reg <- read.table("regression.txt", header=T) 
reg

#  2. 데이터를 시각화 한다. 
attach(reg)
plot(growth~tannin, data = reg, pch=21, col='blue', bg='red')
#       y ~ x 

# 3. 회귀분석을 해서 회귀 계수인 기울기와 절편을 구하시오 !
m <- lm( growth ~ tannin, data=reg)
#    ↑       ↑        ↑
#  reg함수  종속    독립 
m # 회귀계수-> 직선의 기울기

# 4. 위의 산포도 그래프에 회귀 직선을 그린다. 
abline( m ,  col='red')  

# 5. 그래프 제목을 회귀 직선의 방정식으로 출력되게한다. 
title(paste( '성장률=', round(m$coefficients[2], 4), "* 탄닌 + ", round(m$coefficients[1], 4)))

# 6. 위의 그래프에 잔차를 그린다. 
y_hat <- predict( m, tannin=tannin) # 
y_hat

join <- function(i){
  lines( c(tannin[i], tannin[i]), c( growth[i],y_hat[i]), col="green")
}
sapply(1:9, join) # 
