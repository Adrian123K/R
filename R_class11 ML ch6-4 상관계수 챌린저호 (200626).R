# 1. 데이터를 로드한다. 
launch <- read.csv("challenger.csv")
launch

# 2. lm 회귀함수로 기울기와 절편을 구한다. 
attach(launch)
lm( distress_ct ~ temperature,  launch )
#       ↑              ↑
#       y축            x축 

# Call: lm(formula = distress_ct ~ temperature, data = launch)
# Coefficients: (Intercept)  temperature  
#               3.69841     -0.04754  
#                 ↑           ↑
#               절편         기울기 

plot( distress_ct ~ temperature,  data=launch,  col="red", bg="red", pch=21)
m <- lm( distress_ct ~ temperature, launch)

abline( m ,  col="blue")  

title(paste('파손수=', round(m$coefficients[1], 4), "* 온도 + ", 
            round(m$coefficients[2], 4)))
cor(launch$temperature,launch$distress_ct)
