reg_func <- function(){
  
  insurance <- read.csv('insurance.csv', header=T)
  insurance$bmi30 <- ifelse(insurance$bmi>=30,1,0)
  
  rs_m <- lm(expenses ~ age+age^2+children+bmi+sex+bmi30*smoker+region,data=insurance)
  
  print(summary(rs_m))
  test <- data.frame(age=as.numeric(readline('나이는 어떻게 되십니까? ')),
                     sex=readline('성별이 무엇입니까?(male/female) '),
                     bmi=as.numeric(readline('비만지수는 어떻게 되십니까?(16~59) ')),
                     children=as.numeric(readline('부양가족수는 몇 명입니까? ')),
                     smoker=readline('흡연을 하십니까?(yes/no) '),
                     region=readline('사는 지역은 어디십니까?(southwest/southeast/northwest/northeast) ')
  )
  test$bmi30 <- ifelse(test$bmi>=30,1,0)
  
  rs_p <- predict(rs_m, test)
  print(paste('연간 의료비가 ',round(rs_p,2),'로 예상됩니다.'))
}

reg_func()

