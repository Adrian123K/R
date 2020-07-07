ret_err <- function(n,err) {
  sum <- 0 
  for(i in floor(n/2):n) { 
    sum <- sum + choose(n,i) * err^i * (1-err)^(n-i)
  }
  sum
}

for(j in 1:60) {
  err <- ret_err(j , 0.4)
  cat(j,'--->',1-err,'\n') 
  if(1-err >= 0.9) break
}

## Bagging 실습
install.packages('ipred')
library(ipred)

credit <- read.csv('credit.csv',stringsAsFactors = T)

set.seed(300)
mybag <- bagging(default~., data=credit, nbagg=25) # nbagg=25 : 앙상블에 사용되는  bag의 개수
credit_pred <- predict(mybag, credit)
table(credit_pred,credit$default)
prop.table(table(credit_pred==credit$default))

# 299
mybag <- bagging(default~., data=credit, nbagg=50)
credit_pred <- predict(mybag, credit)
table(credit_pred,credit$default)
prop.table(table(credit_pred==credit$default))


## Boosting 실습
install.packages('adabag')
library(adabag)

set.seed(300)

credit <- read.csv('credit.csv',stringsAsFactors = T)
m_adaboost <- boosting(default~., data=credit)
p_adaboost <- predict(m_adaboost, credit)

head(p_adaboost$class)
p_adaboost$confusion
table(p_adaboost$class,credit$default)


# 300
#install.packages('adabag')
#library(adabag)

set.seed(300)

mushroom <- read.csv('mushrooms.csv',stringsAsFactors = T)
str(mushroom)
m_adaboost <- boosting(type~., data=mushroom)
p_adaboost <- predict(m_adaboost, mushroom)

table(p_adaboost$class,mushroom$type)
