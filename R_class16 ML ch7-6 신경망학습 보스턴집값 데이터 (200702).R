boston<-read.csv("boston.csv", stringsAsFactors=T)
str(boston)
head(boston)

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

boston_norm <- as.data.frame(lapply(boston, normalize))
summary(boston_norm$MEDV)
summary(boston$MEDV)

hist(boston$MEDV)

dim(boston_norm)

set.seed(1)
s_cnt<-round(0.7*(nrow(boston_norm)))
s_index<-sample(1:nrow(boston_norm), s_cnt, replace=F)

boston_train <- boston_norm[s_index, ]
boston_test <- boston_norm[-s_index, ]

head(boston_train)

library(neuralnet)

boston_model <- neuralnet(MEDV~CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO+B+LSTAT, data=boston_train, hidden=10)

model_results <- compute(boston_model, boston_test[1:13])

predicted_houseprice <- model_results$net.result
cor(predicted_houseprice, boston_test$MEDV)

# 275
boston_model2 <- neuralnet(MEDV~CRIM+ZN+INDUS+CHAS+NOX+RM+AGE+DIS+RAD+TAX+PTRATIO+B+LSTAT, 
                           data=boston_train, hidden=c(10, 5), rep=10)
model_results2 <- compute(boston_model2, boston_test[1:13])

predicted_houseprice2 <- model_results2$net.result

cor(predicted_houseprice2, boston_test$MEDV)
