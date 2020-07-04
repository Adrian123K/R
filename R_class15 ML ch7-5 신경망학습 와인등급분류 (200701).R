install.packages("nnet")
library(nnet)
library(gmodels)

wine <- read.csv("wine.csv",stringsAsFactors = T)
head(wine)
str(wine)

wine.scale <- cbind(wine[1], scale(wine[-1]))
summary(wine.scale)
size <- nrow(wine.scale) # 178

set.seed(100)
index <- c(sample(1:size, size * 0.7))
train <- wine.scale[index, ]
test <- wine.scale[-index, ]

model.nnet2 <- nnet(Type ~ ., data = train, size = 2, decay = 5e-04, maxit = 200)
predicted <- predict(model.nnet2, test, type = "class")
predicted

actual <- test$Type

model.confusion.matrix <- table(actual, predicted)
CrossTable(model.confusion.matrix)
