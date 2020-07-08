body <- read.csv("kbody2.csv", header = F,stringsAsFactors = T)

library(e1071)

body1 <- na.omit(body)

colnames(body1) <- c("gender", "age", "height", "chest", "heory", "bae", "ass", "kyeo",
                     "face_vertical", "head", "bone", "body_fat", "body_water",
                     "protein", "mineral", "body_fat_per", "bae_fat_per", "work", "bae_fat_test",
                     "work_test")

set.seed(12345)

body_ran <- body1[order(runif(12894)), ]

body_train <- body_ran[1:10314, ]

body_test <- body_ran[10315:12894, ]

body_svm <- svm(work_test~., data = body_train, kernel='rbf')
body_svm

p <- predict(body_svm, body_test, type="class")
p

table(p, body_test[, 20])

mean(p == body_test[, 20])

# 301
body_svm <- svm(work_test~., data = body_train, kernel='radial')

p <- predict(body_svm, body_test, type="class")

table(p, body_test[, 20])

mean(p == body_test[, 20])

# caret
ctrl <- trainControl(method='cv', number=10)
bagctrl <- bagControl(fit=svmBag$fit,
                     predict=svmBag$pred,
                     aggregate=svmBag$aggregate)
set.seed(300)
svmbat <- train(work_test~., data=body_train, trControl=ctrl, bagControl=bagctrl)
