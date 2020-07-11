sms_result <- read.csv('sms_results.csv',stringsAsFactors = T)
head(sms_result)

library(gmodels)
CrossTable(sms_result[,2],sms_result[,1])

head(sms_result)
CrossTable(sms_result$predict_type,sms_result$actual_type)

install.packages('vcd')
library(vcd)

table(sms_result$predict_type,sms_result$actual_type)
Kappa(table(sms_result$predict_type,sms_result$actual_type))

2*(156/1390)*(152/156)/(0.8306+(156/1390))

install.packages('caret')
library(caret)

sensitivity(sms_result$predict_type,sms_result$actual_type,positive = 'spam')
specificity(sms_result$predict_type,sms_result$actual_type,negative = 'ham')
precision <- posPredValue(sms_result$predict_type,sms_result$actual_type,positive = 'spam')
recall <- sensitivity(sms_result$predict_type,sms_result$actual_type,positive = 'spam')

install.packages('ROCR')
library(ROCR)

head(sms_result)
pred <- prediction(predictions = sms_result$prob_spam,
                   labels = sms_result$actual_type)
pred

# ROC curves
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
dev.new()
plot(perf, main = "ROC curve for SMS spam filter", col = "blue", lwd = 2)

# add a reference line to the graph
# 대각선 출력 
abline(a = 0, b = 1, lwd = 2, lty = 2)

# calculate AUC
perf.auc <- performance(pred, measure = "auc")
str(perf.auc)
unlist(perf.auc@y.values)


precision <- posPredValue(sms_result$predict_type,sms_result$actual_type,positive = 'spam')
recall <- sensitivity(sms_result$predict_type,sms_result$actual_type,positive = 'spam')
Fmeasure <- 2*precision*recall/(precision+recall)
Fmeasure
