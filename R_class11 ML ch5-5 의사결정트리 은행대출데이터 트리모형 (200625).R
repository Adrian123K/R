install.packages('caret')
install.packages('rpart')
install.packages('rpart.plot')

# 의사결정트리 알고리즘을 이용하여 은행 대출 채무 이행/불이행 여부 예측

credit = read.csv('credit.csv',header=T,stringsAsFactors = T)

# 데이터 형태 확인

str(credit) # 수치형, 명목형 섞여있음


# 데이터 분류 (caret 사용)

library(caret)

set.seed(5)
intrain = createDataPartition(credit$default,p=0.9,list=F)

# train(90%) / test(10%)

credit_train = credit[intrain,]

credit_test = credit[-intrain,]


# 의사결정트리 모델 생성

library(C50)

credit_model = C5.0(default~.,data=credit_train,trials=24) # 24 : 0.87
credit_result = predict(credit_model,credit_test[,-17])

# 이원 교차표로 결과 확인

library(gmodels)

x = CrossTable(credit_test[,17],credit_result)

library(rpart)
library(rpart.plot)

rpartmod = rpart(default~., data=credit_train, method='class')
dev.new()
rpart.plot(rpartmod)
savePlot('dcs_tree.png', type='png')

x$prop.tbl[1]+x$prop.tbl[4] # 0.87
