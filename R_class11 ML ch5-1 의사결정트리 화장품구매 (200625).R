(-2/5)*log2(2/5)+(-3/5)*log2(3/5)
((-2/3)*log2(2/3)+(-1/3)*log2(1/3))*3/5
  source(my_func.R)
(-10/16)*log2(10/16)+(-6/16)*log2(6/16)

skin <- read.csv('skin.csv',header=T, stringsAsFactors = T)
str(skin)
head(skin)

install.packages('FSelector')
library(FSelector)

weights <- information.gain(cupon_react~., skin, unit='log2')
print(weights)

fat <- read.csv('fatliver2.csv',head=T,stringsAsFactors = T)
str(fat)

rs <- information.gain(FATLIVER~., fat, unit='log2')
print(rs)


install.packages("C50")
library(C50)

skin <- read.csv("skin.csv", head=T ,stringsAsFactors = T)

skin_real_test_cust <- skin[30,  ]  # 나중에 모델 만들고 정확도를 올린후에 최종적으로 모델이 잘 맞추는지 확인하려 한건 제외 시킨다. 
skin2 <-  skin[ 1:29, ] 

skin_real_test_cust

skin2 <- skin2[ , -1] # 고객번호를 제외시킨다. 

set.seed(20)
skin2_shuffle <- skin2[sample(nrow(skin2)),    ]  # shuffle 시킴 


train_num <-  round(0.7 * nrow(skin2_shuffle), 0) 

skin2_train <- skin2_shuffle[1:train_num,  ]  

skin2_test  <- skin2_shuffle[(train_num+1) : nrow(skin2_shuffle), ] 


library(C50)
skin_model <- C5.0(skin2_train[  , -6],  skin2_train$cupon_react )  
skin_model
summary(skin_model)

skin2_result  <- predict( skin_model , skin2_test[  , -6])

library(gmodels)
CrossTable( skin2_test[  , 6],  skin2_result ) 

skin_model2 <- C5.0(skin2_train[  , -6],  skin2_train$cupon_react,trials=10 )  
skin2_result  <- predict( skin_model2 , skin2_test[  , -6])
CrossTable( skin2_test[  , 6],  skin2_result ) 
