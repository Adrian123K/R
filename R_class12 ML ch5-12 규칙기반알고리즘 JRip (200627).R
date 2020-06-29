install.packages("RWeka")
library(RWeka)

model2 <-  JRip(type~ ., data=mushroom_train)
model2

summary(model2)  

# 작은 이원교차표가 하나 보임 

result2 <- predict( model2, mushroom_test[   , -1] )

library(gmodels)

CrossTable( mushroom_test[ , 1],  result2) 
