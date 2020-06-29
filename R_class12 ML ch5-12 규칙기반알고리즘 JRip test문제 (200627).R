data.frame(colnames(mushroom))
head(mushroom,n = 1)

install.packages("RWeka")
library(RWeka)

model3 <- JRip(type~ ., data=mushroom_train)

fname <- file.choose()
table <- read.csv(fname, header=T, stringsAsFactor=T )

rs2 <- predict(model3,table[ , -1] )
rs2

head(mushroom[mushroom$type=='poisonous',],1)
str(mushroom)


test <- read.csv('mushrooms_test1.csv',head=T,stringsAsFactors = T)
rs3 <- predict(model3,test[,-1])
rs3
