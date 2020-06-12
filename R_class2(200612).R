emp=read.csv('emp3.csv',header=T)
emp

library(doBy)
library(data.table)

orderBy(~-보너스,data.frame(이름=emp$ename,직업=emp$job,보너스=ifelse(emp$job=='SALESMAN',4000,ifelse(emp$job=='ANALYST',6000,0))))

