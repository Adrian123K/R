#182
class1<-c( rep(19,3), rep(20,6), rep(21,3), 145, 147 )
median(class1)
summary(class1)

#183
summary(car$price)
summary(car$mileage)

library(plotly)

car<-read.csV('usedcars.csv',head=T)
car
summary(car)
dev.new()
rs1<-plot_ly(x=~car$price, type='histogram') %>% 
  layout(title='Histogram of used cars prices',xaxis=list(title='Used Car Price',zerolines=F),yaxis=list(title='Count',zerolines=F))
rs1



boxplot(car$mileage)$out

hist(car$price)
par(new=T)
par(mfrow=c(1,2))
hist(car$mileage)

#185
install.packages("fBasics")
library(fBasics)

skewness(car$mileage) # 왜도값 > 0 : 오른쪽으로 꼬리가 긴 경우
skewness(car$price) # 왜도값 < 0 : 왼쪽으로 꼬리가 긴 경우

#186
par(mfrow=c(1,1))
class2<-sort(car$mileage)
hist(class2, axes=F)
par(new=T)
plot(class2, dnorm(class2, mean=mean(class2), sd=sd(class2)), type='l', main='Mileage Normal Distribution Graph')

#187
class3<-sort(iris$Sepal.Length)
plot(class3, dnorm(class3, mean=mean(class3),sd=sd(class3)),type='l', main='Iris Norm. Graph')

rs1<-plot_ly(x=~car$price, type='histogram') %>% 
  layout(title='Histogram of used cars prices',xaxis=list(title='Used Car Price',zerolines=F),yaxis=list(title='Count',zerolines=T))
rs1


#190
emp<-read.csv('emp3.csv',head=T)
plot(emp$comm, emp$sal)

emp$comm[is.na(emp$comm)]<-0
cor(emp$comm,emp$sal) # 아주 약한 음의 상관관계를 보이고 있다

#191
plot(car$mileage, car$price, pch=21, col='red',bg='red')
cor(car$mileage, car$price) # 강한 음의 상관관계

install.packages("gmodels")
library(gmodels)

attach(emp)
tapply(empno, list(deptno,job), length, default=0)

CrossTable(emp$deptno, emp$job)

library(data.table)

data.frame(월급=emp$sal, '2500이상'=emp$sal>=2500)
emp$sal_tf<-emp$sal>=2500
emp
CrossTable(emp$job, emp$sal_tf)

#195
car$conservative<- car$color %in% c('Black','Gray','Silver','White')
table(car$conservative)
CrossTable(car$model, car$conservative)

print(1-pchisq(q=8.33,df=1,lower.tail = T))
