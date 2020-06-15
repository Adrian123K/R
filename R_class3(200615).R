

getwd()

emp

attach(emp)

aggregate(sal~job, emp, sum, na.action = na.pass)

tapply(sal, job, sum)

#79
dept=read.csv('dept.csv',header=T)
x=merge(emp,dept,by='deptno',all=T)
aggregate(sal~loc, x, sum, na.action = na.pass)
attach(x)
tapply(sal, loc, sum)

x2=aggregate(sal~dname, x, mean, na.action = na.pass)
x2$sal=round(x2$sal)
x2
attach(x)
round(tapply(sal, dname, mean))

#81
x3=tapply(x$sal,x$dname,mean)
barplot(x3, ylim=c(0,3000),col=rainbow(4))

#82
library(googleVis)

x2
x3=data.frame(x2)
x4=gvisBarChart(x3)
plot(x4)

#83
library(lubridate)
year(x$hiredate)  

attach(x)
tapply(sal,list(loc,year(hiredate)),sum)

