

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

#84
rs=tapply(sal,list(loc,year(hiredate)),sum)
rs[is.na(rs)]=0
rs

#85
barplot(rs, col=rainbow(4), legend=rownames(rs),beside=T,args.legend=list(x='topright',bty='n',inset=c(-0.01,0)))

#87
install.packages("")
line=read.csv('1-4호선승하차승객수.csv',header=T)
t1 <- gvisMotionChart(line, idvar="line_no", timevar="time")
plot(t1)
