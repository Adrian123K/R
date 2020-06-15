

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
library(googleVis)
line=read.csv('1-4호선승하차승객수.csv',header=T)
t1 <- gvisMotionChart(line, idvar="line_no", timevar="time")
plot(t1)

#88
line2=read.csv('서울지하철_5-8호선_이용현황_시간대별.csv',header=T)
t2 = gvisMotionChart(line2, idvar="호선명", timevar="시간")
plot(t2)

emp
rbind(
  emp[ emp$deptno %in% c(10,20), c("ename","sal","deptno") ],
  emp[ emp$deptno ==10, c("ename","sal","deptno") ] 
  )

#90
x=rbind(aggregate(sal~deptno,emp,sum),c('total',sum(emp$sal)))
names(x)=c('부서번호','토탈월급')
x

install.packages("doBy")
library(doBy)

#91
rs=rbind(aggregate(sal~job,emp,sum),c('',sum(emp$sal)))
names(rs)=c('직업','토탈값')
rs

library(doBy)
x2=setdiff(
  emp[emp$deptno %in% c(10,20),c('ename','sal','deptno')],
  emp[emp$deptno==10,c('ename','sal','deptno')])
orderBy(~ename,x2)

univ<-read.csv('전국_대학별등록금통계_현황.csv', stringsAsFactors=F,header=T)
head(univ)

#94
data.frame(사원명=emp[emp$empno %in% emp$mgr,'ename'])

#97
cday=read.csv('crime_day.csv',header=T)
cday<-cday[trimws(cday$C_C)=='강력범죄',]
cday[cday$CNT==max(cday$CNT),]

x=data.frame(이름=emp$ename, 월급=emp$sal, 순위=rank(-emp$sal, ties.method = 'min', ))
orderBy(~순위,x)

x2=data.frame(이름=emp$ename, 월급=emp$sal, 순위=dense_rank(-emp$sal))
orderBy(~순위,x2)

#99
rs=(data.frame(이름=emp$ename, 월급=emp$sal,직업=emp$job, 순위=dense_rank(-emp$sal)))
rs=orderBy(~순위,rs)
rs[rs$직업=='SALESMAN',]

#100
cancer=read.csv('cancer2.csv',header=T)
cdf=cancer[cancer$성별=='여자' & cancer$암종!='모든암',]
cdf=unique(cdf[!is.na(cdf$환자수),])
cdf
