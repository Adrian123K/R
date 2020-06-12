emp=read.csv('emp3.csv',header=T)
emp

library(doBy)
library(data.table)

orderBy(~-보너스,data.frame(이름=emp$ename,직업=emp$job,보너스=ifelse(emp$job=='SALESMAN',4000,ifelse(emp$job=='ANALYST',6000,0))))

max(emp$sal)

data.frame(이름=emp$ename,커미션=ifelse(is.na(emp$comm,'no comm',emp$comm)))

aggregate(sal~job, emp, max)

aggregate(sal~deptno,emp,sum)

rs=aggregate(sal~deptno, emp, sum)
names(rs)=c('부서번호','토탈월급')
rs

orderBy(~-토탈월급,rs)

rs2=aggregate(empno~job,emp,length)
names(rs2)=c('직업','인원수')
rs2

rs3=aggregate(sal~deptno+job,emp,sum)
names(rs3)=c('부서번호','직업','토탈월급')
orderBy(~부서번호-토탈월급,rs3)

rs4=aggregate(sal~format(as.Date(emp$hiredate),'%Y'),emp,mean)
names(rs4)=c('입사년도','평균월급')
rs4

attach(emp)
tapply(sal,job,mean)
trunc(tapply(sal,job,mean))

x=trunc(tapply(sal,job,mean))
barplot(x,main='연도별 평균월급',col=rainbow(5),density=80)

