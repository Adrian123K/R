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

#59
attach(emp)
tapply(sal,job,mean)
trunc(tapply(sal,job,mean))

#61
x=trunc(tapply(sal,job,mean))
barplot(x,main='연도별 평균월급',col=rainbow(5),density=80)

#62
attach(emp)
tapply(sal,list(job,deptno),sum)

#63
rs5=tapply(sal,list(job,deptno),sum)
rs5[is.na(rs5)]=0
rs5

#65
barplot(rs5,col=rainbow(5),legend=rownames(rs5),beside=T)

x=tapply(emp$sal,emp$job,sum)
pie(x,col=rainbow(5),density=80)

library(plotrix)
pie3D(x,labels = rownames(x),explode = 0.2)

x2=aggregate(sal~job,emp,sum)
pct=round(x2$sal/sum(x2$sal)*100,1)
job_label=paste(x2$job,':',pct,'%')
job_label
pie3D(x,labels=job_label,explode=0.2,labelcex = 1)

#69
x=tapply(emp$empno,emp$deptno,length)
x3=aggregate(empno~deptno,emp,length)
pct=round(x3$empno/sum(x3$empno)*100,1)
dept_label=paste(x3$deptno,'번:',pct,'%')
pie3D(x,col=rainbow(3),explode=0.1,labels=dept_label,labelcex = 1.5)

#75
x=merge(emp,emp,by.x='mgr',by.y='empno')
data.frame(x$ename.x, x$ename.y)

#77
a=x[,c('ename.x','ename.y')]
b=graph.data.frame(a,direct=T)
dev.new()
plot(b)
plot(b, layout=layout.fruchterman.reingold, vertex.size=8, edge.arrow.size=0.5)



library(googleVis)
a <- merge(emp,emp, by.x="empno",by.y="mgr", all.y=T)
org <- gvisOrgChart(a, idvar="ename.y",parentvar="ename.x", options=list(width=600, height=250, size='middle',allowCollapse=T))
plot(org)
