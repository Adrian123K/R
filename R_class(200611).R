setwd("d:/R")
getwd()

library(data.table)
install.packages("data.table")

emp<-read.csv('emp3.csv',header=T)
emp

data.table(paste(emp$ename,'의 직업은',emp$job))

emp[emp$job %in% c('SALESMAN','ANALYST'),c('ename','sal','job')]

emp[!emp$job %in% c('SALESMAN','ANALYST'),c('ename','sal','job')]

emp[is.na(emp$comm), c('ename','sal','comm')]

emp[!is.na(emp$comm),c('ename','sal','comm')]

emp[grep("^A",emp$ename),c('ename','sal')]

emp[grep('T$',emp$ename),'ename']

emp[grep('^*.M',emp$ename),c('ename','sal')]

emp[grep('^..L',emp$ename),c('ename','sal')]

data.frame("부서번호" =unique(emp$deptno))

rs=emp[emp$job=='SALESMAN',c('ename','sal','job')]
rs[order(rs$sal,decreasing = T),]

ls()
rm(x)

install.packages("doBy")
library(doBy)

install.packages("rlang")

orderBy(~-sal,emp[emp$deptno==20,c('ename','sal')]) # 내림차순
orderBy(~sal,emp[emp$deptno==20,c('ename','sal')]) # 오름차순

c_loc=read.csv('crime_loc.csv',header = T)
c_loc

head(c_loc)

rs=c_loc[c_loc$범죄=='살인',]
orderBy(~-건수,rs)

data.frame('범죄'=unique(c_loc$범죄))

data.frame(이름=tolower(emp$ename),직업=tolower(emp$job))

substr(emp[emp$ename=='SMITH','ename'],2,4)

10%%3

library(doBy)
orderBy(~-연봉, data.frame(이름=emp$ename, 연봉=emp$sal*12))

Sys.Date()
data.frame(근무일=Sys.Date()-as.Date(emp$hiredate))

ceiling_date(Sys.Date(),'month')

last_day=function(x){
  ceiling_date(x,'month')-days(1)
}
last_day(Sys.Date())

first_day=function(x){
  floor_date(x,'month')
}
first_day(Sys.Date())

