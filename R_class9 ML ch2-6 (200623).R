getwd()

emp<-read.csv('emp3.csv',head=T)
sal<-c()
for (i in 1:length(emp$sal)){
  sal[i]<-emp$sal[i]
}
sal

#207
emp<-read.csv('emp3.csv',head=T)
sal_sale<-c()
cnt<-1
for (i in 1:length(emp$sal)){
  if (emp$job[i]=='SALESMAN'){
    sal_sale[cnt]<-emp$sal[i]
    cnt<-cnt+1
  }
}
sal_sale

#208
dept_sal<-c()
cnt<-1
for (i in 1:length(emp$sal)){
  if (emp$deptno[i]==20){
    dept_sal[cnt]<-emp$sal[i]
    cnt<-cnt+1
  }
}
sum(dept_sal)

#209
income_deptno<-function(){
  dept<-readline()
  rs<-c()
  cnt<-1
  for (i in 1:length(emp$sal)){
    if (emp$deptno[i]==dept){
      rs[cnt]<-emp$sal[i]
      cnt<-cnt+1
    }
  }
  print(sum(rs))
}
income_deptno()

#210
x <- c()
for  ( i in 1:length(emp$sal)){
  x[i] <-  emp$sal[i]
}
cat('최대값', max(x), '\n', 
    '최소값', min(x), '\n',
    '평균값', mean(x),'\n',
    '중앙값', median(x),'\n',
    '분산값', var(x) , '\n',
    '표준편차',sd(x) )

#211
stats<-function(){
  tb <- readline(prompt = 'which table?')
  col <- readline(prompt = 'which column?')
  table_name <- get(tb)
  data <- table_name[,col]
  rs <- c()
  for (i in 1:length(data)){
    rs[i]<-data[i]
  }
  cat('max', max(rs), '\n', 
      'min', min(rs), '\n',
      'mean', mean(rs),'\n',
      'median', median(rs),'\n',
      'var', var(rs) , '\n',
      'std',sd(rs) )
}
stats()

###### 탐색기 활성화
fname<-file.choose()
fname
table<-read.csv(fname,head=T)
head(table)

#212
stats<-function(){
  tb <- file.choose()
  table <- read.csv(tb, head=T)
  print(colnames(table))
  col <- readline(prompt = 'which column?')
  data <- table[,col]
  rs <- c()
  for (i in 1:length(data)){
    rs[i]<-data[i]
  }
  cat('max', max(rs), '\n', 
      'min', min(rs), '\n',
      'mean', mean(rs),'\n',
      'median', median(rs),'\n',
      'var', var(rs) , '\n',
      'std',sd(rs) )
}
stats()

#213
stats<-function(){
  tb <- file.choose()
  table <- read.csv(tb, head=T)
  print(colnames(table))
  col <- readline(prompt = 'which column?')
  data <- table[,col]
  rs <- c()
  for (i in 1:length(data)){
    rs[i]<-data[i]
  }
  cat('max', max(rs), '\n', 
      'min', min(rs), '\n',
      'mean', mean(rs),'\n',
      'median', median(rs),'\n',
      'var', var(rs) , '\n',
      'std',sd(rs),'\n',
      'out',rs$out)
  
  boxplot(rs,main=list(paste(col,'Summary')))
}
stats()

source('myfunc.R')
