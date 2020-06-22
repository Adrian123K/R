getwd()

d1=read.csv("data1.csv")
d1
apply(d1[,c(2:15)],2,sum)
apply(d1[,c(2:15)],1,sum)

data2=read.csv("1-4호선승하차승객수.csv",stringsAsFactors=F)
View(data2)
class(data2)
str(data2)
# stringsAsFactors(): dataFrame으로 쓸때는 factor로 들어오지 않도록 바꿔줘야함 -> character로 변경

attach(data2)
# 노선 번호별 승차인원수 합계
tapply(승차,노선번호,sum)
# 노선 번호별 하차 인원수 합계
tapply(하차,노선번호,sum)
# 노선 번호 상관없이 승하차 인원수 합계
sapply(data2[,c(3,4)],sum)
# 노선 번호별 승하차 인원수 합계
aggregate(승차+하차~노선번호,data2,sum)
# 노선 번호별 승차 인원수 합계
aggregate(승차~노선번호,data2,sum)
# 노선 번호별 하차 인원수 합계
aggregate(하차~노선번호,data2,sum)

# Fruits 데이터셋에서 Expenses값이 80보다 큰 값을 출력
install.packages("googleVis")
library(googleVis)

attach(Fruits)
Fruits_2=filter(Fruits, Expenses>80);Fruits_2

# Expenses 값이 90보다 크고 Sales값도 90보다 큰 값을 출력
Fruits_3=filter(Fruits, Expenses>90 & Sales>90);Fruits_3
Fruits_4=filter(Fruits, Expenses>90 || Sales>90);Fruits_4
Fruits_5=filter(Fruits, Expenses %in% (c(79,91)));Fruits_5
select(Fruits,Fruit:Sales,-Location)

# Fruits 데이터셋에서 과일 이름별로 모아서 판매량을 합계 출력. NA는 자동 삭제
Fruits %>% group_by(Fruit) %>% summarise(sum_sales=sum(Sales,na.rm=T)) 

# Fruits 데이터셋에서 과일 이름별로 모아서 판매량(Sales)과 이익(Profit) 합계 출력
Fruits %>% group_by(Fruit) %>% summarise_each(funs(sum),Sales,Profit)

# reshape2() 패키지: column이 많은 형태(wide)를 세로로 긴(long) 형태로 변경. 반대로도 가능
# melt, cast함수가 가장 많이 사용
install.packages("reshape2")
library(reshape2)

fruits=read.csv("fruits_10.csv", header=T)
fruits

# melt: wide -> long
melt(fruits, id='year')
melt(fruits, id=c('year','name'))
melt(fruits, id=c('year','name'), variable.name = 'var_name', value.name = 'val_name')

# cast: long -> wide
mtest=melt(fruits,id=c('year','name'), variable.name = 'var_name', value.name = 'val_name');mtest
# melt작업을 한 것을 dcast() 원래 모양으로 만듬
dcast(mtest,year+name~var_name)
# melt() 작업 시 사용한 column을 누락시키고 dcast()를 사용했을 경우  경고메시지 발생 -> 기존 사용한 column 전부 작성 필요
dcast(mtest,year~var_name)

library(plyr)
# dcast(data, 기준컬럼~대상컬럼, 함수)
dcast(mtest,name~var_name,sum)
dcast(mtest,name~var_name,sum, subset=.(name=='apple'))

# stringr() 패키지: 작업할 대상 데이터가 문자일 경우
install.packages("stringr")
library(stringr)

# str_detect(): 특정 문자 포함 여부 확인(true,false)
f=c('apple',"Apple","banana","pineapple")
str_detect(f,'A')
str_detect(f,'^a') # ^a: 첫 문자 a
str_detect(f,'e$') # e$: 끝 문자 e
str_detect(f,'^[aA]') # ^[aA]: 첫 문자 a or A
str_detect(f,'[aA]') # ^이 빠지면 단어에 a or A가 있는지 확인인

# ignore.case(): 대소문자 무시
str_detect(f,ignore.case('a'))

str_detect(f,fixed('a',ignore_case=T))

p='a.b'
s=c('abb','a.b')
str_detect(s,p) # 생김새 상관없이 순서만 일치하면 true
str_detect(s,fixed(p)) # 정확하게 일치하는 것만 검색
str_detect(s,coll(p))

# str_count(): 특정 문자 출현 횟수 
str_count(f,fixed('A',ignore_case=T))

# str_c(): 문자열 합치기
str_c('apple','pen')
str_c("Fruits:",f)
str_c(f," name is ", f)
str_c(f, collapse="")

# str_dup(): 반복
str_dup(f,3)

# str_length(): 문자열 길이
str_length('apple')
str_length(f)

# str_locate(): 특정 문자의 위치(문자열에서 특정 문자가 처음나오는 위치와 마지막 위치)
str_locate('apple','a') # start와 end 위치는 동일
str_locate(f,'a') # 첫번째나오는 문자의 위치만 출력

# str_replace(): 문자열 변경
str_replace('apple','p','*') # 처음 나오는 문자만 변경
str_replace_all('apple','p','*')

# str_split(): 데이터셋을 지정된 기호로 분리
f2=str_c('apple','/','orange','/','banana');f2
str_split(f2,'/')

# str_sub(): 지정된 길이만큼 문자를 잘라내기
str_sub(f2,start=1,end=3)
str_sub(f2,start=6,end=9)
str_sub(f2,start=-5)

# str_trim(): 문자열의 가장 앞과 뒤에 공백 제거
str_trim('             apple          banana        orange        ')

library(sqldf)
Fruits
str(Fruits)
7sqldf('select * from Fruits where Fruit=\'Apples\'')
sqldf('select * from Fruits where Fruit="Apples"')
sqldf('select * from Fruits limit 4')
sqldf('select * from Fruits order by year')
sqldf('select Fruit, sum(Sales) as Sales from Fruits')
sqldf('select Fruit, sum(Sales) as Sales from Fruits group by Fruit')

# 과일 이름별 평균 판매량 출력
sqldf('select Fruit, round(avg(Sales),2) as Avg_Sales from Fruits group by Fruit')
sqldf(c('update Fruits set Profit=50 where Fruit="Apples" and Year=2008','select * from Fruits'))
sqldf(c('delete from Fruits where Fruit="Apples" and Year=2008','select * from Fruits'))

# if조건문: 조건이 2개인 경우
# if(조건식){조건문이 참일 경우 실행될 실행문} else {조건문이 거짓일 경우 실행될 실행문}
plus=function(x){
  if(x<0){
    return(-x)
  }else{
    return(x)
  }
}
plus(-3)

# 입력된 숫자가 양수이면 제곱으로 출력, 숫자가 0보다 작거나 같으면 0으로 출력
f2=function(x){
  if(x>0){
    return(x^2)
  }else{
    return(x=0)
  }
}
f2(-3)
f2(3)
f2(10)
f2(-1000000000000)

# 입력된 숫자가 0보다 크면 2배값, 0일경우 0, 0보다 작을 경우 -2배의 값 출력
f3=function(x){
  if(x>0){
    x=x*2
    return(x)
  } else if(x==0){
    x=0
    return(x)
  } else {
    x=x*-2
    return(x)
  }
}
f3(-4)
f3(0)
f3(100)

# ifelse(a,b,c): a가 참이면 b, 거짓이면 c를 출력
no=scan()
no
ifelse(no%%2==0,'짝수','홀수')

# 서로 다른 두개의 숫자를 입력받아서 두 숫자 사이에 존재하는 숫자의 개수 출력
f4=function(x,y){
  x=abs(x)
  y=abs(y)
  t=x-y
  t=abs(t)-1
  return(t)
}
f4(3,5)
f4(100,2)

num=scan()
ifelse(num[1]-num[2]>0,num[1]-num[2]-1,num[2]-num[1]-1)

# 반복문 for, while
for1=function(x){
  i=0
  for(j in 1:x){
    i=i+j
    print(i)
  }
}
for1(10)

myf=function(a,b){
  if(a>0&&b>0){
    c=a*b
    return(c)
  } else {
    c=a+b
    return(c)
  }
}

myf(3,5)
myf(1,-2)
myf(0,-5)
myf(-3,-9)

var1=readLines("채소.txt")
length(var1)
i=0
while(i<length(var1)){
  i=i+1
  if(var1[i] =='버섯'){
    next
  } else {
    print(var1[i])
  }
}

# 특정 패턴만 골라내기: grep(pattern,a) vector a에서 특정 패턴을 찾아 그 위치를 나타냄
c1=c('apple','Apple','APPLE','banana','grape')
grep('apple',c1)
c2=c('apple','banana')
grep(c2,c1)   
grep(paste(c2,collapse='|'),c1,value=T) # 두가지 패턴을 동시에 찾을 때 collapse='|' 삽입
grep('pp',c1,value=T) # value -> 값 자체를 출력
grep('^A',c1,value=T)
grep('e$',c1,value=T)

c2=c('grape1','apple1','apple','orange','Apple')
grep('ap',c2,value=T)
grep('[0-9]',c2,value=T)
grep('[A-Z]',c2,value=T)
grep('[[:upper:]]',c2,value=T)

# nchar(a): 입력된 배열이나 문자열의 길이
nchar(c1)
nchar('홍길동')

# paste('a','b','c'): a,b,c를 합쳐서 하나의 문자열로 출력
paste("홍","길","동",sep='')

# substr('a',시작위치,끝위치): 특정 부분만 출력
substr('abc123',3,5)

# strsplit("문자열",split="기준문자"): 특정 글자를 기준으로 분리
strsplit("2020/02/23",split="/")

# 특정 패턴 찾기: regexpr("pattern",text)
grep('-','010-1111-1111')
regexpr('-','010-1111-1111')

# 결측치(NA): 누락된 값, 비어있는 값
# 함수 적용 불가. 분석결과를 왜곡
# 반드시 제거 후 분석을 실시
df=data.frame(sex=c('M','F',NA,'M','F'),
              score=c(5,4,7,8,NA))
df

is.na(df)
table(is.na(df)) # 빈도수 확인
table(is.na(df$sex))
table(is.na(df$score))

mean(df$score)

df %>% filter(is.na(score))
df %>% filter(!is.na(score))
df %>% filter(is.na(sex))
df %>% filter(!is.na(sex))

df_ns=df %>% filter(!is.na(score))
str(df_ns)
mean(df_ns$score)
sum(df_ns$score)

df_ns=df %>% filter(!is.na(score)&!is.na(sex));df_ns

df_ns2=na.omit(df) # 결측치 없는 데이터만 추출
df_ns2

mean(df$score, na.rm=T)
sum(df$score, na.rm=T)

exam=read.csv("csv_exam.csv")
exam[c(3,8,15),"math"]=NA;exam
exam %>% summarise(count_std=length(exam$id),
                   mean_math=mean(math,na.rm=T),
                   sum_math=sum(math,na.rm=T),
                   median_math=median(math,na.rm=T))

# 결측치 대체법: 대표값(평균,최빈)으로 일괄대체, 통계분석기법 적용(예측값 추정하여 대체)
mathmean=sum(exam$math,na.rm=T)/length(exam$id);mathmean
exam$math=ifelse(is.na(exam$math),47,exam$math);table(is.na(exam$math))
exam$math
mean(exam$math)
boxplot(exam$math)
