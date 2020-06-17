# https://m.blog.naver.com/rndrnjs2003/221330203042
getwd()
install.packages("dplyr")
install.packages("googleVis")
install.packages("plyr")
install.packages("lubridate")
install.packages("stringr")
install.packages("ggplot2")
install.packages("RColorBrewer")
install.packages("wordcloud")
library(dplyr)
library(googleVis)
library(plyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(RColorBrewer)
library(wordcloud)

# mpg데이터 이용, 분석
# Q1. displ(배기량)이 4이하인 자동차와 5이상인 자동차 중 어떤 자동차의 hwy가 평균적으로 더 높은가
# 평균이용.filter()이용
# Q2. 제조회사별 도시 연비. audi, toyota중 어느 manufaturer의 cty가 평균적으로 높은가
# 변수의 값이 문자라는 점이 차이
# Q3. chevrolet, ford, honda 고속도로 연비 평균. hwy전체 평균
# 여러 조건 중 하나이상 충족하면 추출하도록 filter() 구성. 추출 후 평균 구하기. %in% 이용하면 짧음.

# Q1
class(mpg)
str(mpg)
mpg_a = mpg %>% filter(displ <= 4)
mpg_b = mpg %>% filter(displ >= 5)
mean(mpg_a$hwy)
mean(mpg_b$hwy)

# Q2
mpg_audi = mpg %>% filter(manufacturer == 'audi')
mpg_toyota = mpg %>% filter(manufacturer == 'toyota')
mean(mpg_audi$cty)
mean(mpg_toyota$cty)

# Q3
mpg_hwyav = mpg %>% filter(manufacturer %in% c('chevrolet','ford','honda'))
mean(mpg_hwyav$hwy)



### 데이터 분석
# 1. 패키지 준비
# 2. 데이터 준비 
# 3. 데이터 검토
# 4. 변수명 바꾸기
# 5. 데이터 분석절차

# 1단계: 변수검토 및 전처리
# 2단계: 변수간 관계분석
# 6단계: 시각화

install.packages('foreign') # SPSS 불러오기용 패키지
library(foreign)
install.packages('readxl')
library(readxl)


wel=read.spss(file='Koweps_hpc10_2015_beta1.sav', to.data.frame = T)
wel1=wel

View(wel1)
dim(wel1) # row, col
str(wel1)
View(summary(wel1))

wel1=rename(wel1,
            sex=h10_g3,
            birth=h10_g4,
            marrage=h10_g10,
            religion=h10_g11,
            income=p1002_8aq1,
            code_job=h10_eco9,
            code_region=h10_reg7)

View(wel1)

class(wel1$sex)
table(wel1$sex) # 가설 설립 가능: 여자 신생아가 많다 or 노년층이 많다(오래산다)

# 결측치 -> 누락된 값, 비어있는 값. 함수 적용X, 분석결과 왜곡
# 반드시 제거 후 분석
table(is.na(wel1$sex))
wel1$sex=ifelse(wel1$sex==9,NA,wel1$sex) # 이상치 변경
table(is.na(wel1$sex))

wel1$sex=ifelse(wel1$sex==1,'M','F')
table(wel1$sex)
qplot(wel1$sex)

class(wel1$income)
summary(wel1$income)
hist(wel1$income)
qplot(wel1$income) + xlim(0,1000)


summary(wel1$income)
wel1$income=ifelse(wel1$income%in%c(0,9999),NA,wel1$income)
table(is.na(wel1$income))

spi=wel1 %>% filter(!is.na(wel1$income)) %>% group_by(sex) %>% summarise(mean_income=mean(income))
spi

ggplot(data=spi, aes(x=sex,y=mean_income))+geom_col() 

# 연령대별 수입
class(wel1$birth)
summary(wel1$birth)
qplot(wel1$birth)
table(is.na(wel1$birth))
wel1$birth=ifelse(wel1$birth==9999,NA,wel1$birth)
table(is.na(wel1$birth))
wel1$age=2015-wel1$birth+1 # col 추가
summary(wel1$age)
qplot(wel1$age)
ai=wel1 %>% filter(!is.na(income)) %>% group_by(age) %>%  summarise(mean_income=mean(income))
head(ai)

ggplot(data=ai, aes(x=age, y=mean_income))+geom_line() # geom : 그래프 형태

# plot() : 분포나 꺾은선 그래프
# plot(y, 옵션)
# plot(x, 옵션)
# plot(x,y,옵션)

# 인수
# main : 제목
# sub : 부제목
# xlab="", ylab="" 축에 사용될 문자열(label)
# ann=F : 축 제목 사용X
# axes=F : 축 표시X
# axis : x, y축을 사용자 지정값으로 표시

v1=c(2,2,2)
plot(v1)
x=1:3
y=3:1
plot(x,y, xlim=c(1,10), ylim=c(1,5), xlab="x축", ylab="y축", main="TEST",sub="200307")
plot.new()
dev.new()

# type: 그래프 타입
# o : 점과 선을 중첩
# p : 점
# l : 선
# b : 점과 선 (중첩X, 떨어짐)
# c : b에서 점 생략
# h : 각 점에서 x축까지 수직선 그래프
# s : 왼쪽기준 계단
# S : 오른쪽 기준 계단
# n : 축만 그림
# lty : 선 모양(0~6) 1: 실선, 2: 대쉬, 3:점선

v1=c(100,130,120,160,150)
plot(v1, type='o', col='red', ylim=c(0,200), axes=F,ann=F) 
axis(1, at=1:5, lab=c('MON','TUE','WED','THU','FRI'))
axis(2, ylim=c(0,200))
title(main='FRUIT', col.main='red', font.main=4)
title(xlab='DAY', col.lab='black')
title(ylab='PRICE', col.lab='blue')

# 그래프의 배치 조정 : mfrow
# par(mfrow=c(nr,nc)) nr:numrow, nc:numcol, par(파티션)
par(mfrow=c(1,3)) # 한 화면에 3개의 그래프 출력(한줄에 3개)
plot.new()
plot(v1, type='b')
plot(v1, type='s')
plot(v1, type='S')
plot(v1, type='l')
pie(v1)
plot(v1, type='o')
barplot(v1)
par(mfrow=c(1,1))

a=c(1,2,3)
plot(a, xlab='aaa')
par(mgp=c(3,2,1)) # mpg=c(제목 위치, 지표값 위치, 지표선 위치)
plot(a, xlab='aaa')

# oma(outside margine) : 그래프의 전체 여백 조정(여백 늘리기)
# oma(bot, left, top, right)
par(oma=c(0,0,0,0))
plot(a,xlab='aaa')

# 여러개의 그래프를 중첩으로 그리기
# par(new=T)
par(mfrow=c(1,1)) 
v1=c(1,2,3,4,5)
v2=c(5,4,3,2,1)
v3=c(3,4,5,6,7)
plot(v1, type='s', col='red', ylim=c(1,5))
par(new=T)
plot(v2, type='o', col='blue', ylim=c(1,5))
par(new=T)
plot(v3, type='l', col='green') 
# 라벨까지 중복으로 작성됨
plot.new()

plot(v1, type='s', col='red', ylim=c(1,10))
lines(v2, type='o', col='blue', ylim=c(1,5))
lines(v3, type='l', col='green', ylim=c(1,13))
# 범례 추가
# legend(x,y, 내용, cex, col, pch, lty) cex: 글자크기, pch: 선크기, lty:선모양
legend(4,9,c('v1','v2','v3'),cex=0.9,col=c('red','blue','green'), lty=1)

# barplot()
x=c(1,2,3,4,5)
barplot(x,horiz=T)

x=matrix(c(5,4,3,2),2,2)
par(oma=c(1,0.5,1,0.5))
barplot(x, names=c(5,3), col=c('red','blue'),horiz=T, xlim=c(0,10))

v1=c(100,120,140,160,180)
v2=c(120,130,150,140,170)
v3=c(140,170,120,110,160)
qty=data.frame(BANANA=v1, APPLE=v2, ORANGE=v3)
qty

# barchart를 그룹으로 묶어서 그릴땐 반드시 출력대상이 matrix일 때
barplot(as.matrix(qty), main="SALES FRUITS", beside=T, col=rainbow(nrow(qty)), ylim=c(0,400))
legend(14,400, c("MON","TUE","WED","THU","FRI"), cex=0.8, fill=rainbow(nrow(qty)))

# 전치행렬
barplot(t(qty), main="SALES FRUITS", ylim=c(0,900),col=rainbow(length(qty)), 
        space=0.9, cex.axis = 0.8, las=1, names.arg = c("MON","TUE","WED","THU","FRI"), cex=0.8)
legend(1,800, names(qty), cex=0.8, fill=rainbow(length(qty)))

qty
t(qty)

peach=c(180,200,250,199,170)
colors=c()
for(i in 1:length(peach)){
  if(peach[i] >= 200){
    colors=c(colors, 'red')
  } else if(peach[i] >= 180){
    colors=c(colors, 'yellow')
  } else {
    colors=c(colors, 'green')
  }
}
barplot(peach, main='PEACH COLOR', names.arg=c('MON','TUE','WED','THU','FRI'), col=colors)

f1=function(fruits){
  pColor=NULL
  for(i in 1:length(peach)){
    if(peach[i] >= 200){
      pColor[i]='red'
    } else if(peach[i] >= 180){
      pColor[i]='pink'
    } else {
      pColor[i]='blue'
    }
  }
  return(pColor)
}
f1(peach)
barplot(peach, col=f1(peach),main='PEACH COLOR', names.arg=c('MON','TUE','WED','THU','FRI') )

# 히스토그램 : hist() 특정데이터의 빈도수를 막대로 표현
height=c(182,175,167,154,189,154,178)
hist(height, main='HEIGHT')

# pie() : 파이형 그래프. 전체합이 100이 되어야하는 경우 서로를 비교할 때 사용
p1=c(10,22,35,40)
pct=round(p1/sum(p1)*100,1)
lab=paste(pct,' %')
pie(p1, radius=0.5, init.angle = 90, col=rainbow(length(p1)), label=lab) # angle은 시작위치, 반시계방향
legend(0.4,1.1,c('week1','week2','week3','week4'),cex=0.5,fill=rainbow(length(p1)))
lab1=c('week1','week2','week3','week4')
lab2=paste(lab1," \n", pct, " %")
pie(p1, radius=0.7, init.angle = 90, col=rainbow(length(p1)),label=lab2)

install.packages("plotrix")
library(plotrix)

p1=c(10,20,30,40,50)
day=round(p1/sum(p1)*100,1)
label=paste(day,"%", "\n",)
pie3D(p1, main="PIE 3D", radius=0.8, col=rainbow(length(p1)), cex=0.6,labels=label, explode=0.05 )# explode: 파이 분할 간격
legend(0.4,1.2,c('MON','TUE','WED','THU','FRI'), cex=1, fill=rainbow(length(p1)))

# 상자차트(최대, 최소, 중앙값 등을 한눈에 봄) : boxplot()
v1=c(10,12,15,11,20)
v2=c(5,7,15,8,9)
v3=c(11,20,15,18,13)
boxplot(v1,v2,v3, col=c('blue','green','red'), names=c('BLUE','GREEN','RED'), horizontal=T)

# 관계도 그리기: igrap()
# 서로 연관되어 있는 데이터들을 연결해서 표현
install.packages('igraph')
library(igraph)
g1=graph(c(1,2, 2,3, 2,4, 1,4, 5,6, 3,6, 5,5), directed = T)
plot(g1)
print_all(g1)

name=c('대표','부장1','부장2','부장3','과장1','과장2','과장3','대리1','대리2','대리3','사원')
pemp=c('대표','대표','대표','대표','부장1','부장2','부장3','과장1','과장2','과장3','대표')
emp=data.frame(이름=name,상사이름=pemp)
emp
g=graph.data.frame(emp, directed = T)

dev.new()
plot(g, layout=layout.fruchterman.reingold, vertex.size=8, edge.arrow.size=0.5) #vertex.label=NA(이름가리기), layout은 다양한 형태로 가능
g

savePlot('network1.png', type='png') # dev.new를 사용해야만 저장 가능
