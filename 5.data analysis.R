install.packages("devtools")
library(devtools)

install_github("christophergandrud/d3Network")
install.packages("RCurl")
library(d3Network)
library(RCurl)

name=c('대표','부장1','부장2','부장3','과장1','과장2','과장3','대리1','대리2','대리3','사원')
pemp=c('대표','대표','대표','대표','부장1','부장2','부장3','과장1','과장2','과장3','대표')
emp=data.frame(이름=name,상사이름=pemp)

d3SimpleNetwork(emp, width=1000, height=1000, file="K://r_data//d3.html")

setwd("K://r_data")
g=read.csv("군집분석.csv", head=T,stringsAsFactors = F)
g1=data.frame(학생=g$학생, 교수=g$교수)
head(g1)
g=graph.data.frame(g1, directed=T)
plot(g, layout=layout.kamada.kawai, vertex.size=2, arrow.size=0.01, vertex.color='green', vertex.label=NA)

View(g)$name
gubun1=View(g)$name;gubun1
gubun=str_sub(gubun1, start=1, end=1)
gubun
colors=c()
sizes=c()

for(i in 1:length(gubun)){
  if(gubun[i] == 'S'){
    colors=c(colors,'red')
    sizes=c(sizes,3)
  } else {
    colors=c(colors,'green')
    sizes=c(sizes,6)
  }
}
plot(g, layout=layout.fruchterman.reingold, vertex.size=sizes, edge.arrow.size=0.3, vertex.color=colors, vertex.label=NA)

shapes=c()
for(i in 1:length(gubun)){
  if(gubun[i] == 'S'){
    shapes=c(shapes, 'circle')
  } else {
    shapes=c(shapes, 'square')
  }
}
plot(g, layout=layout.fruchterman.reingold, vertex.size=sizes, edge.arrow.size=0.3, vertex.color=colors, vertex.label=NA, vertex.shape=shapes)

virus=read.csv("메르스전염현황.csv", head=T)
d3SimpleNetwork(virus, width=800, height=800, file="g://r_data//mers.html")

install.packages("treemap")
library(treemap)
tot=read.csv("학생시험결과_전체점수.csv", head=T, sep=",")
tot
View(tot)
treemap(tot, vSize='점수', index=c('조','점수',"이름"))

tot=read.table("학생별전체성적_new.txt", head=T, sep=",")
tot
row.names(tot)=tot$이름 # 행번호를 이름으로 설정
tot
dev.new()
stars(tot, flip.labels=FALSE, draw.segments=TRUE, frame.plot=TRUE, full=TRUE, main="학생별 성적분석", 
      key.loc=c(1,2.5), key.xpd=NA, scale=T, radius=F, len=0.8)

# lab=names(tot)
# value=table(lab)
# dev.new()
# pie(value, labels=lab, radius=0.1, cex=0.6, col=NA)
# savePlot("star_2.png", type="png")

# radarchart
# 1. Sample Data 만들기 
# 2. 최댓값, 최솟값 지정
# 3. radarchart 함수 이용

install.packages("fmsb")
library(fmsb)
layout=data.frame(
  분석력=c(5,1),
  창의력=c(15,3),
  판단력=c(3,0),
  이해력=c(5,1),
  사고력=c(5,1)
)
set.seed(123)
data=data.frame(
  분석력=runif(3,1,5),
  창의력=rnorm(3,10,2),
  판단력=c(0.5,NA,3),
  이해력=runif(3,1,5),
  사고력=c(5,2.5,4)
)
data1=rbind(layout,data)
op=par(mar=c(1,0.5,3,1),mfrow=c(2,2))
radarchart(data1, axistype=1, seg=5, plty=1, title="1st")
radarchart(data1, axistype=2, pcol=topo.colors(3), plty=1, title="2nd")
radarchart(data1, axistype=3, pty=32, plty=1, axislabcol='grey', na.itp=FALSE, title="3rd")
radarchart(data1, axistype=0, plwd=1:5, pcol=1, title="4th")

# 저수준 작도함수
# points()
# lines(), segment(), abline()
# grid()
# arrows()
# rect() 사각형 박스
# text(), mtext, title() (x,y,"내용")
# legend()
# box(), axis()
# polygon() 다각형

mfrow=c(1,1)

plot(1:15)
abline(h=8)
rect(1,6,3,8)
arrows(1,1,5,5)
text(8,9,"TEXT")
title("TEST", "SUB")

kor=read.table("학생별국어성적_new.txt", head=T, sep=",")
kor
ggplot(kor, aes(x=이름, y=점수))+geom_point()
# ggplot(dataframe, aes(x=x축,y=y축))+geom_함수()
# geom 설정값
# stat: 주어진 데이터에서 geom에 필요한 데이터를 생성
# stat_bin
# 1.count: 빈도수
# 2.density: 밀도수
# 3.ncount: count와 같으나 값의 범위가 (0,1)로 스케일링
# 4.ndensity: density와 같으나 값의 범위가 (0,1)로 스케일링
# 위 설정 값을 지정하지 않으면 기본 값은 count
# geom_bar()
gg=ggplot(kor, aes(x=이름,y=점수))+geom_bar(stat='identity', fill='blue', color='red') # color: 외곽선, fill: 내부색상
gg+theme(axis.text.x=element_text(angle=45, hjust=0, vjust=0, color='blue', size=8)) #hjust: horizental justification

kem=read.csv("학생별과목별성적_국영수_new.csv", head=T)
kem
library(plyr)
skem=arrange(kem, 이름, 과목)
skem
skem2=ddply(skem, "이름", transform, 누적합계=cumsum(점수))
skem2
skem3=ddply(skem2, "이름", transform, 누적합계=cumsum(점수), label=cumsum(점수)-0.5*점수)
skem3

gg2=ggplot(skem3, aes(x=이름, y=점수, fill=과목))+geom_bar(stat='identity', position=position_stack(reverse=T))+geom_text(aes(y=label, label=paste(점수, '점')), color='black', size=4)+guides(fill=guide_legend(reverse=T))
gg2+theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))

# geom_segment(): 클리블랜드 점 그래프로 알려진 데이터를 출력
sc=read.table("학생별전체성적_new.txt", head=T, sep=",")
sc
sc[,c("이름","영어")]
gg=ggplot(sc, aes(x=영어,y=reorder(이름,영어)))
gg+geom_point(size=6)+theme_bw()+theme(panel.grid.major.x=element_blank(), 
                                       panel.grid.major.y=element_line(color='red', linetype='dashed')) # x축의 점수선, y축에서 그리는 선
ggplot(sc, aes(x=영어,y=reorder(이름,영어)))+geom_segment(aes(yend=이름), xend=0, color='blue')+geom_point(size=6, color='red')+theme_bw()+theme(panel.grid.major.x=element_blank())

install.packages("gridExtra")
library(gridExtra)
mt=mtcars
head(mt)
g1=ggplot(mt, aes(x=hp, y=mpg))
g1+geom_point(aes(color=factor(am), shape=factor(am),size=wt))+scale_color_manual(values=c('red','green'))+geom_line()+labs(x='마력',y='연비(mile/gal)')
# factor값에 따라 다르게 표현, size도 동일. 색상 지정하여 사용 가능
str(mt)

th=read.csv("학생별과목별성적_3기_3명.csv", head=T)
th
ss=arrange(th, 이름, 과목)
ss
ggplot(ss, aes(x=과목, y=점수, color=이름, group=이름))+geom_line()+geom_point(size=6, shape=23)

dis=read.csv("1군전염병발병현황_년도별.csv", stringsAsFactors=F)
str(dis)
ggplot(dis, aes(x=년도별, y=장티푸스, group=1))+geom_area(color='red',fill='blue', alpha=0.5)+geom_line()+theme_bw()


