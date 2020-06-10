install.packages("dplyr")
install.packages("googleVis")
install.packages("devtools")
install.packages("lubridate")
install.packages("stringr")
install.packages("ggplot2")
install.packages("rJava")
install.packages("RColorBrewer")
install.packages("wordcloud")
library(dplyr)
library(googleVis)
library(devtools)
library(lubridate)
library(stringr)
library(ggplot2)
library(rJava) # 안되는 라이브는 삭제 후 다시 
library(RColorBrewer)
library(wordcloud)

# Rtools 설치 후 

install.packages("multilinguer")
library(multilinguer)
install_jdk()
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
Sys.setenv(JAVA_HOME="c:/Program files/Java/jre.1.8.0_241/")

install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"), force=TRUE)

library(KoNLP)
useSejongDic() # 3:none

install.packages("glue")

# WordCloud
# 1. 데이터에서 단어만 추출
# 2. 단어 집합 생성
# 3. 단어 필터링
# 4. 단어 핸들링
# 5. txt파일로 저장하고 table로 불러오면서 공백제거
# 6. 단어 빈도수 저장
# 7. wordcloud 출력

d1=readLines("BTS유엔연설_국문.txt")
d1 
# 1. 데이터에서 단어만 추출
d2=sapply(d1, extractNoun, USE.NAMES=F)
d2

# 2. 단어 집합 생성
head(unlist(d2),30)
d3=unlist(d2)
d3

# 3. 단어 필터링
# 4. 단어 핸들링
# gsub("변경 전 글자", "변경 후 글자", "원본데이터")
d3=gsub("\\d+","",d3)
d3=gsub("들","",d3)
d3=gsub("저","나",d3)
d3=gsub("내","나",d3)
d3=gsub("나","자신",d3)
d3
d3=gsub("해","",d3)
d3=gsub("곳","",d3)
d3=gsub("한","",d3)
d3=gsub("것","",d3)
d3=gsub("돌","",d3)
d3=gsub("이","",d3)
d3=gsub("름","",d3)

# 5. txt파일로 저장하고 table로 불러오면서 공백제거
write(unlist(d3),"BTS_kor.txt")
d4=read.table("BTS_kor.txt")
d4

# 6. 단어 빈도수 저장
nrow(d4)
wc=table(d4)
wc
head(sort(wc,decreasing=T),20)

# 7. wordcloud 출력
pal=brewer.pal(9,"Set3")
wordcloud(names(wc), freq=wc, scale=c(5,1), rot.per=0.25, min.freq=1, random.order=F, random.color=T, colors=pal)
legend(0.3,1,"BTS UN 연설문", cex=0.8, fill=NA, border=NA, bg='white', text.col='red', text.font=2, box.col='red')

library(KoNLP)
library(wordCloud)
useSejongDic()
useNIADic()

d1=readLines("remake.txt")
d1
d2=sapply(d1, extractNoun, USE.NAMES=F)
d2
d3=unlist(d2)
d3
d3=Filter(function(x){
  nchar(x)<=10
}, d3)
d3
head(unlist(d3),30)
d3=gsub("\\d+","",d3)
d3=gsub("수","",d3)
d3=gsub("술","",d3)
d3=gsub("한","",d3)
d3=gsub("것","",d3)
d3=gsub("쌍수","쌍꺼풀",d3)
d3=gsub("쌍코풀","쌍꺼풀",d3)
d3=gsub("메부리코","매부리코",d3)
d3=gsub("\\.","",d3)
d3=gsub(" ","",d3)
d3=gsub("\\'","",d3)
d3
write(unlist(d3),"remake_2.txt")
d4=read.table("remake_2.txt")
d4
nrow(d4)
txt=readLines("성형gsub.txt")
txt
cnt=length(txt)
i=1
for(i in 1:cnt){
  d3=gsub((txt[i]),"",d3)
}
d3
d3=Filter(function(x){
  nchar(x)>=2
},d3)
write(unlist(d3),"remake_2.txt")
d5=read.table("remake_2.txt")
d5
wc=table(d5)
wc
head(sort(wc,decreasing=T),30)

pal=brewer.pal(9,"Set3")
wordcloud(names(wc), freq=wc, scale=c(5,1), rot.per=0.25, min.freq=3, random.order=F, random.color=T, colors=pal)
legend(0.4,1,"Remake Counseling", cex=0.8, fill=NA, border=NA, bg='white', text.col='blue', text.font=2, box.col='red')

# 네이버->"성형수술 부작용" 검색한 후 원본파일 생성
# 주어진 remake2.txt 파일 분석하여 워드클라우스 생성
# 불필요한 단어는 성형부작용gsub.txt를 사용하여 제거

d1=readLines("remake2.txt")
d1
d2=sapply(d1,extractNoun,USE.NAMES=F)
d2
d3=unlist(d2)
d3
d3=Filter(function(x){
  nchar(x)<=5
},d3)
d3
d3=gsub("\\d+","",d3)
d3=gsub("\\.","",d3)
txt=readLines("성형gsub.txt")
cnt=length(txt)
i=1
for(i in 1:cnt){
  d3=gsub((txt[i]),"",d3)
}
d3=Filter(function(x){
  nchar(x)>=2
},d3)
d3
write(unlist(d3),"remake2_2.txt")
d4=read.table("remake2_2.txt")
nrow(d4)
wc=table(d4)
wc
head(sort(wc, decreasing=T),30)
pal=brewer.pal(9,"Set3")
wordcloud(names(wc), freq=wc, scale=c(5,1), rot.per=0.25, min.freq=2, random.order=F, random.color=T, colors=pal)
legend(0.3,1,"Remake SideEffect", cex=0.8, fill=NA, border=NA, bg='white', text.col='red',text.font=2,box.col='red')


library(KoNLP)
library(wordCloud)
library(stringr)
library(RColorBrewer)
useSejongDic()
useNIADic()

mergeUserDic(data.frame(readLines("제주도여행지.txt"),"ncn")) # 사전에 데이터 입력하여 분할되는것을 방지

txt=readLines('jeju.txt')
place=sapply(txt, extractNoun, USE.NAMES=F)
place
head(unlist(place),30)
cdata=unlist(place)
place=str_replace_all(cdata,"[^[:alpha:]]","") # 첫글자가 문자인 경우 제외하고 문자는 전부 바꾸기
place=gsub(" ","",place)
txt=readLines("제주도여행코스gsub.txt")
cnt=length(txt)
i=1
for(i in 1:cnt){
  place=gsub((txt[i]),"",place)
}
place
place=Filter(function(x){
  nchar(x)<=5
},place)
place=Filter(function(x){
  nchar(x)>=2
},place)
write(unlist(place),"jeju_2.txt")
place2=read.table("jeju_2.txt")
place2
wc=table(place2)
wc
head(sort(wc,decreasing=T),30)
pal=brewer.pal(9,"Set1")
wordcloud(names(wc),freq=wc,scale=c(5,0.5), rot.per=0.5, min.freq=1, random.order=F, random.color=T, colors=pal)

install.packages("tm")
library(tm)

d1=readLines("BTS유엔연설_영문.txt")
d1
class(d1)
# corpus(말뭉치) 형태로 변환
c1=VCorpus(VectorSource(d1))
c1
inspect(c1)
tdm=TermDocumentMatrix(c1) # tm이 분석할 수 있도록 변환
tdm
m=as.matrix(tdm)
m

c2=tm_map(c1, stripWhitespace) # 여러 개의 공백을 하나로
c2=tm_map(c2, tolower) # 대문자->소문자
c2=tm_map(c2, removeNumbers) # 숫자제거
c2=tm_map(c2, removePunctuation) # 마침표, 콤마, 콜론, 세미콜론 등 제거
c2=tm_map(c2, PlainTextDocument) # 일반문서로 변환
sw=c(stopwords('en'),'and','but','or','not') #불용어(사용하지 않는 단어) 제거 및 단어 추가
c2=tm_map(c2, removeWords, sw) # c2에서 불용어 제거

tdm2=TermDocumentMatrix(c2)
tdm2
m2=as.matrix(tdm2)
m2
class(m2)

colnames(m2)=c(1:31)
m2

f1=sort(rowSums(m2), decreasing=T)
head(f1)

f2=sort(colSums(m2), decreasing=T)
head(f2,20)

findFreqTerms(tdm2, 2) # 특정횟수 이상 언급된 단어 검색
pal=brewer.pal(7,"Set3")
wordcloud(names(f1),freq=f1,scale=c(5,1),min.freq=1, random.order=F,random.color=T,colors=pal)
barplot(f1, las=2, ylim=c(0,5))

# 스티브 잡스 연설문 분석. tm() package이용
d1=readLines("steve.txt")
c1=VCorpus(VectorSource(d1))
c2=tm_map(c1, stripWhitespace)
c2=tm_map(c2, tolower)
c2=tm_map(c2, removeNumbers)
c2=tm_map(c2, removePunctuation)
c2=tm_map(c2, PlainTextDocument)
sw=c(stopwords('en'),'and','but','or','not')
c2=tm_map(c2, removeWords, sw)
c3=TermDocumentMatrix(c2, control=list(wordLengths=c(1,Inf)))
c3
findFreqTerms(c3,10)
findAssocs(c3,'apple',0.5)
m1=as.matrix(c3)
m1
f1=sort(rowSums(m1), decreasing=T)
f2=sort(colSums(m1), decreasing=T)
head(f1,20)
head(f2,20)
dim(m1)
colnames(m1)=c(1:59)
f2=sort(colSums(m1), decreasing=T)
f2

pal=brewer.pal(7,"Set2")
wordcloud(names(f1), freq=f1, scale=c(5,1), min.freq=5, random.order=F, random.color=T, colors=pal)
