# 1.  라라랜드 데이터를 로드한다.  
library(KoNLP)
library(wordcloud)

lala <- read.csv('라라랜드.csv', header=T, stringsAsFactors = F)

# 2. 영화평점이 9점이상은 긍정변수넣고 2점 이하는 부정변수에 넣는다 
lala_positive <- lala[lala$score>=9,c('content')]
lala_negative <- lala[lala$score<=2,c('content')]

head(lala_positive)
head(lala_negative)

# 3. 긍정게시판 변수에서 명사만 추출하고 데이터 정제 작업을 한다.

po <- sapply(lala_positive, extractNoun, USE.NAMES=F)
po2 <- unlist(po)
po2 <- Filter(function(x){nchar(x)>=2},po2)
po3 <- gsub('\\d+','',po2)
po3 <- gsub('관람객','',po3)
po3 <- gsub('평점', '', po3)
po3 <- gsub('영화', '', po3)
po3 <- gsub('진짜', '', po3)
po3 <- gsub('완전', '', po3)
po3 <- gsub('시간', '', po3)
po3 <- gsub('올해', '', po3)
po3 <- gsub('장면', '', po3)
po3 <- gsub('남자', '', po3)
po3 <- gsub('여자', '', po3)
po3 <- gsub('만큼', '', po3)
po3 <- gsub('니가', '', po3)
po3 <- gsub('년대', '', po3)
po3 <- gsub('옆사람', '', po3)
po3 <- gsub('들이', '', po3)
po3 <- gsub('저녁', '', po3)

write(unlist(po3), 'lala_positive.txt')
po4 <- read.table('lala_positive.txt')
po_wordcount <- table(po4)

# 4. 라라랜드 영화에 부정적인 평가 게시글들 명사로 변경하고 정재 작업을 수행한다. 

ne <- sapply(lala_negative, extractNoun, USE.NAMES=F)
ne2 <- unlist(ne)
ne2 <- Filter(function(x){nchar(x)>=2},ne2)
ne3 <- gsub('\\d+','',ne2)
ne3 <- gsub('관람객','',ne3)
ne3 <- gsub('평점', '', ne3)
ne3 <- gsub('영화', '', ne3)
ne3 <- gsub('진짜', '', ne3)
ne3 <- gsub('완전', '', ne3)
ne3 <- gsub('시간', '', ne3)
ne3 <- gsub('올해', '', ne3)
ne3 <- gsub('장면', '', ne3)
ne3 <- gsub('남자', '', ne3)
ne3 <- gsub('여자', '', ne3)
ne3 <- gsub('만큼', '', ne3)
ne3 <- gsub('니가', '', ne3)
ne3 <- gsub('년대', '', ne3)
ne3 <- gsub('옆사람', '', ne3)
ne3 <- gsub('들이', '', ne3)
ne3 <- gsub('저녁', '', ne3)

write(unlist(ne3), 'lala_negative.txt')

ne4 <- read.table('lala_negative.txt')
ne_wordcount <- table(ne4)

# 5. 긍정 단어와 부정단어를 각각 워드 클라우드로 그려서 한 화면에 출력한다. 
graphics.off()
palete <- brewer.pal(9,'Set1')
par(new=T, mfrow=c(1,2))

wordcloud(names(po_wordcount), freq=po_wordcount, scale=c(3,1), rot.per=0.1, random.order = F,
          random.color = T, col=rainbow(15))
title(main='라라랜드의 긍정적인 평가', col.main='blue')

wordcloud(names(ne_wordcount), freq=ne_wordcount, scale=c(3,1), rot.per=0.1, random.order = F,
          random.color = T, col=rainbow(15))
title(main='라라랜드의 부정적인 평가', col.main='red')


#282 :
library(KoNLP)
library(wordcloud)
library(tm)
library(stringr)
library(arules)

lala <- read.csv('라라랜드.csv', header=T, stringsAsFactors = F)
lala_positive <- lala[lala$score>=9,c('content')]
lala_positive <- sapply(lala_positive, extractNoun, USE.NAMES=F)

c <- unlist(lala_positive)
lala_positive2 <- Filter(function(x){ nchar(x) >= 2 & nchar(x) <= 5 } , c)

txt <- readLines("lalagsub.txt")
cnt <- length(txt)
i=1
for(i in 1:cnt){
  lala_positive2 <- gsub((txt[i]),'',lala_positive2)
}
lala_positive2 <- Filter(function(x){ nchar(x) >= 2 & nchar(x) <= 5 } , c)
lala_positive2 <- gsub('\\d+','',lala_positive2)
lala_positive2

res <- str_replace_all(lala_positive2, "[^[:alpha:]]","")
res <- res[res != ""] 

wordcount <- table(res)
wordcount2 <- sort( table(res), decreasing=T)

keyword <- names( wordcount2[wordcount2>100] )
length(res)

contents <- c()
for(i in 1:length(res)) { 
  inter <- intersect(res[[i]] , keyword)
  contents <- rbind(contents ,table(inter)[keyword])
}

colnames(contents) <- keyword

contents[which(is.na(contents))] <- 0 
dim(lala_positive)

detach(package:tm, unload=T)
library(arules) 

rules_lala <- apriori(contents , parameter = list(supp = 0.007 , conf = 0.3 , target = "rules"))
rules_lala 

inspect(sort(rules_lala))

b2 <- t(as.matrix(contents)) %*% as.matrix(contents) 
b2

b2.w <- b2 - diag(diag(b2))
b2.w

library(sna)
library(rgl)

#rownames(b2.w) 
#colnames(b2.w) 
dev.new()
par(new=T, mfrow=c(1,2))
gplot(b2.w , displaylabel=T , vertex.cex=sqrt(diag(b2)) , vertex.col = "green" , 
      edge.col="blue" , boxed.labels=F , arrowhead.cex = .3 , label.pos = 3 , edge.lwd = b2.w*1.5)

#visualization

gplot(b2.w , displaylabel=T , vertex.cex=sqrt(diag(b2)) , vertex.col = "pink" , 
      edge.col="light green" , boxed.labels=F , 
      arrowhead.cex = .3 , label.pos = 3 , edge.lwd = b2.w*1.5) 
