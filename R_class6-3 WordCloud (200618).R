install.packages("KoNLP")
library(plyr)

winter<-readLines('winter.txt')
nouns<-sapply(winter, extractNoun, USE.NAMES = F)
nouns<-unlist(nouns)
nouns<-nouns[nchar(nouns)>=2]
nouns
cnouns<-count(nouns)
cnouns
pal<-brewer.pal(6,'Dark2')
pal<-pal[-(1)]
# 글씨 폰트 설정
windowsFonts(malgun=windowsFont("맑은 고딕"))
dev.new()
wordcloud(words=cnouns$x,        # 단어
          freq=cnouns$freq,       # 단어 빈도수
          colors=pal,            # 색깔
          min.freq=3,            # 빈도수가 3개 이상인 것만 시각화
          random.order=F,        # F로 하게되면 큰 글씨부터 출력하여 중앙에서부터 
          family='malgun')       # 폰트
