mh <- read.csv('보건복지부 국립정신건강센터_외래환자기초정보_20200331.csv',head=T, stringsAsFactors=T)
View(mh)
str(mh)

unique(mh$진료연도)

mh <- mh[,c(1,2,4)]

summary(mh)
library(treemap)

colnames(mh)=c('age','sex','diag')
dev.new()
treemap(mh,vSize='age',index = c('age','sex','diag'),)

mh_19 <- read.csv('국립정신건강센터_외래환자기초정보_2019..csv',head=T,stringsAsFactors=T)
View(mh_19)
summary(mh_19)

unique(mh_19$상병명)

write.csv(mh,'principal mentalhealth.csv')


pie(mh$diag)
