install.packages("tuneR")   
library(tuneR) 
audio <- readWave("output.wav")
play(audio)
head(audio@left, 1000)
plot(head(audio@left, 1000)) 

par(mfrow=c(2,2) )  # 한 화면에 4개의 그래프를 그릴때 사용 
par(mar=c(1,1,1,1) ) # 가장 자리 margin 크기 
audio1 <- readWave("normal.wav")
audio2 <- readWave("ps.wav")
audio3 <- readWave("mr.wav")
audio4 <- readWave("ar.wav")

plot(audio1)
plot(audio2)
plot(audio3)
plot(audio4) 

install.packages("maps")
install.packages("mapproj")
library(maps)
library(mapproj)
par(mfrow=c(2,1))
par(mar=c(0,0,0,0))
map("world")
map("world","south korea")

map("world","north korea",col='red',fill=T)
map("world","south korea",col='blue',fill=T)
