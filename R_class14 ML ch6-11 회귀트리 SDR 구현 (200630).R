attr <- data.frame(pattern=c('수직','수직','대각선','수평','수평','수평','수직','수직','대각선','수평','수직','대각선','대각선','수평'),
                   outline=c('점선','점선','점선','점선','실선','실선','실선','점선','실선','실선','실선','점선','실선','점선'),
                   dot=c('무','유','무','무','무','유','무','무','유','무','유','유','무','유'))

area <- data.frame(area=c(25,30,46,45,52,23,43,35,38,46,48,52,44,30))
data <- cbind(attr,area)
data
ft <- data.frame(수직area=sd(data[data$pattern=='수직','area']),
                 대각선area=sd(data[data$pattern=='대각선','area']),
                 수평area=sd(data[data$pattern=='수직','area']))
ft
                   
                   

origin <- c(1,1,1,2,2,3,4,5,5,6,6,7,7,7,7)
at1 <- c(1,1,1,2,2,3,4,5,5)
at2 <- c(6,6,7,7,7,7)
bt1 <- c(1,1,1,2,2,3,4)
bt2 <- c(5,5,6,6,7,7,7,7)

sdr_a <- sd(origin)-(length(at1)/length(origin)*sd(at1)+length(at2)/length(origin)*sd(at2))
sdr_a

sdr_b <- sd(origin)-(length(bt1)/length(origin)*sd(bt1)+length(bt2)/length(origin)*sd(bt2))
sdr_b

mean(bt1)
mean(bt2)

