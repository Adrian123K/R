emp <- read.csv("emp3.csv", header=T)
attach(emp)
emp$deptsal <- ave(sal, deptno, FUN=function(x) mean(x))
emp$jobsal <- ave(sal, job, FUN=function(x) mean(x))

emp[ , c("ename","sal","deptno","avgsal")]
emp
attach(emp)
emp

emp$comm[is.na(emp$comm)] <- ave(emp$sal, emp$deptno, FUN=function(x) mean(x))
emp

ave(1:3)

require(graphics)

warpbreaks
warpbreaks$breaks
warpbreaks$wool
warpbreaks$tension

ave(1:3)  # no grouping -> grand mean

# 이렇게 해보면 확실히 알 수 있다.

attach(warpbreaks)
warpbreaks$a<-ave(breaks, wool)
warpbreaks

ave(1:3)  # no grouping -> grand mean

attach(warpbreaks)
ave(breaks, wool)
ave(breaks, tension)
ave(breaks, tension, FUN = function(x) mean(x, trim = 0.1))

dev.new()
plot(breaks, main ="ave( Warpbreaks )  for   wool  x  tension  combinations")
lines(ave(breaks, wool, tension              ), type = "s", col = "blue")
lines(ave(breaks, wool, tension, FUN = median), type = "s", col = "green")
legend(40, 70, c("mean", "median"), lty = 1,col = c("blue","green"), bg = "gray90")

detach()

