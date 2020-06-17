getwd()

######################################## 132
x1=c(7,8,9,9,10,10,11,11,12,13)
x2=c(7,9,9,10,10,10,10,11,11,13)
x3=c(1,1,7,7,10,10,10,10,11,13,30)
mean(x1)
mean(x2)
mean(x3)

median(x1)
median(x2)
median(x3)

names(table(x1))[table(x1) == max(table(x1))]

range(x1)
range(x3)

rs=boxplot(x3)
rs
rs$stats
