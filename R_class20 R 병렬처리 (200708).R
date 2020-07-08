install.packages('parallel')
library(parallel)

a <- rnorm(1000000)
a
system.time(a <- rnorm(1000000))

system.time(l2<-unlist(mclapply(1:2, function(x) {
  rnorm(5000000) } , mc.cores=2 )))

# 리눅스에서 수행  
#* foreach 와 doParallel 를 이용한 병렬 작업 (p543)
#1. 병렬처리 안했을때 
library(foreach)
system.time(l4<- foreach(i =1:400, .combine='c')
            %do% rnorm(250000))


# 2. 병렬처리 했을때 
library(doParallel)
registerDoParallel(cores=4) # 병렬처리하는 코어 수 작성
system.time(l4<- foreach(i =1:400, .combine='c')
            %do% rnorm(250000))

