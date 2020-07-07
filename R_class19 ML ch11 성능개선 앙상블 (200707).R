ret_err <- function(n,err) {
  sum <- 0 
  for(i in floor(n/2):n) { 
    sum <- sum + choose(n,i) * err^i * (1-err)^(n-i)
  }
  sum
}

for(j in 1:60) {
  err <- ret_err(j , 0.4)
  cat(j,'--->',1-err,'\n') 
  if(1-err >= 0.9) break
}