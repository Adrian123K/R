stats<-function(){
  tb <- file.choose()
  table <- read.csv(tb, head=T)
  print(colnames(table))
  col <- readline(prompt = 'which column?')
  data <- table[,col]
  rs <- c()
  for (i in 1:length(data)){
    rs[i]<-data[i]
  }
  cat('max', max(rs), '\n', 
      'min', min(rs), '\n',
      'mean', mean(rs),'\n',
      'median', median(rs),'\n',
      'var', var(rs) , '\n',
      'std',sd(rs) )
  
  boxplot(rs,main=list(paste(col,'Summary')))
}
stats()