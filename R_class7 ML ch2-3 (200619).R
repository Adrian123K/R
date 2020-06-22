matrix( c(1:9), nrow=3, ncol=3 ) 
matrix( c(1:9), nrow=3, ncol=3, byrow=T) 

a<-matrix(c(1:9),nrow=3, ncol=3, byrow=T)
b<-matrix(c(1:9),nrow=3, ncol=3)
a%*%b

array( c(1:12) , dim=c(3,4) )   # 2차원 
array( c(1:12) , dim=c(2,2,3) )  # 3차원 
array( c(1:12) , dim=c(2,2,2,2) ) # 4차원 

install.packages("DBI")
install.packages("RJDBC")
library(DBI)
library(RJDBC)
driver <- JDBC('oracle.jdbc.driver.OracleDriver', 'ojdbc8-12.2.0.1.jar')
oracle_db <- dbConnect(driver, 'jdbc:oracle:thin:@//127.0.0.1:1521/orcl', 'scott', 'tiger')
emp_query <- 'select * from emp'

emp_data <- dbGetQuery(oracle_db, emp_query)
emp_data

install.packages("outliers")
library(outliers)

class1<-c( rep(19,3), rep(20,6), rep(21,3), 145, 147 )
class1
table(class1)

x<-boxplot(class1)
x$out
