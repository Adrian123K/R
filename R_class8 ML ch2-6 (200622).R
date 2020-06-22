income<-function(ename){
  emp<-read.csv('emp3.csv',head=T)
  sal<-emp[emp$ename==ename,'sal']
  print(sal)
}
income('ALLEN')


pytha<-function(){
  num<-scan()
  if(num[1]^2+num[2]^2==num[3]^2){
    print('직각삼각형이 맞습니다.')
  } else {
    print('직각삼각형이 아닙니다.')
  }
}
pytha()

test<-scan()
test[9]
