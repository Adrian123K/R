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

sd(x1)
sd(x2)
sd(x3)

##################################################################### usedcars.csv
car=read.csv('usedcars.csv',head=T,stringsAsFactors=F)
str(car)
quantile(car$price)

boxplot(car$price, horizontal = T)

IQR(car$price)

nrow(car)
car$mileage
sort(car$mileage) # 정렬

class1<-sort(car$mileage)
hist(sort(car$mileage),col='green')
par(new=T)
plot(class1, dnorm(class1,mean=mean(class1),sd=sd(class1)),type='l',axes=F, ann=F, col='red')
     

rs<-hist(sort(car$price),col='light green')
hist(sort(car$price),col='green',main='중고차 가격 히스토그램 그래프')
par(new=T)
class1<-sort(car$price)
plot(class1, dnorm(class1,mean=mean(class1),sd=sd(class1)),type='l',axes=F, ann=F, col='red')

####################################################################### iris data
summary(iris)

iris$Petal.Width

hist(iris$Petal.Width, main='Iris 꽃잎의 넓이 히스토그램',col='magenta')
par(new=T)
class1<-sort(iris$Petal.Width)
plot(class1, dnorm(class1,mean=mean(class1),sd=sd(class1)),type='l',axes=F, ann=F, col='blue')


####################################################################### 147
library(shiny)
library(datasets)
# Define UI ----
ui <- fluidPage(    
  # Give the page a title
  titlePanel("iris 데이터의 히스토그램 그래프"),
  # Generate a row with a sidebar
  sidebarLayout(      
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "컬럼:", 
                  choices= colnames(iris)),
      hr(),
      helpText("iris 컬럼의 히스토그램 그래프 ")
    ),
    # Create a spot for the barplot
    mainPanel(
      plotOutput("typePlot")  
    )
  )
)
# Define server logic ----
server <-function(input, output) {
  # Fill in the spot we created for a plot
  output$typePlot <- renderPlot({
    hist( iris[ ,input$region ] , col="green", density=80 ) 
  })
}
# Run the app ----
shinyApp(ui = ui, server = server)
