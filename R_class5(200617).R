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

################################################################# 148
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
                  choices= colnames(iris)[-5]),
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
    par(new=T)
    class1<-sort(iris[,input$region[-5]])
    plot(class1, dnorm(class1,mean=mean(class1),sd=sd(class1)),type='l',axes=F, ann=F, col='red')
  })
}
# Run the app ----
shinyApp(ui = ui, server = server)

################################################################# 149
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
                  choices= colnames(iris)[-5]),
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
      par(mfrow=c(1,2))
      rs<-sort(iris[,input$region])
      hist(iris[,input$region], col="green", density=80)
      par(new=T)
      plot(rs,dnorm(rs,mean=mean(rs),sd=sd(rs)),type='l',axes=F, ann=F, col='red')
      boxplot(iris[,input$region], col='Sky Blue')
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)

################################################################# 150
# Define UI ----
ui <- fluidPage(    
  # Give the page a title
  titlePanel("iris 데이터의 히스토그램 그래프"),
  # Generate a row with a sidebar
  sidebarLayout(      
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "컬럼:", 
                  choices= colnames(iris)[-5]),
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
    par(margin=c(0.1,0.1,1,0.1))
    par(mfrow=c(2,2))
    rs<-sort(iris[,input$region])
    hist(iris[,input$region], col="green", density=80)
    par(new=T)
    plot(rs,dnorm(rs,mean=mean(rs),sd=sd(rs)),type='l',axes=F, ann=F, col='red')
    boxplot(iris[,input$region], col='Sky Blue')
    #rs<-aggregate(input$region~iris$Species,iris,mean)
    #pie(rs[,input$region],labels=rs$`iris$Species`)
    barplot(iris[,input$region],col=rainbow(150))
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)

###
rs<-aggregate(iris$Sepal.Length~iris$Species,iris,mean)
pie(rs$`iris$Sepal.Length`,labels=rs$`iris$Species`)


############## set this file location to working directory ##########################
packages <- 'rstudioapi'
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
library('rstudioapi')
current_dir<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)

package_in<-function(p_name,option=1){
  packages <- p_name
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))
  }
  if (option==1){
    library(p_name,character.only = TRUE)
  }
}

###########################1. 패키지 설치##########################################

package_in('shinydashboard')
package_in('shiny')
package_in('ggplot2')
package_in('plotly')
package_in('lattice')

######################### 2. 화면 개발 ###########################################

sidebar <- dashboardSidebar(
  sidebarMenu(
    fileInput("file1", "Choose CSV File",
              multiple = FALSE,
              accept = c("text/csv",".xlsx",".txt",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    
    menuItem("Plot",
             menuSubItem('Barplot',tabName='barplot'),
             menuSubItem('Piechart',tabName='piechart'),
             menuSubItem('Lineplot',tabName='lineplot'),
             menuSubItem('Scatterplot',tabName='scatterplot')
    )
  )
)

body <- dashboardBody(
  tabItems(
    ##### bar plot
    tabItem(tabName = "barplot",
            sidebarPanel(
              selectInput("in_sel_bar_yVar","y Variable:", choices = NULL),
              selectInput("in_sel_bar_xVar","x Variable:", choices = NULL)
            ),
            mainPanel(
              plotOutput('plot_bar')
            )
    ),
    ##### piechart
    tabItem(tabName = "piechart",
            sidebarPanel(
              selectInput("in_sel_pie_xVar","x Variable:", choices = NULL)
            ),
            mainPanel(
              plotlyOutput('plot_pie')
            )
    ),
    ##### line plot
    tabItem(tabName = "lineplot",
            sidebarPanel(
              selectInput("in_sel_line_yVar","y Variable:", choices = NULL),
              selectInput("in_sel_line_xVar","x Variable:", choices = NULL)
            ),
            mainPanel(
              plotlyOutput('plot_line')
            )
    ),
    ##### scatter plot
    tabItem(tabName = "scatterplot",
            sidebarPanel(
              selectInput("in_sel_scatter_yVar","y Variable:", choices = NULL),
              selectInput("in_sel_scatter_xVar","x Variable:", choices = NULL)
            ),
            mainPanel(
              plotOutput('plot_scatter'),
              textOutput('text_scatter')
            )
    )
  )
)

ui<-dashboardPage(
  dashboardHeader(title='my graph'),
  sidebar,
  body
)

######################3. 서버단 개발 ########################################
server <- function(input, output,session) {
  options(warn = -1)
  options(shiny.maxRequestSize = 30*1024^2)
  dataload<-reactive({
    req(input$file1)
    file1 = input$file1
    data1 = read.csv(file1$datapath)
    
    updateSelectInput(session, "in_sel_bar_xVar", choices = colnames(data1))
    updateSelectInput(session, "in_sel_bar_yVar", choices = colnames(data1))
    
    updateSelectInput(session, "in_sel_pie_xVar", choices = data1[,1])
    
    updateSelectInput(session, "in_sel_line_xVar", choices = colnames(data1))
    updateSelectInput(session, "in_sel_line_yVar", choices = colnames(data1))
    
    updateSelectInput(session, "in_sel_scatter_xVar", choices = colnames(data1))
    updateSelectInput(session, "in_sel_scatter_yVar", choices = colnames(data1))
    
    return(data1)
  })
  ####nomal_bar
  output$plot_bar <- renderPlot({
    table_in<-dataload()
    
    xdata<-as.factor(table_in[,input$in_sel_bar_xVar])
    ydata<-as.factor(table_in[,input$in_sel_bar_yVar])
    fdata=data.frame(x=xdata,y=ydata)
    
    ggplot(fdata) + 
      geom_bar(aes_string(x='x',y='y',fill='x'),stat = "identity",show.legend=F)
  })
  
  output$plot_pie <- renderPlotly({
    table_in<-dataload()
    plot_ly(table_in, labels = ~colnames(table_in)[-1], values=~as.factor( table_in[table_in[,1] == input$in_sel_pie_xVar,-1] ),type='pie')
  })
  
  output$plot_line <- renderPlotly({
    table_in<-dataload()
    x <- list(title = input$in_sel_line_xVar)
    y <- list(title = input$in_sel_line_yVar)
    plot_ly(data = table_in,x=~table_in[,input$in_sel_line_xVar],y=~table_in[,input$in_sel_line_yVar],type='scatter',mode='dot')%>%
      layout(xaxis = x, yaxis = y)
  })
  
  output$plot_scatter <- renderPlot({
    table_in<-dataload()
    xyplot(table_in[,input$in_sel_scatter_yVar]~table_in[,input$in_sel_scatter_xVar], grid=T,type=c('p','smooth'),col.line='darkorange',lwd=2, xlab=input$in_sel_scatter_xVar,ylab=input$in_sel_scatter_yVar)
  })
  
  output$text_scatter <- renderText({
    table_in<-dataload()
    paste("The correlation between the two is: ", cor(table_in[,input$in_sel_scatter_yVar],table_in[,input$in_sel_scatter_xVar]))
  })
}

######################### 4. 샤이니 실행 ###############################

shinyApp(ui = ui, server = server)
