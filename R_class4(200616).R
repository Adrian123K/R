getwd()

###########################################R shiny 실습###############################################
install.packages("ggplot2")
install.packages("DT")
install.packages("shiny")

library(ggplot2)
library(DT)
library(shiny)

ui<-fluidPage()
server<-function(input, output){
  
}
shinyApp(ui=ui, server=server)

ui<-fluidPage(
  titlePanel('title panel'),
  sidebarLayout(
    sidebarPanel('sidebar panel'),
    mainPanel('main panel')
  )
  
)
server<-function(input, output){
  
}
shinyApp(ui=ui, server=server)

#101
ui<-fluidPage(
  titlePanel('Data Visualize Software'),
  sidebarLayout(
    sidebarPanel('sidebar panel'),
    mainPanel('main panel')
  )
)
server<-function(input, output){
  
}
shinyApp(ui=ui, server=server)

#
library(shiny)

# Define UI ----
ui <- fluidPage(
  titlePanel("Basic widgets"),
  fluidRow(
    column(3,
           h3("Buttons"),
           actionButton("action", "Action"),
           br(),
           br(), 
           submitButton("Submit")),
    column(3,
           h3("Single checkbox"),
           checkboxInput("checkbox", "Choice A", value = TRUE)),
    column(3, 
           checkboxGroupInput("checkGroup", 
                              h3("Checkbox group"), 
                              choices = list("Choice 1" = 1, 
                                             "Choice 2" = 2, 
                                             "Choice 3" = 3),
                              selected = 1)),
    column(3, 
           dateInput("date", 
                     h3("Date input"), 
                     value = "2014-01-01"))   
  ),
  fluidRow(
    column(3,
           dateRangeInput("dates", h3("Date range"))),
    column(3,
           fileInput("file", h3("File input"))),
    column(3, 
           h3("Help text"),
           helpText("Note: help text isn't a true widget,", 
                    "but it provides an easy way to add text to",
                    "accompany other widgets.")),
    column(3, 
           numericInput("num", 
                        h3("Numeric input"), 
                        value = 1))   
  ),
  fluidRow(
    column(3,
           radioButtons("radio", h3("Radio buttons"),
                        choices = list("Choice 1" = 1, "Choice 2" = 2,
                                       "Choice 3" = 3),selected = 1)),
    column(3,
           selectInput("select", h3("Select box"), 
                       choices = list("Choice 1" = 1, "Choice 2" = 2,
                                      "Choice 3" = 3), selected = 1)),
    column(3, 
           sliderInput("slider1", h3("Sliders"),
                       min = 0, max = 100, value = 50),
           sliderInput("slider2", "",
                       min = 0, max = 100, value = c(25, 75))
    ),
    column(3, 
           textInput("text", h3("Text input"), 
                     value = "Enter text..."))   
  )
)

# Define server logic ----
server <- function(input, output) {

}

# Run the app ----
shinyApp(ui = ui, server = server)

#103
emp
str(emp)
barplot(emp$sal)

#104
barplot(emp$sal, main='Salary Bar Chart')

#108
ccnt=read.csv('창업건수.csv',header=T)
ccnt

#############################################################114

library(shiny)
create_cnt <- read.csv("창업건수.csv", header=T)

# Define UI ----

ui <- fluidPage(    
  # Give the page a title
  titlePanel("년도별 업종별 창업현황"),
  # Generate a row with a sidebar
  sidebarLayout(      
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "업종:", 
                  choices=colnames(create_cnt)[-1]),
      helpText("업종별 창업과 폐업 현황 보고서")
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
    # Render a barplot
    barplot(create_cnt[,input$region], 
            main=input$region,
            col = rainbow(10),
            density=50,
            names.arg= create_cnt$년도,    
            ylab="건수",
            xlab="년도")
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)


##########################################################115
create_cnt <- read.csv("창업건수.csv", header=T)
drop_cnt <- read.csv('폐업건수.csv',header=T)
x=rbind(create_cnt,drop_cnt)
# Define UI ----
ui <- fluidPage(    
  # Give the page a title
  titlePanel("년도별 업종별 폐업현황"),
  # Generate a row with a sidebar
  sidebarLayout(      
    # Define the sidebar with one input
    sidebarPanel(############################################## 데이터 입력에 대한 화면 구현
      selectInput("region", "업종:",########################### selectInput에서 설정한 변수를 server에서 데이터를 받아온다.
                  choices=colnames(create_cnt)[-1]),
      helpText("업종별 창업과 폐업 현황 보고서")
    ),
    # Create a spot for the barplot
    mainPanel(
      plotOutput("typePlot")  ################################# output$typePlot data를 받아서 main panel에서 출력(plotOutput)
    )
  )
)

# Define server logic ----
server <-function(input, output) {
  # Fill in the spot we created for a plot
  output$typePlot <- renderPlot({
    # Render a barplot
    barplot(drop_cnt[,input$region], ####################### input$변수 -> column 데이터 
            main=input$region,
            col = rainbow(10),
            density=50,
            names.arg= drop_cnt$년도,    
            ylab="건수",
            xlab="년도")
  })
}
# Run the app ----
shinyApp(ui = ui, server = server)

#############################################116
library(shiny)
library(datasets)
setwd("d:/R") 
create_cnt <- read.csv("창업건수.csv", header=T)
drop_cnt <- read.csv("폐업건수.csv", header=T) 

# Define UI ----
ui <- fluidPage(    
  # Give the page a title
  titlePanel("연도별 업종별 창/폐업현황"),
  # Generate a row with a sidebar
  sidebarLayout(      
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "업종:", 
                  choices=colnames(create_cnt)[-1]),
      hr(),
      helpText("업종별 창업과 폐업 현황 보고서")
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
    # Render a barplot
    barplot( rbind( create_cnt[,input$region], 
                    drop_cnt[,input$region] ),
             main=input$region,
             col = c("Sky Blue","Hot pink"),
             beside=T, 
             density=80,
             names.arg= create_cnt$년도,    
             ylab="건수",
             xlab="연도")
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)

ui <- fluidPage(    
  # Give the page a title
  titlePanel("연도별 업종별 창/폐업현황"),
  # Generate a row with a sidebar
  sidebarLayout(      
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "업종:", 
                  choices=colnames(create_cnt)[-1]),
      hr(),
      helpText("업종별 창업과 폐업 현황 보고서")
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
    # Render a barplot
    barplot( rbind( create_cnt[,input$region], 
                    drop_cnt[,input$region] ),
             main=input$region,
             col = c("Sky Blue","Hot pink"),
             beside=T, 
             density=80,
             names.arg= create_cnt$년도,    
             ylab="건수",
             xlab="연도",
             args.legend=list(x='topright', bty='n',inset=c(0,-0.2)),
             legend=c('창업','폐업'))
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)

##############################################원형그래프 그리기##################################################

########################################121
create_cnt=read.csv('창업건수.csv',head=T)
x=create_cnt[create_cnt$년도=='2014',][-1]
lab=colnames(x)
x2=t(x)
x2

########################################123
library(shiny)
library(datasets)
create_cnt <- read.csv("창업건수.csv", header=T)
drop_cnt <- read.csv("폐업건수.csv", header=T) 

# Define UI ----
ui <- fluidPage(    
  # Give the page a title
  titlePanel("연도별 업종별 창업현황"),
  # Generate a row with a sidebar
  sidebarLayout(      
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "업종:", 
                  choices=create_cnt$년도),
      hr(),
      helpText("업종별 창업과 폐업 현황 보고서")
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
        x=create_cnt[create_cnt$년도==input$region,-1]
        pct=round(x/sum(x)*100,1)
        lab=paste(colnames(x),pct,'%')
        pie(t(x), main='연도별 업종 창업 비율', col=rainbow(7),labels=lab)
        
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)

############################## 선생님 답#################################
library(shiny)
library(datasets)

create_cnt <- read.csv("창업건수.csv", header=T)
drop_cnt <- read.csv("폐업건수.csv", header=T) 
# Define UI ----
ui <- fluidPage(    
  # Give the page a title
  titlePanel("년도별 업종별 창업현황"),
  # Generate a row with a sidebar
  sidebarLayout(      
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "년도:", 
                  choices=create_cnt$년도 ) ,
      helpText("년도별 창업 현황 보고서")
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
    x2 <-  create_cnt[ create_cnt$년도==input$region, -1  ]
    cnt_labels <- round( x2/sum(x2) * 100, 1) 
    cnt_labels2 <-  paste( colnames(cnt_labels) ,cnt_labels ,'%') 
    pie(  t(x2)  , col=rainbow(7), labels=cnt_labels2) 
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)

########################################124
create_cnt <- read.csv("창업건수.csv", header=T)
drop_cnt <- read.csv("폐업건수.csv", header=T) 

# Define UI ----
ui <- fluidPage(    
  # Give the page a title
  titlePanel("연도별 업종별 창/폐업현황"),
  # Generate a row with a sidebar
  sidebarLayout(      
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "업종:", 
                  choices=create_cnt$년도),
      hr(),
      helpText("업종별 창업과 폐업 현황 보고서")
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
    x=create_cnt[create_cnt$년도==input$region,-1]
    pct=t(x)/sum(t(x))
    lab=paste(colnames(x),)
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
lab
pct=
  lab2=paste(x,lab,'')
pie(x, main='2014년도 업종별 창업 비율', )

##############################################
library(shiny)
library(datasets)

create_cnt <- read.csv("창업건수.csv", header=T)
drop_cnt <- read.csv("폐업건수.csv", header=T) 
# Define UI ----
ui <- fluidPage(    
  # Give the page a title
  titlePanel("연도별 업종별 폐업현황"),
  # Generate a row with a sidebar
  sidebarLayout(      
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "년도:", 
                  choices=drop_cnt$년도 ) ,
      helpText("연도별 폐업 현황 보고서")
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
    x2 <-  drop_cnt[ drop_cnt$년도==input$region, -1  ]
    cnt_labels <- round( x2/sum(x2) * 100, 1) 
    cnt_labels2 <-  paste( colnames(cnt_labels) ,cnt_labels ,'%') 
    pie(  t(x2)  , col=rainbow(7), labels=cnt_labels2) 
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)

######################################## 125 ggplot2
library(shiny)
library(datasets)
library(data.table)
library(ggplot2)

create_cnt <- read.csv("창업건수.csv", header=T)
drop_cnt <- read.csv("폐업건수.csv", header=T) 
# Define UI ----
ui <- fluidPage(    
  # Give the page a title
  titlePanel("년도별 업종별 창업현황"),
  # Generate a row with a sidebar
  sidebarLayout(      
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "년도:", 
                  choices=create_cnt$년도 ) ,
      helpText("년도별 창업 현황 보고서")
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
    a=create_cnt[create_cnt$년도==input$region,-1]
    a=data.table(colnames(a),t(a))
    colnames(a)=c('가게','건수')
    per=round(a$건수/sum(a$건수)*100,1)
    ggplot(a, aes(x = "", y = 건수, fill=가게))+
    geom_bar(width = 1, stat = "identity",color='white')+
    coord_polar("y")+
    geom_text(aes(label = paste0(per,"%")),position = position_stack(vjust = 0.5))
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)

################################################ 라인 그래프 그리기################################################

######################################## 128
cars=c(1,3,6,4,9)
trucks=c(2,5,4,5,12)
plot(cars,type='o',col='blue',ylim=c(0,12),pch=21, lty=1)
lines(trucks, type='o',col='red',ylim=c(0,12),pch=22,lty=2)

######################################## 129
cars=c(1,3,6,4,9)
trucks=c(2,5,4,5,12)
plot(cars,type='o',col='blue',ylim=c(0,12),pch=21, lty=1,axes=F,ann=F) 
lines(trucks, type='o',col='red',ylim=c(0,12),pch=22,lty=2)
# 새로운 축 추가
axis(1, at=1:5, lab=c('월','화','수','목','금')) # axis(1:x축, at= : 범위, lab= : 축 라벨)
axis(2) # axis(2) : y축
box()
g_range<-range(0,create_cnt$치킨집, drop_cnt$치킨집) # 0에서 부터 둘 중 큰 값까지
legend(8,g_range[2],c('창업','폐업'), cex=0.7, col=c('blue','red'),pch=22, lty=1:2)

######################################## 130
create_cnt <- read.csv("창업건수.csv", header=T)
drop_cnt <- read.csv("폐업건수.csv", header=T) 
g <- create_cnt$치킨집
m <- drop_cnt$치킨집
g_range <- range(0, g, m)
plot(g, type="o", col="blue", ylim=g_range, 
     axes=FALSE, ann=FALSE)
axis(1, at=1:10, lab=create_cnt$년도)
axis(2)
box()
lines(m, type="o", pch=22, lty=2, col="red")
title(main="치킨집 창업/폐업현황", col.main="red")
title(xlab="year" )
title(ylab="Total" )
legend( 8, g_range[2], c("창업","폐업"), cex=0.8, col=c("blue","red"), pch=21:22, lty=1:2)

######################################## 131
library(shiny)
library(datasets)
create_cnt <- read.csv("창업건수.csv", header=T)
drop_cnt <- read.csv("폐업건수.csv", header=T) 

# Define UI ----
ui <- fluidPage(    
  # Give the page a title
  titlePanel("연도별 업종별 창/폐업현황"),
  # Generate a row with a sidebar
  sidebarLayout(      
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("region", "업종:", 
                  choices=colnames(create_cnt)[-1]),
      hr(),
      helpText("업종별 창업과 폐업 현황 보고서")
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
    g <- create_cnt[,input$region]
    m <- drop_cnt[,input$region]
    g_range <- range(0, g, m)
    plot(g, type="o", col="blue", ylim=g_range, 
         axes=FALSE, ann=FALSE)
    axis(1, at=1:10, lab=create_cnt$년도)
    axis(2)
    box()
    lines(m, type="o", pch=22, lty=2, col="red")
    title(main="업종별 창업/폐업현황", col.main="red")
    title(xlab="year" )
    title(ylab="Total" )
    legend( 8, g_range[2], c("창업","폐업"), cex=0.8, col=c("blue","red"), pch=21:22, lty=1:2)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)

