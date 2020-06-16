getwd()

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

#114

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


#115
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
