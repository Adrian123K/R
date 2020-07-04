
packages <- c("shinydashboard", "shiny", "ggplot2", "plotly", "lattice")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  
  install.packages(setdiff(packages, rownames(installed.packages())))
}

require(shinydashboard)
require(shiny)
require(ggplot2)
require(plotly)
require(lattice)
require(RColorBrewer)



######################### 2. 화면 개발 ###########################################

sidebar <- dashboardSidebar(
  sidebarMenu(
    # fileInput : csv 파일 및 다양한 파일들을 불러오는 화면 구현
    fileInput("file1", "Choose CSV File",
              multiple = FALSE,
              accept = c("text/csv",".xlsx",".txt",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    menuItem("Table",
             menuSubItem('Tableformat',tabName='tableformat') ),
    
    # 그래프 종류 보여줌
    menuItem("Plot",
             menuSubItem('barplot',tabName='barplot'),
             menuSubItem('barplot_advenced',tabName='barplotly'),
             menuSubItem('pieplot',tabName='piechart'),
             menuSubItem('lineplot',tabName='lineplot'),
             menuSubItem('scatterplot',tabName='scatterplot'),
             menuSubItem('boxplot',tabName='boxplot'),
             menuSubItem('histogram',tabName='histplot')
    )
  )
)


body <- dashboardBody(
  tabItems(
    
    ##### table_format
    tabItem(tabName = "tableformat",
            
            mainPanel(
              DT::dataTableOutput("table")
            )
    )
  ),
  
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
    
    ##### advanced bar plot
    tabItem(tabName = "barplotly",
            sidebarPanel(
              selectInput("in_sel_barly_show","show", choices = NULL),
              selectInput("in_sel_barly_yVar","y Variable:", choices = NULL),
              selectInput("in_sel_barly_xVar","x Variable:", choices = NULL)
              
            ),
            mainPanel(
              plotlyOutput('plotly_bar')
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
    ),
    ##### box plot
    tabItem(tabName = "boxplot",
            sidebarPanel(
              selectInput("in_sel_box_Var1","Variable 1:", choices = NULL),
              selectInput("in_sel_box_Var2","Variable 2:", choices = NULL)
              
            ),
            mainPanel(
              plotlyOutput('plot_box')
            )
    ),
    
    ##### hist plot
    tabItem(tabName = "histplot",
            sidebarPanel(
              selectInput("in_sel_hist_Var1","Variable 1:", choices = NULL)   
            ),
            mainPanel(
              plotlyOutput('plot_hist')
            )
    )
    
    
  )
)



ui<-dashboardPage(
  skin='purple',
  dashboardHeader(title='Adrian Auto',titleWidth = 250),
  sidebar,
  body
)


######################3. 서버단 개발 ########################################


server <- function(input, output,session) {
  options(warn = -1)
  options(shiny.maxRequestSize = 30*1024^2)
  
  
  
  
  dataload<-reactive({
    # 서버로 해당 파일 불러옴
    req(input$file1)
    
    # 파일이름을 file1 변수에 넣음
    file1 = input$file1
    # 파일이름.csv가 불러와져서 data1에 로드됨
    data1 = read.csv(file1$datapath)
    
    
    updateSelectInput(session, "in_sel_bar_xVar", choices = colnames(data1))
    updateSelectInput(session, "in_sel_bar_yVar", choices = colnames(data1))
    
    updateSelectInput(session,"in_sel_barly_show",choices=c('col','row'))
    updateSelectInput(session, "in_sel_barly_xVar", choices = colnames(data1))
    updateSelectInput(session, "in_sel_barly_yVar", choices = colnames(data1))
    
    updateSelectInput(session, "in_sel_pie_xVar", choices = data1[,1])
    
    updateSelectInput(session, "in_sel_line_xVar", choices = colnames(data1))
    updateSelectInput(session, "in_sel_line_yVar", choices = colnames(data1))
    
    updateSelectInput(session, "in_sel_scatter_xVar", choices = colnames(data1))
    updateSelectInput(session, "in_sel_scatter_yVar", choices = colnames(data1))
    
    updateSelectInput(session, "in_sel_box_Var1", choices = colnames(data1))
    updateSelectInput(session, "in_sel_box_Var2", choices = colnames(data1))
    
    updateSelectInput(session, "in_sel_hist_Var1", choices = colnames(data1))
    
    return(data1)
    
  })
  
  
  ####table_format
  output$table <- DT::renderDataTable(DT::datatable({
    req(input$file1)
    
    file1 = input$file1
    data1 = read.csv(file1$datapath)
    
    
  }))
  
  ####nomal_bar
  output$plot_bar <- renderPlot({
    table_in<-dataload()
    
    xdata<-as.factor(table_in[,input$in_sel_bar_xVar])
    ydata<-as.factor(table_in[,input$in_sel_bar_yVar])
    fdata=data.frame(x=xdata,y=ydata)
    
    
    ggplot(fdata) + 
      geom_bar(aes_string(x='x',y='y',fill='x'),stat = "identity",show.legend=F)
    
    
  })
  
  ####advenced_bar
  output$plotly_bar <- renderPlotly({
    table_in<-dataload()
    
    if(input$in_sel_barly_show=='col'){
      plot_ly(x=~table_in[,input$in_sel_barly_xVar],y=~table_in[,input$in_sel_barly_yVar],type='bar',marker = list(color = brewer.pal(nrow(table_in), "Paired"))) %>%
        layout(xaxis= list(title=input$in_sel_barly_xVar) , yaxis = list(title=input$in_sel_barly_yVar))
    }
    
    else if(input$in_sel_barly_show=='row'){
      plot_ly(x=~table_in[,input$in_sel_barly_xVar],y=~table_in[,input$in_sel_barly_yVar],type='bar',orientation = "h",marker = list(color = brewer.pal(nrow(table_in), "Paired"))) %>%
        layout(xaxis= list(title=input$in_sel_barly_xVar) , yaxis = list(title=input$in_sel_barly_yVar))
    }
    
  })
  
  ####pie
  output$plot_pie <- renderPlotly({
    table_in<-dataload()
    
    plot_ly(table_in, labels = ~colnames(table_in)[-1], values=~as.factor( table_in[table_in[,1] == input$in_sel_pie_xVar,-1] ),type='pie')
    
    
  })
  
  
  ####line
  output$plot_line <- renderPlotly({
    table_in<-dataload()
    
    x <- list(title = input$in_sel_line_xVar)
    y <- list(title = input$in_sel_line_yVar)
    
    plot_ly(data = table_in,x=~table_in[,input$in_sel_line_xVar],y=~table_in[,input$in_sel_line_yVar],type='scatter',mode='dot')%>%
      layout(xaxis = x, yaxis = y)
    
    
  })
  
  
  ####scatter
  output$plot_scatter <- renderPlot({
    table_in<-dataload()
    
    xyplot(table_in[,input$in_sel_scatter_yVar]~table_in[,input$in_sel_scatter_xVar], grid=T,type=c('p','smooth'),col.line='darkorange',lwd=2, xlab=input$in_sel_scatter_xVar,ylab=input$in_sel_scatter_yVar)
    
  })
  
  output$text_scatter <- renderText({
    table_in<-dataload()
    paste("The correlation between the two is: ", cor(table_in[,input$in_sel_scatter_yVar],table_in[,input$in_sel_scatter_xVar]))
  })
  
  ####boxplot(사분위수)
  output$plot_box <- renderPlotly({
    table_in<-dataload()
    
    x <- list(title = input$in_sel_line_xVar)
    
    subplot(
      add_markers = plot_ly(data = table_in, y=~table_in[,input$in_sel_box_Var1],type='box',name=input$in_sel_box_Var1) %>%
        layout(yaxis=list(title=input$in_sel_box_Var1)),
      add_markers = plot_ly(data = table_in, y=~table_in[,input$in_sel_box_Var2],type='box',name=input$in_sel_box_Var2) %>%
        layout(yaxis=list(title=input$in_sel_box_Var2))
    )
    
    
  })
  
  #### histogram(히스토그램)
  output$plot_hist <- renderPlotly({
    table_in<-dataload()
    
    x <- list(title = input$in_sel_hist_Var1)
    
    plot_ly(x=~table_in[,input$in_sel_hist_Var1],
            type = "histogram")  %>% 
      layout(title = "Histogram",
             xaxis = list(title = x,
                          zeroline = FALSE),
             yaxis = list(title = "Count",
                          zeroline = FALSE))
    
  })
  
}

shinyApp(ui = ui, server = server)

