
############## set this file location to working directory ##########################
packages <- 'rstudioapi'
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
library('rstudioapi')
current_dir<-dirname(rstudioapi::getSourceEditorContext()$path)
setwd(current_dir)

# package_in : 없으면 설치, 있으면 library화 하라는 함수
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

package_in('shinydashboard') # shinydashboard 패키지가 없으면 설치하고 있으면 library화 하라는 뜻
package_in('shiny')
package_in('ggplot2')
package_in('plotly')
package_in('lattice')

######################### 2. 화면 개발 ###########################################

sidebar <- dashboardSidebar(
  sidebarMenu(
    # fileInput : csv 파일 및 다양한 파일들을 불러오는 화면 구현
    fileInput("file1", "Choose CSV File",
              multiple = FALSE,
              accept = c("text/csv",".xlsx",".txt",".jpg",".png",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    
    # 그래프 종류 보여줌
    menuItem("Plot",
             menuSubItem('막대그래프',tabName='barplot'),
             menuSubItem('원형그래프',tabName='piechart'),
             menuSubItem('라인그래프',tabName='lineplot'),
             menuSubItem('사분위수그래프',tabName='boxplot'),
             menuSubItem('산포도',tabName='scatterplot')
             
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
    ##### box plot
    tabItem(tabName = "boxplot",
            sidebarPanel(
              selectInput("in_sel_box_yVar","y Variable:", choices = NULL),
              selectInput("in_sel_box_xVar","x Variable:", choices = NULL)
            ),
            mainPanel(
              plotlyOutput('plot_box')
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
  skin='purple',
  dashboardHeader(title='Adrian Data Visualizing Program',titleWidth=250),
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
    
    updateSelectInput(session, "in_sel_pie_xVar", choices = data1[,1])
    
    updateSelectInput(session, "in_sel_line_xVar", choices = colnames(data1))
    updateSelectInput(session, "in_sel_line_yVar", choices = colnames(data1))
    
    updateSelectInput(session, "in_sel_box_xVar", choices = colnames(data1))
    updateSelectInput(session, "in_sel_box_yVar", choices = colnames(data1))
    
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
  
  output$plot_box <- renderPlotly({
    table_in <- dataload()
    subplot(
      add_markers = plot_ly(data = table_in, y=~table_in[,input$in_sel_box_xVar],type='box',name=input$in_sel_box_xVar) %>%
        layout(yaxis=list(title=input$in_sel_box_xVar)),
      add_markers = plot_ly(data = table_in, y=~table_in[,input$in_sel_box_yVar],type='box',name=input$in_sel_box_yVar) %>%
        layout(yaxis=list(title=input$in_sel_box_yVar))
    )
  })
  
  output$plot_scatter <- renderPlot({
    table_in<-dataload()
    xyplot(table_in[,input$in_sel_scatter_yVar]~table_in[,input$in_sel_scatter_xVar], grid=T,type=c('p','smooth'),col.line='darkorange',lwd=2, xlab=input$in_sel_scatter_xVar,ylab=input$in_sel_scatter_yVar)
  })
}

######################### 4. 샤이니 실행 ###############################

shinyApp(ui = ui, server = server)
