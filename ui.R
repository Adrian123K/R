
packages <- c("shinydashboard", "shiny", "ggplot2", "plotly", "lattice")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  
  install.packages(setdiff(packages, rownames(installed.packages())))
}

require(shinydashboard)
require(shiny)
require(ggplot2)
require(plotly)
require(lattice)



sidebar <- dashboardSidebar(
  sidebarMenu(
    fileInput("file1", "Choose CSV File",
              multiple = FALSE,
              accept = c("text/csv",".xlsx",".txt",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    menuItem("Table",
             menuSubItem('Tableformat',tabName='tableformat') ),
    
    menuItem("Graph",
             menuSubItem('Barplot',tabName='barplot'),
             menuSubItem('Piechart',tabName='piechart'),
             menuSubItem('Lineplot',tabName='lineplot'),
             menuSubItem('Scatterplot',tabName='scatterplot'),
             menuSubItem('boxplot',tabName='boxplot')
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
    ),
    
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
    ),
    ##### scatter plot
    tabItem(tabName = "boxplot",
            sidebarPanel(
              selectInput("in_sel_box_xVar","x Variable:", choices = NULL)
              
            ),
            mainPanel(
              plotOutput('plot_box')
            )
    )
  )
)


ui<-dashboardPage(
  dashboardHeader(title='my graph'),
  sidebar,
  body
)

