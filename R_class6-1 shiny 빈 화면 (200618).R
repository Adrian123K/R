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
  
)

body <- dashboardBody(
  
)

ui<-dashboardPage(
  dashboardHeader(title='my graph'),
  sidebar,
  body
  
)

######################3. 서버단 개발 ########################################

server <- function(input, output,session) {
  options(warn = -1) ###################### 가장 앞에 창이 뜨도록 설정
  options(shiny.maxRequestSize = 30*1024^2)
  dataload<-reactive({
    
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
}

######################### 4. 샤이니 실행 ###############################

shinyApp(ui = ui, server = server)