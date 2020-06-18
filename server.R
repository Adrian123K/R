
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
    
    updateSelectInput(session, "in_sel_box_xVar", choices = colnames(data1))
    
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
  
  output$plot_box <- renderPlot({
    table_in<-dataload()
    
    bwplot(~table_in[,input$in_sel_box_xVar], data=table_in,xlab=input$in_sel_box_xVar)
    
  })
  
  
}
