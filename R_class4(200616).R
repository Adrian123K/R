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

