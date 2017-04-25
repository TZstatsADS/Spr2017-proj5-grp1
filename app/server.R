
library(shiny)

playoff16<-c("BOS","CHI","WAS","ATL","TOR","MIL","CLE","IND","GSW","POR","LAC","UTA","HOU","OKC","SAS","MEM")
#source("../lib/radarchart.R")
#source("../lib/rchart.R")
#source("../lib/graphs_choice3.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$plot_3 <- renderPlot({
    a<-which(input$team_1==playoff16)
    b<-which(input$team_2==playoff16)
    
    radarfunction(a,b)
  })
  
  output$plot_1 <- renderPlot({
    # rchartfunction(input$Team)
    1
  })
  
  output$plot_2 <- renderPlot({
    #graphs_choice3(input$Feature_1, input$Feature_2)
    2
  })
})
