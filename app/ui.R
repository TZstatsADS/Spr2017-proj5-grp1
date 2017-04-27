## Packages

packages.used <- 
  c("shiny","shinydashboard","shinythemes","XML", "highcharter", "rCharts", "devtools","shinyLP")

# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

#load the packages

library(shiny)
library(shinydashboard)
library(shinythemes)
library(XML)
library("highcharter")
library(rCharts)
library(radarchart)
library(devtools)
featureList<-c("FG","FGA","FG.","X3P","X3PA",
                "X3P.","FT","FTA","FT.","ORB",
                "TRB","AST","STL","BLK","TOV",
                "PF","EFF","ELO")
playoff16<-c("BOS","CHI","WAS","ATL","TOR","MIL","CLE","IND","GSW","POR","LAC","UTA","HOU","OKC","SAS","MEM")

shinyUI(navbarPage(theme = "bootstrap.min-copy.css","Who is the champion",id="nav",
                   
                   
                   tabPanel("Welcome",
                            div(id="canvas"),
                            mainPanel(
                              HTML('<center><img src="img/top.jpg" width="1380"></center>'),
                              img(src='img/left.jpg', align = "left"),
                              img(src='img/right.jpg', align = "right"),
                              h1("Who is the Champion ?",style = "font-family: 'Ranga';
                                  color: #800020; text-align: center;padding: 20px"),
                              
                              h1("Team Members:", align = "center"),
                              h2("Qingyuan Zhang", align = "center"),
                              h2("Jihan Wei", align = "center"),
                              h2("Ruxue Peng", align = "center"),
                              h2("Yue Jin", align = "center"),
                              h2("Zixuan Guan", align = "center"),
                              width = 12)
      
                   ),
                   
                   
                   tabPanel("Prediction",div(id="canvas",style="text-align:center"),
                      mainPanel(
                        h2("Prediction Video",br(),tags$video(src = "img/lol.mp4", type = "video/mp4", autoplay = NA, controls = NA,width = "100%"),align="center")
                      )
                   ),
                  
                   tabPanel("Statistics",div(id="canvas"),
                            h2("Summary Statistics"),
                            
                            wellPanel(style = "overflow-y:scroll; height: 850px; max-height: 750px; background-color: #ffffff;",
                                      tabsetPanel(type="tabs",
                                                  tabPanel(title="Feature Selection",
                                                           br(),
                                                           fixedRow(
                                                             column(3, selectInput(inputId = "Feature_1", label = "Select first feature",
                                                                                   choices = featureList, selected = "TRB")),
                                                             column(4, selectInput(inputId = "Feature_2", label = "Select second feature",
                                                                                   choices = featureList))),
                                                           
                                                             column(6, highchartOutput("plot_1",height=600, width =800)),
                                                              img(src='img/table_1.png', align = "right",height=600, width =400,style = "opacity: 0.72")
                                                           
                                                  ),
                                                  tabPanel(title="Team Performance",
                                                           br(),
                                                           fixedRow(
                                                             column(3, selectInput(inputId = "Team", label = "Select a team",
                                                                                   choices = playoff16, selected = "GSW"))),
                                                             
                                                             mainPanel(
                                                               div(class='wrapper',showOutput("plot_2", "nvd3"))
                                                             )
                                                             
                                                  ),
                                                  tabPanel(title="Team Comparation",
                                                           br(),
                                                           fixedRow(
                                                             column(3, selectInput(inputId = "team_1", label = "Select first team",
                                                                                   choices = playoff16, selected = "GSW")),
                                                             column(4, selectInput(inputId = "team_2", label = "Select second team",
                                                                                   choices = playoff16))),
                                                           br(),
                                                           column(6, chartJSRadarOutput("plot_3",height=600, width =800))
                                                  )
                                                  
                                      )
                            )
                   )
         
))