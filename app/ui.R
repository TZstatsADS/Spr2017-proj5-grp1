## Packages

packages.used <- 
  c("shiny","shinydashboard","shinythemes","XML")

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
                   
                   tabPanel("Prediction",div(id="canvas"),
                            mainPanel(img(src='img/playoff.jpg', align = "center",width="1380", style = "opacity: 0.72"))
                            ),
                   
                  
                   tabPanel("Statistics",div(id="canvas"),
                            h2("Summary Statistics"),
                            
                            wellPanel(style = "overflow-y:scroll; height: 850px; max-height: 750px; background-color: #ffffff;",
                                      tabsetPanel(type="tabs",
                                                  tabPanel(title="Feature Selection",
                                                           br(),
                                                           div(plotOutput("plot_1"), align="center")
                                                  ),
                                                  tabPanel(title="Team Comparation",
                                                           br(),
                                                           div(plotOutput("plot_2"), align="center")
                                                  ),
                                                  tabPanel(title="Team Performance",
                                                           br(),
                                                           div(plotOutput("plot_3"), align="center")
                                                  )
                                                  
                                      )
                            )
                   )
         
))