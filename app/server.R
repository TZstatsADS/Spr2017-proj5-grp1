
library(shiny)
library(magrittr)
library(highcharter)
library(radarchart)
library(devtools)
library(rCharts)
library(ggplot2)
library(reshape2)
#####################################################
#graph funtion 1
####################################################
###This is the function for generating graph 1:
load("./output/training_output/Ave_performace.RData")
rown<-rownames(Ave_performace)
Ave_performace<-round(Ave_performace,3)
rownames(Ave_performace)<-c("ATL","BOS","CHI","CLE","GSW","HOU","IND","LAC","MEM","MIL","OKC","POR","SAS",
                            "TOR","UTA","WAS")
Ave_performace<-as.data.frame(Ave_performace)

graph_choice1<-function(xname="TRB",yname="FG"){
  ind1<-which(colnames(Ave_performace)==xname)
  ind2<-which(colnames(Ave_performace)==yname)
  
  highchart() %>% 
    hc_add_theme(hc_theme_sandsignika())%>%
    hc_title(text = "Highchart of Winning Rate vs Selected Variable") %>% 
    hc_xAxis(title=list(text = xname))%>% 
    hc_yAxis(title=list(text = yname))%>% 
    hc_add_series_scatter(Ave_performace[,ind1],Ave_performace[,ind2],
                          Ave_performace$y,Ave_performace$y,rownames(Ave_performace),
                          dataLabels = list(
                            enabled = TRUE,
                            format = "{point.label}"
                          ))%>%
    hc_chart(zoomType = "xy") %>% 
    hc_tooltip(useHTML = TRUE,
               headerFormat = "<table>",
               pointFormat = paste("<tr><th colspan=\"1\"><b>{point.label}</b></th></tr>"),
               footerFormat = "</table>")
  
}

#####################################################
#graph funtion 2
####################################################
###This is the function for generating graph 1:
team<-read.csv("./data/baseline/team.csv")[1:30,]
short<-read.csv("./data/baseline/short.csv",header = F)
playoff16<-c("BOS","CHI","WAS","ATL","TOR","MIL","CLE","IND","GSW","POR","LAC","UTA","HOU","OKC","SAS","MEM")
short<-short[order(short$V1),]
colnames(team)[2]<-"Team"
team<-team[order(team$Team),]
team$short<-short$V2
team$Team<-team$short
tn<- team$Team %in% playoff16
newdata<- team[tn,]
n<-nrow(team)
d<-ncol(team)
newdata<-as.data.frame(cbind(newdata$FG.,newdata$X3P.,newdata$TRB,
                             newdata$AST,newdata$STL,newdata$BLK))
average<-apply(newdata,2,mean)
radardata<-matrix(NA,ncol = 6,nrow=16)
for (i in 1:16){
  for (j in 1:6){
    radardata[i,j]<-newdata[i,j]/average[j]
  }
}
radardata<-as.data.frame(radardata)
labs<-c("FG%","X3P%","TRB","AST","STL","BLK")



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$plot_1 <- renderHighchart({
    graph_choice1(input$Feature_1, input$Feature_2)
  })
  
  output$plot_2 <- renderChart2({
    team<-paste0(input$Team,"_2015.csv")
    path<-paste0("./data/PerGame_2015/",team)
    data<-read.csv(path,header = T)
    data<-as.data.frame(data)
    data<-cbind(data$G,data$Tm,data$Opp.1)
    data<-as.data.frame(data)
    
    #####Choose Variables Time of a Game and Opp.1
    colnames(data)<-c("No","Tm","Opp.1")
    df.melt.id <- as.data.frame.array(melt(data, id="No"))
    p<-nPlot(value ~ No, group = 'variable', data = df.melt.id, type = 'lineWithFocusChart')
    p$chart(color = c('red', 'blue'))
    p
  })
  
  output$plot_3 <- renderChartJSRadar({
    a<-which(input$team_1==playoff16)
    b<-which(input$team_2==playoff16)
    scores<-list(
      "team1"=as.numeric(radardata[a,1:6]),
      "team2"=as.numeric(radardata[b,1:6]))
    
    chartJSRadar(scores = scores, labs = labs, 
                 scaleStartValue=min(radardata[a,],radardata[b,])+0.01,
                 maxScale = max(radardata[a,],radardata[b,])+0.01,
                 showToolTipLabel=TRUE,main = "Compare two teams average ability")
    
  })

})
