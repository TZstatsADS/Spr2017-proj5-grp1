
#install.packages("magrittr")
#install.packages("highcharter")

#########################
#Pre-Processing
#########################
library(magrittr)
library(highcharter)
load("../output/training_output/Ave_performace.RData")
rown<-rownames(Ave_performace)
Ave_performace<-round(Ave_performace,3)
rownames(Ave_performace)<-rown
Ave_performace<-as.data.frame(Ave_performace)



#####################################################
#graph funtion 3
####################################################
###This is the function for generating graph 3:
graph_choice3<-function(xname="TRB",yname="FG"){
  ind1<-which(colnames(Ave_performace)==xname)
  ind2<-which(colnames(Ave_performace)==yname)
  
  highchart() %>% 
    hc_title(text = "Highchart of Winning Rate vs Selected Variable") %>% 
    hc_xAxis(title=list(text = xname))%>% 
    hc_yAxis(title=list(text = yname))%>% 
    hc_add_series_scatter(Ave_performace[,ind1],Ave_performace[,ind2],
                          Ave_performace$y,Ave_performace$y)
  ##四个位置分别对应：x,y,zise,color
}


graph_choice3()

