setwd("~/Desktop/5243 ADS/Spr2017-proj5-grp1-master/data/PerGame_2015")
gsw<-read.csv("GSW_2015.csv",header = T)
library(devtools)
#install_github('ramnathv/rCharts')
library(rCharts)
library(ggplot2)
gsw<-as.data.frame(gsw)
dim(gsw)
library(reshape2)
data<-cbind(gsw$G,gsw$Tm,gsw$Opp.1)
data<-as.data.frame(data)

#####Choose Variables Time of a Game and Opp.1
colnames(data)<-c("No","Tm","Opp.1")

df.melt.id <- as.data.frame.array(melt(data, id="No"))

p <- nPlot(value ~ No, group = 'variable', data = df.melt.id, type = 'lineWithFocusChart')
p$set(height=200)
p