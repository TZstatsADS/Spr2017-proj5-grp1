########################################################
##Get_simulations
########################################################
#source("../lib/for_training_simulations/simulation_function.R")

###The for the 4-layers simulations:
teams<-c("GSW","POR","LAC","UTA","HOU","OKC","SAS","MEM",
        "BOS","CHI","WAS","ATL","TOR","MIL","CLE","IND")
WininngProb1<-WininngProb
WininngProb2<-WininngProb
WininngProb3<-WininngProb
WininngProb4<-WininngProb
pre1<-simulation_layer(teams,WininngProb=WininngProb1,iter=1000)
pre2<-simulation_layer(pre1,WininngProb=WininngProb2,iter=1000)
pre3<-simulation_layer(pre2,WininngProb=WininngProb3,iter=1000)

#for the final round:
pre4<- simulation_layer(pre3,WininngProb=WininngProb3,iter=1000)

pre1
pre2
pre3
pre4

