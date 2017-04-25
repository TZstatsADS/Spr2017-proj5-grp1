#########################################
###Train the Model
#########################################
###Source function:
lib.path<-"../lib/for_training_simulations/"

##Sourse all functions:
functions=list.files(path = lib.path,pattern = "*.[Rr]")
for(i in 1:length(functions)){
  source(paste(lib.path,functions[i],sep=""))
}


###Load trainging data:
train.path="../data/data_use/training/"
tra<-list.files(path = train.path, pattern = "*.csv")
train_data<-as.list(1:length(tra))
n<-length(train_data)

##Read as list:
for (i in 1:n){
  train_data[[i]]<-read.csv(paste(train.path,tra[i],sep=""),header = T,as.is=T)
}

train_data<-lapply(train_data,clean_data)
lapply(train_data,dim)


##Convert trining data to matrix:
train_data<-Reduce(rbind,train_data)
#dim(train_data)
##get the model:
fit_model<-fit_xgboost(train_data)

##Save data and model
#save(train_data,file="../output/training_output/train_data.RData")
#save(fit_model,file="../output/training_output/xgboost_model.RData")

#################################################
##get historical performace for Each Team
#################################################
###Source function and functions:
#load("../output/training_output/xgboost_model.RData")
lib.path<-"../lib/for_training_simulations/"


##Sourse all functions:
functions=list.files(path = lib.path,pattern = "*.[Rr]")
for(i in 1:length(functions)){
  source(paste(lib.path,functions[i],sep=""))
}

####Get the averge performance based on the 2016 data
ave.path="../data/data_use/averge_performance/"
ave<-list.files(path = ave.path, pattern = "*.csv")
ave_data<-as.list(1:length(ave))
m<-length(ave_data)
##Read as list:
for (i in 1:m){
  ave_data[[i]]<-read.csv(paste(ave.path,ave[i],sep=""),header = T,as.is=T)
}


##Get the most updated data:
##Pre-Processes the data:
ave_data<-lapply(ave_data,clean_data)
Ave_performace<-lapply(ave_data,get_average)
Ave_performace<-Reduce(rbind,Ave_performace)
#dim(Ave_performace)

##Save it for viluzalization:
#save(Ave_performace,file="../output/training_output/Ave_performace.RData")

###Con't
d<-ncol(Ave_performace)
Ave_performace<-Ave_performace[,-c(d-1,d)]
d<-ncol(Ave_performace)
ave<-substr(ave, start=1, stop=3)
rownames(Ave_performace)<-ave
dim(Ave_performace)

####################################################
#Construct the test matrix
###################################################
L<-length(ave)

name<-c()
test_data<-data.frame()
for (i in 1:L){
  name2<-paste(ave[i],ave[-i],sep = "_")
  teami<-matrix(rep(Ave_performace[i,],L-1),ncol = ncol(Ave_performace),byrow = T)
  new<-cbind(teami,Ave_performace[-i,])
  test_data<-rbind(test_data,new)
  name<-c(name,name2)
}
rownames(test_data)<-name
dim(test_data)
##make the colnames of the test data consistent with the training model:

test_data1<-cbind(test_data,rep(1,nrow(test_data)))
test_data0<-cbind(test_data,rep(0,nrow(test_data)))
colnames(test_data0)<-c(colnames(train_data)[1:(ncol(train_data)-2)],"home")
colnames(test_data1)<-c(colnames(train_data)[1:(ncol(train_data)-2)],"home")


##Based home and elo ranking: 
Test.x1<- data.matrix(test_data1,rownames.force = NA)
Test.x2<- data.matrix(test_data0,rownames.force = NA)

prediction1 <- predict(fit_model,Test.x1)  ##predicted wining rate for home=1
prediction2 <- predict(fit_model,Test.x2)  ##predicted wining rate for home=0


##Get Probability Matrix:
WininngProb<-cbind(name,prediction1,prediction1,prediction2,prediction2,prediction1,prediction2,prediction1)

##Save outputs:
#save(WininngProb,file="../output/training_output/WininngProb.RData")

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
pre1<-simulation_layer(teams,WininngProb=WininngProb1,iter=5000)
pre2<-simulation_layer(pre1,WininngProb=WininngProb2,iter=5000)
pre3<-simulation_layer(pre2,WininngProb=WininngProb3,iter=5000)

#for the final round:
pre4<- simulation_layer(pre3,WininngProb=WininngProb3,iter=5000)
pre1
pre2
pre3
pre4
#save(pre1,pre2,pre3,pre4,file="../output/training_output/simulation_results.RData")
