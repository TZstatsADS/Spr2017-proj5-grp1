###Load trainging and testing data:
###Source function:
source("../lib/function.R")

####Specify Path:
train.path="../data/all/"
train.path="../data/PerGame_2016/"  

tra=list.files(path = train.path, pattern = "*.csv")
tra1<-substr(tra, start=1, stop=nchar(tra)-9)
train_data<-as.list(1:length(tra))

names(train_data)<-tra

##Read as list:
for (i in tra){
  train_data[[i]]<-read.csv(paste(train.path,i,sep = ""),header = T,as.is=T)
}

##Data Cleaning
train_data<-lapply(train_data,clean_data)

##get historical performace
####Get the averge performance
Ave_performace<-lapply(train_data,get_average,weight=rep(1/82,82))
Ave_performace<-Reduce(rbind,Ave_performace)
d<-ncol(Ave_performace)
rownames(Ave_performace)<-tra1
###
train_data<-Reduce(rbind,train_data)
#dim(train_data)

####################################################
#############Modelling:XGBOOST#######################
#####################################################
library(xgboost)

####Data Preperation:
Train.x<- data.matrix(train_data[,-c(ncol(train_data)-1)])
Train.y<-train_data[,c(ncol(train_data)-1)]
Train.D <- xgb.DMatrix(data=Train.x,label=Train.y,missing = NaN)

####################
##Train the model###
####################
parameters <- list ( objective = "binary:logistic",
                     #booser = "gbtree",
                     eta = 0.4,
                     max_depth = 8,
                     subsample = 0.5,
                     gamma = 0)
fit_xgboost<-xgb.train( params              = parameters,
                        data                = Train.D,
                        nrounds             = 100, 
                        verbose             = 1,
                        maximize            = FALSE)



#############################################################
####################Get Test Matrix #########################
#############################################################
Q<-rownames(Ave_performace)
L<-length(Q)
test_data<-matrix(NA,nrow=length(name),ncol = 2*ncol(Ave_performace))

name<-c()
test_data<-data.frame()
for (i in 1:L){
  name2<-paste(Q[i],Q[-i],sep = "_")
  teami<-matrix(rep(Ave_performace[i,],L-1),ncol = ncol(Ave_performace),byrow = T)
  new<-cbind(teami,Ave_performace[-i,])
  test_data<-rbind(test_data,new)
  name<-c(name,name2)
}
dim(test_data)
##make the colnames of the test data consistent with the training model:


test_data1<-cbind(test_data,rep(1,nrow(test_data)))
dim(test_data1)
colnames(test_data1)<-c(colnames(train_data)[1:32],"home")
test_data0<-cbind(test_data,rep(0,nrow(test_data)))
colnames(test_data0)<-c(colnames(train_data)[1:32],"home")

Test.x1<- data.matrix(test_data1,rownames.force = NA)
Test.x2<- data.matrix(test_data0,rownames.force = NA)

prediction1 <- predict (fit_xgboost,Test.x1)  ##predicted wining rate for home=1
prediction2 <- predict (fit_xgboost,Test.x2)  ##predicted wining rate for home=0

##Get Probability Matrix:
WininngProb<-cbind(name,prediction1,prediction1,prediction2,prediction2,prediction1,prediction2,prediction1)
#dim(WininngProb)



prediction<-simulations_all(n=5000,WininngProb,WininngProb,WininngProb,WininngProb)
table(prediction)
