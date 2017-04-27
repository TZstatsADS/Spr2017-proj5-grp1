###Load trainging and testing data:
###Source function:
source("../lib/function.R")
#source("../lib/simulations_each_layer.R")

####Specify Path:

train.path1="../data/PerGame_2015/"
train.path2="../data/PerGame_2016/"
train.path3="../data/PerGame_playoff_2016/"
train.path.all<-c(train.path1,train.path2,train.path3)
  
tra1<-list.files(path = train.path1, pattern = "*.csv")
tra2<-list.files(path = train.path2, pattern = "*.csv")
tra3<-list.files(path = train.path3, pattern = "*.csv")



data_name<-c(paste(train.path1,tra1,sep=""),
             paste(train.path2,tra2,sep=""),paste(train.path3,tra3,sep=""))
train_data<-as.list(1:length(data_name))
n<-length(train_data)

##Read as list:
for (i in 1:n){
  train_data[[i]]<-read.csv(data_name[i],header = T,as.is=T)
}
length(train_data)

##Combined the historical data according to teams:
tra<-substr(tra1, start=1, stop=nchar(tra1)-9)
train_data2<-as.list(1:16)
for(j in 1:16){
  train_data2[[j]]<-rbind(train_data[[j]],train_data[[16+j]],train_data[[32+j]])
}
rm(train_data)
names(train_data2)<-tra


##Data Cleaning
train_data<-lapply(train_data2,clean_data)
lapply(train_data,dim)

##get historical performace
####Get the averge performance
Ave_performace<-lapply(train_data,get_average)
Ave_performace<-Reduce(rbind,Ave_performace)
d<-ncol(Ave_performace)
rownames(Ave_performace)<-tra

##Convert trining data to matrix:
train_data<-Reduce(rbind,train_data)

##Obtain the test data:
#test_data<-read.csv("../data/Test.csv")
test_data<-read.csv("../data/Test_regular.csv")
test_data_festures<-test_data
n_test<-nrow(test_data)
d_test<-ncol(test_data)

##Update features for the testing data:
for(i in 1:n_test){
  teamA<-test_data$TeamA[i]
  opp<-test_data$Opp[i]
  ind1<-which(tes1==teamA)
  ind2<-which(tes1==opp)
  test_data_festures[i,3:34]<-c(Ave_performace[ind1,],Ave_performace[ind2,])
  Ave_performace[ind1,]<-(Ave_performace[ind1,]+as.numeric(test_data[i,3:18]))/2
  Ave_performace[ind2,]<-(Ave_performace[ind2,]+as.numeric(test_data[i,19:34]))/2
}


##Converting to matrix form:
train_data<-Reduce(rbind,train_data)
test_data_festures<-test_data_festures[,-c(1,2)]
dim(train_data)
dim(test_data_festures)
#colnames(train_data)
#colnames(test_data)
##


##################################################
##Modelling:XGBOOST:
#####################################################
library(xgboost)

####Data Preperation:
Train.x<- data.matrix(train_data[,-c(ncol(train_data)-1)])
Train.y<-train_data[,c(ncol(train_data)-1)]
Test.x<-data.matrix(test_data_festures[,-c(ncol(test_data_festures)-1)])
Test.y<-test_data_festures[,c(ncol(test_data_festures)-1)]


Train.x<- data.matrix(Train.x,rownames.force = NA)
Train.D <- xgb.DMatrix(data=Train.x,label=Train.y,missing = NaN)
Test.x<- data.matrix(Test.x,rownames.force = NA)
Test.D <- xgb.DMatrix(data=Test.x,label=Test.y,missing = NaN)

#write.csv(test_data_festures,"../data/test_final.csv",row.names = F)
######CV to select the best parameters:
depth.choice<- c(5,6,7,8)
eta.choice<- seq(0.1,0.5,0.1)
#Initilize:
error<-matrix(NA,nrow = length(eta.choice),ncol = length(depth.choice))
iteration<-matrix(NA,nrow = length(eta.choice),ncol = length(depth.choice))
##################
train.sd<-matrix(NA,nrow = length(eta.choice),ncol = length(depth.choice))
test.sd<-matrix(NA,nrow = length(eta.choice),ncol = length(depth.choice))
for (i in 1:length(depth.choice)) {
  for (j in 1:length(eta.choice) ) {
    parameters <- list ( objective = "binary:logistic",
                         #booser = "gbtree",
                         eta = eta.choice[j],
                         max_depth = depth.choice[i],
                         subsample = 0.5,
                         gamma = 0)
    crossvalid <- xgb.cv( params = parameters,
                          data = Train.D,
                          nrounds = 100,
                          verbose = 1,
                          maximize = FALSE,
                          nfold = 5,
                          early_stopping_rounds = 8,
                          print_every_n = 1)
    iteration[j,i]<-crossvalid$best_iteration
    error[j,i]<-as.numeric(crossvalid$evaluation_log[crossvalid$best_iteration,4])
  }
}

best.index<-which(error == min(error), arr.ind = TRUE)
depth.choose<-depth.choice[best.index[1,2]]
era.choose<-eta.choice[best.index[1,1]]
iteration.choose<-iteration[best.index[1,1],best.index[1,2]]
parameters <- list ( objective = "binary:logistic",
                     #booser = "gbtree",
                     eta = era.choose,
                     max_depth = depth.choose,
                     subsample = 0.5,
                     gamma = 0)
####################
##Train the model###
####################
fit_xgboost<-xgb.train( params              = parameters,
                        data                = Train.D,
                        nrounds             = 100, 
                        verbose             = 1,
                        maximize            = FALSE)

####################
##Get the prediction:
####################

###Testing:
prediction <- predict (fit_xgboost,Test.x)
prediction<-as.numeric(prediction > 0.5)
mean(prediction!=Test.y)

##Training:
prediction2 <- predict (fit_xgboost,Train.x)
prediction2<-as.numeric(prediction2 > 0.5)
mean(prediction2!=Train.y)
