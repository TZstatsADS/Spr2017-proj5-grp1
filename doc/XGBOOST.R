####Load trainging and testing data:
####Source function:
source("../lib/function.R")

####Specify Path:
train.path="../data/PerGame_2015/"
test.path="../data/PerGame_playoff_2016/"

tra=list.files(path = train.path, pattern = "*.csv")
tes=list.files(path = test.path, pattern = "*.csv")
tra1<-substr(tra, start=1, stop=nchar(tra)-9)
tes1<-substr(tes, start=1, stop=nchar(tes)-9)
  train_data<-as.list(1:length(tra))
test_data<-as.list(1:length(tes))

names(train_data)<-tra
names(test_data)<-tes

##Read as list:
for (i in tra){
  train_data[[i]]<-read.csv(paste(train.path,i,sep = ""),header = T,as.is=T)
}

#for (i in tes){
 # test_data[[i]]<-read.csv(paste(test.path,i,sep = ""),header = T,as.is=T)
#}


##Data Cleaning
train_data<-lapply(train_data,clean_data)

##get historical performace
####Get the averge performance
wt<-0.9^seq(82, 1, by = -1)
wt<-wt/sum(wt)
Ave_performace<-lapply(train_data,get_average,weight=rep(1/82,82))
Ave_performace<-Reduce(rbind,Ave_performace)
d<-ncol(Ave_performace)
rownames(Ave_performace)<-tes1


##Obtain the test data:
test_data<-read.csv("../data/Test.csv")
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
install.packages('xgboost')
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
