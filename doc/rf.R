# Get data
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

########################################################################################
########################################################################################

# Random Forest
library(randomForest)

Test.y<-test_data_festures[,c(ncol(test_data_festures)-1)]

ntree<- ntree <- seq(100, 600, by=100)
err<-NULL
for (i in 1:length(ntree) ) {
  rf <- randomForest(as.factor(y) ~ . - y, data = train_data,ntree=ntree[i])
  err[i]<-mean(rf$predicted!=train_data$y)
}
ntree.best<-ntree[which.min(err)]
model <- randomForest(as.factor(y) ~ . - y, data = train_data,ntree=ntree.best)
mean(model$predicted!=train_data$y)
pred <- predict(model, newdata =test_data_festures)
mean(pred!=Test.y)
