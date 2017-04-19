####Load trainging and testing data:
##Source function:
source("../lib/clean_data.R")
##Specify Path:
train.path="../data/PerGame_2015/"
test.path="../data/PerGame_playoff_2016/"

tra=list.files(path = train.path, pattern = "*.csv")
tes=list.files(path = test.path, pattern = "*.csv")

train_data<-as.list(1:length(tra))
test_data<-as.list(1:length(tes))

names(train_data)<-tra
names(test_data)<-tes

##Read as list:
for (i in tra){
  train_data[[i]]<-read.csv(paste(train.path,i,sep = ""),header = T,as.is=T)
}


for (i in tes){
  test_data[[i]]<-read.csv(paste(test.path,i,sep = ""),header = T,as.is=T)
}

##Data cleaning and converting to matrix form:
train_data<-lapply(train_data,clean_data)
test_data<-lapply(test_data,clean_data)


train_data<-Reduce(rbind,train_data)
test_data<-Reduce(rbind,test_data)

