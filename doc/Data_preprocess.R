####Load trainging and testing data:
##Source function:
source("../lib/clean_data.R")
source("../lib/Extract.R")
source("../lib/add_zero_to_date.R")
##Specify Path:
train.path="../data/PerGame_2015/"
test.path="../data/PerGame_2016/"

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
test_data = llply(test_data,add_zero_to_date)
##Construct test data
tmp = ldply(test_data,rbind)
tmp$.id = substr(tmp$.id,1,nchar(tmp$.id)-9)
# if data from playoff, use Extract(), from regular, use Extract_string()
Test = Extract_string(tmp)
Test$y<-ifelse(Test$W.L=="W",1,0)
Test$home<-ifelse(Test$X=="@",1,0)
Test = Test[,-c(2:5,7:9)]
colnames(Test)[1] = "TeamA"
write.csv(Test,"../data/Test_regular.csv",row.names = F)

train_data<-Reduce(rbind,train_data)
test_data<- Test
#write.csv(train_data,"../data/Train.csv",row.names = F)
