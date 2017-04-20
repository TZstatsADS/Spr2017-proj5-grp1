install.packages("magrittr")
install.packages("highcharter")


library(magrittr)
library(highcharter)

source("../lib/function.R")
####Specify Path:
path="../data/PerGame_2015/"

tes=list.files(path = path, pattern = "*.csv")
tes1<-substr(tes, start=1, stop=nchar(tes)-9)
train_data<-as.list(1:length(tes))

names(train_data)<-tes

####Read as list:
for (i in tes){
  train_data[[i]]<-read.csv(paste(path,i,sep = ""),header = T,as.is=T)
}

##
#test_data<-read.csv("../data/Test_regular.csv")
#test_data_festures<-test_data
#n_test<-nrow(test_data)
#d_test<-ncol(test_data)


##Update features for the testing data:
train_data<-lapply(train_data,clean_data)

##
get_average2<-function(dataset,weight){
  n<-nrow(dataset)
  col<-ncol(dataset)
  d<-(col-2)/2
  weight<-matrix(rep(weight,d+2),nrow = n,byrow = F)
  dataset<-dataset[,c(1:d,col-1,col)]*weight
  return(colSums(dataset))
}
####Get the averge performance
  Ave_performace<-lapply(train_data,get_average2,weight=rep(1/82,82))
  Ave_performace<-as.data.frame(Reduce(rbind,Ave_performace))
  d<-ncol(Ave_performace)
  rownames(Ave_performace)<-tes1
  colnames(Ave_performace)
head(Ave_performace)
highchart() %>% 
  hc_title(text = "Winning rate with size and color") %>% 
  hc_add_series_scatter(Ave_performace$FG,Ave_performace$X3P,
                        Ave_performace$y,Ave_performace$y)
##四个位置分别对应：x,y,zise,color