clean_data<-function(dataset){
  dataset$y<-ifelse(dataset$W.L=="W",1,0)
  dataset$home<-ifelse(dataset$X=="@",1,0)
  dataset<-dataset[,-c(1:8)]
  return(dataset)
}

get_average<-function(dataset,weight){
  n<-nrow(dataset)
  d<-(ncol(dataset)-2)/2
  weight<-matrix(rep(weight,d),nrow = n,byrow = F)
  dataset<-dataset[,1:d]*weight
  return(colSums(dataset))
}