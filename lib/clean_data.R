clean_data<-function(dataset){
  dataset$y<-ifelse(dataset$W.L=="W",1,0)
  dataset$home<-ifelse(dataset$X=="@",1,0)
  dataset<-dataset[,-c(1:8)]
  return(dataset)
}

