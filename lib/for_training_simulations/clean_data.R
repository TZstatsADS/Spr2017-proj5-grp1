##This Function is for data clearning:
clean_data<-function(dataset){
  dataset$y<-ifelse(dataset$W.L=="W",1,0)
  dataset$home<-ifelse(dataset$X=="@",0,1)
  ind<-which(colnames(dataset)=="FG")
  dataset<-dataset[,-c(1:(ind-1))]
  ind2<-which(colnames(dataset)=="ELO")
  ind3<-which(colnames(dataset)=="ELO_opp")
  dataset[,ind2]<-dataset[,ind2]/1000
  dataset[,ind3]<-dataset[,ind3]/1000
  return(dataset)
}
