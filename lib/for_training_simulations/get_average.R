##This function is used for getting average historical performance:
get_average<-function(dataset){
  n<-nrow(dataset)
  wt<-0.99^seq(n, 1, by = -1)
  wt<-wt/sum(wt)
  d<-(ncol(dataset)-2)/2
  weight<-matrix(rep(wt,d+2),nrow = n,byrow = F)
  dataset<-dataset[,c(1:d,ncol(dataset)-1,ncol(dataset))]*weight
  return(colSums(dataset))
}

