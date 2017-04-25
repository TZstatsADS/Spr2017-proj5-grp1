clean_data<-function(dataset){
  dataset$y<-ifelse(dataset$W.L=="W",1,0)
  dataset$home<-ifelse(dataset$X=="@",0,1)
  dataset<-dataset[,-c(1:8)]
  return(dataset)
}

get_average<-function(dataset){
  n<-nrow(dataset)
  wt<-0.9^seq(n, 1, by = -1)
  wt<-wt/sum(wt)
  d<-(ncol(dataset)-2)/2
  weight<-matrix(rep(wt,d),nrow = n,byrow = F)
  dataset<-dataset[,1:d]*weight
  return(colSums(dataset))
}


##this function is for simulation for one round with two teams
##it will give the simulated winnier
##Input: prob: the winning rate for teamA(7); teamsA means Bos for BOS_LAC; 
#length of teams_prob should be 8(take "Home" into consideration)
one_round<-function(teams_prob){  
  teams<-teams_prob[1]
  i<-2
  outcome<-c()
  while(sum(outcome)<4 & length(outcome)<7){
    prob<-as.numeric(teams_prob[i])
    pre<-sample(c(0,1),1,prob = c(1-prob,prob))
    outcome<-c(outcome,pre)
    i=i+1
  }
  plays<-length(outcome)
  if(plays!=7){
    outcome[(plays+1):7]<-NA
  }
  if(sum(outcome,na.rm = T)>3){
    Win<-substr(teams, start=1, stop=3)
  }else{
    Win<-substr(teams, start=5, stop=7)
  }
  return(c(outcome,Win))
}

