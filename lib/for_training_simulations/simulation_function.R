################
##This function is designed for one round simulation:
################
simulation_layer<-function(teams,WininngProb,iter=1000){
  No_teams<-length(teams)
  TeamAS<-teams[seq(1,No_teams-1,by=2)]
  TeamBS<-teams[seq(2,No_teams,by=2)]
  namelist<-paste(TeamAS,TeamBS,sep = "_")
  
  prob<-WininngProb[WininngProb[,1]%in%namelist,]
  ans<-matrix(NA,iter+2,No_teams/2)
  ans[iter+1,]<-TeamAS
  ans[iter+2,]<-TeamBS
  
  if(!is.null(dim(prob))){
    ##make sure the order is same as the input:
    prob<-prob[order(factor(prob[,1],levels = namelist)),]
    Simu_matrix<-matrix(NA,nrow = nrow(prob),ncol = 9)
    Simu_matrix[,1]<-order(namelist)
    for (k in 1:iter){
      Simu_matrix[,2:9]<-t(apply(prob,1,one_round))
      ans[k,]<-Simu_matrix[,9]
    }
  } else{
    an<-c()
    for (k in 1:iter){
      ans[k,]<-one_round(prob)[8]
    }
  }
  predictions<-apply(ans,2,function(x){return(names(table(x))[which.max(table(x))])})
  return(predictions)
}

