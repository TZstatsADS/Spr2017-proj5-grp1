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

