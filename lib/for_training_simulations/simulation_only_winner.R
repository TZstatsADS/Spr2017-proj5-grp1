#############################################################
#######################Simulation only for the winner############################
#############################################################
simulations_all<-function(n,WininngProb1,WininngProb2,WininngProb3,WininngProb4){
  Winner<-c()
  for (i in 1:n){
    ##First found
    namelist<-c("GSW_POR","LAC_UTA","HOU_OKC","SAS_MEM",
                "BOS_CHI","WAS_ATL","TOR_MIL","CLE_IND")
    prob<-WininngProb1[WininngProb[,1]%in%namelist,]
    prob<-prob[order(factor(prob[,1],levels = namelist)),]
    Simu_matrix<-matrix(NA,nrow = nrow(prob),ncol = 9)
    Simu_matrix[,1]<-order(namelist)
    Simu_matrix[,2:9]<-t(apply(prob,1,one_round))
    
    ##Second Round:
    A<-Simu_matrix[c(1,3,5,7),9]
    B<-Simu_matrix[c(2,4,6,8),9]
    namelist<-paste(A,B,sep="_")
    prob<-WininngProb2[WininngProb[,1]%in%namelist,] 
    prob<-prob[order(factor(prob[,1],levels = namelist)),]
    Simu_matrix<-matrix(NA,nrow = nrow(prob),ncol = 9)
    Simu_matrix[,1]<-order(namelist)
    Simu_matrix[,2:9]<-t(apply(prob,1,one_round))
    
    ##Third Round:
    A<-Simu_matrix[c(1,3),9]
    B<-Simu_matrix[c(2,4),9]
    namelist<-paste(A,B,sep="_")
    prob<-WininngProb3[WininngProb[,1]%in%namelist,]  
    prob<-prob[order(factor(prob[,1],levels = namelist)),]
    Simu_matrix<-matrix(NA,nrow = nrow(prob),ncol = 9)
    Simu_matrix[,1]<-order(namelist)
    Simu_matrix[,2:9]<-t(apply(prob,1,one_round))
    
    ###Final Round:
    namelist<-paste(Simu_matrix[1,9],Simu_matrix[2,9],sep = "_")
    prob<-WininngProb4[WininngProb[,1]%in%namelist,]  
    Winner[i]<-one_round(prob)[8]
  }
  Winner<-names(table(Winner))[which.max(table(Winner))]
  return(Winner)
}
