fit_xgboost<-function(train_data){
  library(xgboost)
  
  ####Data Preperation:
  index<-which(colnames(train_data)=="y")
  Train.x<- data.matrix(train_data[,-index])
  Train.y<-train_data[,index]
  Train.D <- xgb.DMatrix(data=Train.x,label=Train.y,missing = NaN)
  
  #write.csv(test_data_festures,"../data/test_final.csv",row.names = F)
  ######CV to select the best parameters:
  depth.choice<- c(5,6,7,8)
  eta.choice<- seq(0.1,0.5,0.1)
  #Initilize:
  error<-matrix(NA,nrow = length(eta.choice),ncol = length(depth.choice))
  iteration<-matrix(NA,nrow = length(eta.choice),ncol = length(depth.choice))
  ##################
  train.sd<-matrix(NA,nrow = length(eta.choice),ncol = length(depth.choice))
  test.sd<-matrix(NA,nrow = length(eta.choice),ncol = length(depth.choice))
  for (i in 1:length(depth.choice)) {
    for (j in 1:length(eta.choice) ) {
      parameters <- list ( objective = "binary:logistic",
                           eta = eta.choice[j],
                           max_depth = depth.choice[i],
                           subsample = 0.5,
                           gamma = 0)
      crossvalid <- xgb.cv( params = parameters,
                            data = Train.D,
                            nrounds = 100,
                            verbose = 0,
                            maximize = FALSE,
                            nfold = 5,
                            early_stopping_rounds = 8,
                            print_every_n = 1)
      iteration[j,i]<-crossvalid$best_iteration
      error[j,i]<-as.numeric(crossvalid$evaluation_log[crossvalid$best_iteration,4])
    }
  }
  
  best.index<-which(error == min(error), arr.ind = TRUE)
  depth.choose<-depth.choice[best.index[1,2]]
  era.choose<-eta.choice[best.index[1,1]]
  iteration.choose<-iteration[best.index[1,1],best.index[1,2]]
  parameters <- list ( objective = "binary:logistic",
                       #booser = "gbtree",
                       eta = era.choose,
                       max_depth = depth.choose,
                       subsample = 0.5,
                       gamma = 0)
  ####################
  ##Train the model###
  ####################
  fit_xgboost<-xgb.train( params              = parameters,
                          data                = Train.D,
                          nrounds             = 100, 
                          maximize            = FALSE)
  return(fit_xgboost)
}