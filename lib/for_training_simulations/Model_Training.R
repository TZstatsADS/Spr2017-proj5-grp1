#########################################
###Train the Model
#########################################
###Source function:
lib.path<-"../lib/for_training_simulations/"

##Sourse all functions:
functions=list.files(path = lib.path,pattern = "*.[Rr]")
for(i in 1:length(functions)){
  source(paste(lib.path,functions[i],sep=""))
}


###Load trainging data:
train.path="../data/data_use/training/"
tra<-list.files(path = train.path, pattern = "*.csv")
train_data<-as.list(1:length(tra))
n<-length(train_data)

##Read as list:
for (i in 1:n){
  train_data[[i]]<-read.csv(paste(train.path,tra[i],sep=""),header = T,as.is=T)
}

train_data<-lapply(train_data,clean_data)
lapply(train_data,dim)

##Convert trining data to matrix:
train_data<-Reduce(rbind,train_data)
#dim(train_data)
##get the model:
fit_model<-fit_xgboost(train_data)

##Save data and model
save(train_data,file="../output/training_output/train_data.RData")
save(fit_model,file="../output/training_output/xgboost_model.RData")
