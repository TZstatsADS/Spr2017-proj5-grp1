
##Load data and data preperation:
opp<-read.csv("C:/Users/Jihan Wei/Desktop/Opp.csv")[1:30,]
team<-read.csv("C:/Users/Jihan Wei/Desktop/team.csv")[1:30,]
short<-read.csv("C:/Users/Jihan Wei/Desktop/short.csv",header = F)
short<-short[order(short$V1),]
colnames(opp)<-paste(colnames(opp),"_opp")
colnames(opp)[2]<-"Team"
X<-merge(opp,team,by.x="Team")
X<-X[order(X$Team),]
X$short<-short$V2
X$Team<-X$short
n<-nrow(X)
d<-ncol(X)
X<-X[,-c(3,d)]

##Get new dimension:
n<-nrow(X)
d<-ncol(X)

###
y<-read.csv("C:/Users/Jihan Wei/Desktop/y_20162017.csv")
y<-na.omit(y)
#y<-rbinom(30,0,1)

##Construct the design Matrix:
X_full1<-NULL
for(i in 1:n){
  for(j in 1:29){
    X_full1<-rbind(X_full1,X[i,])
  }
}

X_full2<-NULL
for(i in 1:n){
  X_full2<-rbind(X_full2,X[-i,])
}
##
X_Full<-cbind(X_full1,X_full2)

##
colnames(X_Full)[1:d]<-paste("A_",colnames(X_Full)[1:d])

##Construct training data:
train_data<-as.data.frame(cbind(X_Full,y[,2]))
rownames(train_data)<-y[,1]
colnames(train_data)[ncol(train_data)]<-"y"
train_data<-train_data[,-c(1,d+1)]


##Build model:
logit1<-glm(y ~ .,family = "binomial", data = train_data)

