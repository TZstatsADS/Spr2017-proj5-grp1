txt_name = c("2014-2015.txt","2015-2016.txt","2016-2017.txt")
# Scrap response variable
library(plyr)
data <- read.csv(txt_name[2],header = F,sep = "\n",stringsAsFactors = F)    
rule = "data-stat="
test = strsplit(data$V1,split = rule)[-31]
oppoteam_name = unlist(llply(test[[1]],function(t) substring(t,1,3))[-c(1:3)])

# write a function to extract wining probability
f = function(Lelement){
  winprop_test = strsplit(Lelement,split = "csk=")
  p = unlist(llply(winprop_test,function(t) substr(t[2],1,5)))[-c(1:3)]
  return(p)
}

score = llply(test,f)

#put probs in a matrix
y_mat = matrix(NA,30,30)
colnames(y_mat) = oppoteam_name
rownames(y_mat) = oppoteam_name
for(i in 1:30){
  y_mat[i,] = score[[i]]
}
write.csv(y_mat,"y_matrix_20152016.csv")

#Convert into pair vector
Name = unlist(apply(as.matrix(oppoteam_name),1,function(t) paste0(t,"_",names(y_mat[1,]))))
Pair_y = as.numeric(t(y_mat))
names(Pair_y) = Name
Pair_y = as.matrix(Pair_y)
write.csv(Pair_y,"y_20152016.csv")
