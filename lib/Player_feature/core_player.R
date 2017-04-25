# Extract the player that played for over 10 mins
coreplayer = function(t){
  rule = laply(strsplit(t[,2],split = ":"),function(vec) as.numeric(vec[1])) >= 10
  return(t[rule,])
}
