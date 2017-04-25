# input : basic score table for 1 game
# output: player played and minutes played table
ExtractMP = function(Mat){
  colnames(Mat) = Mat[1,]
  Mat = Mat[-c(1,7,nrow(Mat)),]
  Mat = Mat[Mat[,2] != "Did Not Play",]
  name_MP = Mat[,c("Starters","MP")]
  #game_index = substr(links[1],nchar(links[1])-16,nchar(links[1])-5) #"201510270ATL"
  return(name_MP)
}
