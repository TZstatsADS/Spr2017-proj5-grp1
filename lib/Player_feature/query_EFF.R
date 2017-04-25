# season: eg. 2015/2016regular  2015/2016playoffs 2016/2017regular  2016/2017playoffs
# Table: contain the lineup data from each game, use the element in MP list.
query_EFF = function(Table,EFF_table,season){
  Name = Table[,1]
  # get the EFF from the right season
  EFF_table = EFF_table[EFF_table$season == season,]
  # Parse first name and last name
  L = strsplit(Name,split = " ")
  FirstInitial = laply(L,function(Name) substr(Name[1],1,1))
  Lastname = laply(L,function(Name) Name[2])     
  shortN = paste0(FirstInitial,". ",Lastname)
  # query the EFF score for each player
  EFFscore = as.numeric(apply(as.matrix(shortN),1,function(x) EFF_table$EFF[EFF_table$Player == x][1]))
  return(data.frame(Name,EFFscore))
}
