# Player Feature Construction #
library(plyr)
library(rvest)
source("../lib/Parse_html.R")
source("../lib/ExtractMP.R")
source("../lib/core_player.R")
source("../lib/query_EFF.R")
source("../lib/loadEFF.R")
source("../lib/Make_link.R")

# Load EFF data, data source: http://www.hoopsstats.com/basketball/fantasy/nba/playerstats/16/1/eff/7-1
# Create a score dictionary for us to query later
EFFdic = loadEFF()
##############################################################3
# scrap the player lineup of each game

# (1) generate html links

## The link follows pattern: /date + 0 + nameabbreviation +.html
## eg. http://www.basketball-reference.com/boxscores/201510290NYK.html
## The name is the team that plays at home.
# read in game data
setwd("../data/PerGame_2015")
File_list = list.files()
Data = list()
for(i in 1:length(File_list)){
  Data[[i]] = read.csv(File_list[i],as.is = T)
}

links = Make_link(Datatable = Data[[1]],File_list = File_list)
##############################################################

# (2) visit the link and store all the html code as txt

#  parse HTML file and extract the table
#  make sure you have internet connection for this line
table_list = alply(links,.margins = 1,Parse_html)
names(table_list) = substr(links,nchar(links)-16,nchar(links)-5)#"201510270ATL"

table_list_hometeam = alply(links,.margins = 1,Parse_html_hometeam)
names(table_list_hometeam) = substr(links,nchar(links)-16,nchar(links)-5)#"201510270ATL"
##############################################################

# (3) Extract the player name and MP

MP_all = llply(table_list,ExtractMP)
MP_all_hometeam = llply(table_list_hometeam,ExtractMP)
# only include players that have played for more than 10 minutes. 
MP = llply(MP_all,coreplayer)
MP_hometeam = llply(MP_all_hometeam,coreplayer)
##############################################################3

# (4) match lineup data with EFF data

EFFScore = ldply(MP,query_EFF,EFFdic,season = "2015/2016regular")
EFFScore_hometeam = ldply(MP_hometeam,query_EFF,EFFdic,season = "2015/2016regular")
# deal with NAs
EFFScore[is.na(EFFScore$EFFscore),]
EFFScore_hometeam[is.na(EFFScore_hometeam$EFFscore),]
# manually deal with reasonable ones
EFFScore$EFFscore[EFFScore$Name == "Tim Hardaway"] = 0
EFFScore_hometeam$EFFscore[EFFScore_hometeam$Name == "Tim Hardaway"] = 0
EFFScore$EFFscore[EFFScore$Name == "Justin Holiday"] = 
  EFFdic$EFF[EFFdic$Player == "Justin Holiday"&(EFFdic$season == "2015/2016regular")]
EFFScore_hometeam$EFFscore[EFFScore_hometeam$Name == "Justin Holiday"] = 
  EFFdic$EFF[EFFdic$Player == "Justin Holiday"&(EFFdic$season == "2015/2016regular")]
EFFScore$EFFscore[EFFScore$Name == "Marcus Morris"] = 
  EFFdic$EFF[EFFdic$Player == "Marcus Morris"&(EFFdic$season == "2015/2016regular")]
EFFScore_hometeam$EFFscore[EFFScore_hometeam$Name == "Marcus Morris"] = 
  EFFdic$EFF[EFFdic$Player == "Marcus Morris"&(EFFdic$season == "2015/2016regular")]
EFFScore$EFFscore[EFFScore$Name == "Markieff Morris"] = 
  EFFdic$EFF[EFFdic$Player == "Markieff Morris"&(EFFdic$season == "2015/2016regular")]
EFFScore_hometeam$EFFscore[EFFScore_hometeam$Name == "Markieff Morris"] = 
  EFFdic$EFF[EFFdic$Player == "Markieff Morris"&(EFFdic$season == "2015/2016regular")]
EFFScore = na.omit(EFFScore)
EFFScore_hometeam = na.omit(EFFScore_hometeam)

##############################################################3
# (5) output the final Feature matrix
roadteam_score = ddply(EFFScore,.variables = ".id",function(table) mean(table[,"EFFscore"]))
hometeam_score = ddply(EFFScore_hometeam,.variables = ".id",function(table) mean(table[,"EFFscore"]))
EFFscore_20152016regular = cbind(Hometeam = hometeam_score$V1,Roadteam = roadteam_score$V1)
rownames(EFFscore_20152016regular) = hometeam_score[,1]
# re-arrange according to original data structure
EFF = ifelse(Data[[1]]$X == "",EFFscore_20152016regular[,1],EFFscore_20152016regular[,2])
Opp_EFF = ifelse(Data[[1]]$X == "",EFFscore_20152016regular[,2],EFFscore_20152016regular[,1])
# insert the new feature into the data
secondhalf = Data[[1]][,25:40]
firsthalf = Data[[1]][,1:24]
firsthalf$EFF = EFF
secondhalf$EFF_opp = Opp_EFF
newdata = data.frame(firsthalf,secondhalf)
Filenames = paste0("../data/EFFadded_data/",substr(File_list,1,8),"_EFFadded.csv")
write.csv(newdata,Filenames[1])
