# input: Data, a table containing all game one certain team played in a certain season
# input: order, the order of the file, eg. "ATL_2015.csv" is the 1st file
# input: File_list, all the filename under the path

# output: link for each game

Make_link = function(Datatable,order,File_list){
  ## The link follows pattern: /date + 0 + nameabbreviation +.html
  ## eg. http://www.basketball-reference.com/boxscores/201510290NYK.html
  ## The name is the team that plays at home.
  
  Date = Datatable$Date
  Date_split = strsplit(Date,split = "/")
  # cleaning Date
  f_mon = function(vec){ifelse((nchar(vec[1]) == 1),paste0("0",vec[1]),vec[1])}
  f_day = function(vec){ifelse((nchar(vec[2]) == 1),paste0("0",vec[2]),vec[2])}
  f_year = function(vec){paste0("20",vec[3])}
  Date_mon = laply(Date_split,f_mon)
  Date_day = laply(Date_split,f_day)
  Date_year = laply(Date_split,f_year)
  # cleaning Team name abbreviation
  name_list = substr(File_list,1,(nchar(File_list)-9))
  index = which(Datatable$X == "@")
  Teamname = rep(name_list[[order]],length(Datatable$Date))
  Teamname[index] = Datatable$Opp[index]
  # generate links
  links = paste0("http://www.basketball-reference.com/boxscores/",Date_year,Date_mon,Date_day,"0",Teamname,".html")
  rm(Date_day,Date_mon,Date_year,index,Date_split,Date)
  return(links)
}