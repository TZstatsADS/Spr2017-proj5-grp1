# The date column changes format for the playoffs data, so we wrote a new function to create links
## The link follows pattern: /date + 0 + nameabbreviation +.html
## eg. http://www.basketball-reference.com/boxscores/201510290NYK.html

# input: Data, a table containing all game one certain team played in a certain season
# input: order, the order of the file, eg. "ATL_2015.csv" is the 1st file
# input: File_list, all the filename under the path
# output: link for each game


Make_link_playoffs = function(Datatable, order,File_list){
  # cleaning Date
  Date = Datatable$Date
  Date = gsub("-","",Date)
  # cleaning Team name abbreviation
  name_list = substr(File_list,1,3)
  index = which(Datatable$X == "@")
  Teamname = rep(name_list[[order]],length(Datatable$Date))
  Teamname[index] = Datatable$Opp[index]
  links = paste0("http://www.basketball-reference.com/boxscores/",Date,"0",Teamname,".html")
  return(links)
}