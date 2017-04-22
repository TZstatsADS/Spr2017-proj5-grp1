library(rvest)

#---------------------------------2015-2016----------------------------------------------
query_list<-c("october", "november", "december", "january", "february", "march","april", "may", "june")

update<-NA
for (i in 1:length(query_list)){
  url<-paste0("http://www.basketball-reference.com/leagues/NBA_2016_games-", query_list[i], ".html")
  table<-read_html(url) %>%
    html_node("table")%>%
    html_table()
  result<-table[, c(1:6)]
  update<-rbind(update,result)
}

update <- update[-1, ]
write.csv(update, file = "../data/nba_2016.csv")

#---------------------------------2016-2017----------------------------------------------
query_list<-c("october", "november", "december", "january", "february", "march","april")

update<-NA
for (i in 1:length(query_list)){
  url<-paste0("http://www.basketball-reference.com/leagues/NBA_2017_games-", query_list[i], ".html")
  table<-read_html(url) %>%
    html_node("table")%>%
    html_table()
  result<-table[, c(1:6)]
  update<-rbind(update,result)
}

update <- update[-1, ]
write.csv(update, file = "../data/nba_2017.csv")

