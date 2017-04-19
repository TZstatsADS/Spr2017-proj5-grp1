library(rvest)
query_list<-c("BOS","CHI","WAS","ATL","TOR","MIL","CLE","IND",
              "GSW","POR","LAC","UTA","HOU","OKC","SAS","MEM")

update<-list()
for (i in 1:length(query_list)){
  url<-paste0("http://www.basketball-reference.com/teams/",query_list[i],"/2017/gamelog/")
  table<-read_html(url) %>%
    html_nodes("table")%>%
    html_table()
  result<-table[[1]][90:nrow(table[[1]]),]
  colnames(result)<-table[[1]][89,]
  update[[i]]<-result
}

names(update)<-query_list

save(update,file="../data/PerGame_playoff_2017.RData")
