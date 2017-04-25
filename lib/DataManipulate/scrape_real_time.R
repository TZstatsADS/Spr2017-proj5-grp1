library(stringr)
query_list<-c("BOS","CHI","WAS","ATL","TOR","MIL","CLE","IND",
              "GSW","POR","LAC","UTA","HOU","OKC","SAS","MEM")

string<-"Rk	G	Date	Opp	W/L	Tm	Opp	FG	FGA	FG%	3P	3PA	3P%	FT	FTA	FT%	ORB	TRB	AST	STL	BLK	TOV	PF	FG	FGA	FG%	3P	3PA	3P%	FT	FTA	FT%	ORB	TRB	AST	STL	BLK	TOV	P"
colnames<-unlist(str_split(string,"\t"))

update<-list()
for (i in 1:length(query_list)){
  url<-paste0("http://www.basketball-reference.com/teams/",query_list[i],"/2017/gamelog/")
  a<-readLines(url)
  mypattern = "id=\"tgl_basic_playoffs.[1-9]+\""
  datalines = grep(mypattern,a,value=TRUE)
  get_data = ">([.]?[A-Z0-9-]+)<"
  result<-str_match_all(datalines,get_data)
  data<-NULL
  for (j in 1:length(result)){
    data<-rbind(data,result[[j]][,2])  
  }
  colnames(data)<-colnames
  file.name<-paste0("../data/raw_playoff_2017/",query_list[i],".RData")
  save(data,file=file.name)
}
