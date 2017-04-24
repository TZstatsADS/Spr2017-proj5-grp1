# input: individual link
# output: the first table in this link
Parse_html = function(link){
  table_list <- read_html(link) %>% # load the page
    html_nodes("table") %>% # isloate the text
    html_table(header = T)
  Table = table_list[[1]] # only need the first table
  return(Table)
}