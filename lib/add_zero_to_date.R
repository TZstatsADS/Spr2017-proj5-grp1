add_zero_to_date = function(df){
  L = strsplit(df$Date,"/")
  need_zero_index = which(ldply(L, function(t) nchar(t[1])) == 1)
  df$Date[need_zero_index] = paste0("0",df$Date[need_zero_index])
  return(df)
}

