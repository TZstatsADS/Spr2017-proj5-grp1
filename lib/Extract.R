Extract =  function(df){
  new_df = df[order(as.Date(df$Date)),]  
  return(new_df)
}

Extract_string =  function(df){
  new_df = df[order((df$Date)),]  
  return(new_df)
}
