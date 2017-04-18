data_clean <- function(name, year = "2016") {
  df <- read.csv(paste0("../data/raw_2016/", name, "_", year, ".csv"), header = T)
  df <- df[, -c(1, 25)]
  colnames(df)[7] <- "Opp"
  colnames(df)[24:39] <- gsub(".1", "_opp", colnames(df)[24:39])
  return(df)
}




name_vec <- c("CEL", "CHI", "WAS", "ATL", "TOR", "MIL",  "CLE", "IND", 
              "GSW", "POR", "LAC", "UTA", "HOU", "OKC", "SAS", "MEM")
m <- sapply(name_vec, data_clean)
for (i in 1:dim(m)[2]) {
  x <- as.data.frame(m[, i])
  write.csv(x, file = paste0("../data/PerGame_2016/", name_vec[i], "_2016.csv"))
  
}










