data_clean <- function(name, year = "2016") {
  df <- read.csv(paste0("../data/raw_",year,"/", name, "_", year, ".csv"), header = T)
  df <- df[, -c(1, 25)]
  colnames(df)[7] <- "Opp"
  colnames(df)[24:39] <- gsub(".1", "_opp", colnames(df)[24:39])
  return(df)
}




name_vec <- c("BOS", "CHI", "WAS", "ATL", "TOR", "MIL",  "CLE", "IND", 
              "GSW", "POR", "LAC", "UTA", "HOU", "OKC", "SAS", "MEM")
m <- sapply(name_vec, data_clean)

for (i in 1:dim(m)[2]) {
  x <- as.data.frame(m[, i])
  write.csv(x, file = paste0("../data/PerGame_2016/", name_vec[i], "_2016.csv"))
  
}

m <- sapply(name_vec, data_clean, year = "2015")
for (i in 1:dim(m)[2]) {
  x <- as.data.frame(m[, i])
  write.csv(x, file = paste0("../data/PerGame_2015/", name_vec[i], "_2015.csv"))
  
}


name_playoff <- c("CLE", "TOR", "MIA", "ATL", "BOS", "CHO", "IND", "DET", 
                  "GSW", "SAS", "OKC", "LAC", "POR", "DAL", "MEM", "HOU")

data_clean <- function(name, year = "2016") {
  df <- read.csv(paste0("../data/raw_playoff_",year,"/", name, "_", year, ".csv"), header = T)
  df <- df[, -c(1, 25)]
  colnames(df)[7] <- "Opp"
  colnames(df)[24:39] <- gsub(".1", "_opp", colnames(df)[24:39])
  return(df)
}

m <- sapply(name_playoff, data_clean)
for (i in 1:dim(m)[2]) {
  x <- as.data.frame(m[, i])
  write.csv(x, file = paste0("../data/PerGame_playoff_2016/", name_playoff[i], "_2016.csv"))
  
}





