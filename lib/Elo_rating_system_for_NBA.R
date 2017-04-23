# BOS <- read.csv("../data/PerGame_2015/BOS_2015.csv")
# colnames(BOS)
# BOS[1,7]


#-------------------------------2016---------------------------
elo_old <- read.csv("~/Desktop/nbaallelo.csv")
a <- elo_old[c(125000:126314), ]
team_id <- unique(as.character(a$team_id))

get_elo <- function(name){
  df <- a[which(a$team_id==name), ]
  return(df$elo_n[nrow(df)])
}

elo_init <- sapply(team_id, get_elo)
elo_init <- elo_init[order(names(elo_init))]
elo_init <- (0.75*elo_init + 0.25*1505)
game_2016 <- read.csv("../data/nba_2016.csv", as.is = T)
game_2016 = game_2016[!is.na(as.numeric(game_2016$PTS)),]
game_2016[, 5] <- as.numeric(game_2016[, 5])
game_2016[, 7] <- as.numeric(game_2016[, 7])
dates_2016 <- unique(as.character(game_2016$Date))
game_all <- game_2016
team_name <- sort(unique(as.character(game_2016$Visitor.Neutral))[-31])
#team_name

# build a matrix that contains all elo rating values
allgames <- matrix(0, nrow = (length(dates_2016)+1), ncol = 31)
allgames <- as.data.frame(allgames)
colnames(allgames) <- c("Date", sort(team_name))
col_name <- colnames(allgames)
allgames[, 1] <- c("Initial", dates_2016)

# initial elo rating got from 538's historical data
allgames[1, -1] <- elo_init

# Write a function to calculate elo_rating for every game played.
game_index = 0
#----------------------------------------------------------------------
# M <- function(i, row_ind) {
#   pd <- game_all[i, 5] - game_all[i, 7]
#   win_name <- ifelse(pd > 0, game_all[i, 4], game_all[i, 6])
#   lose_name <- ifelse(pd < 0, game_all[i, 4], game_all[i, 6])
#   R_w <- allgames[row_ind, which(win_name==col_name)]
#   R_l <- allgames[row_ind, which(lose_name==col_name)]
#   m <- log(abs(pd)) *2.2/((0.001*(R_w-R_l))+2.2)
#   return(m)
# }

M <- function(i, row_ind, col_ind_a, col_ind_b) {
  pd <- game_all[i, 5] - game_all[i, 7]
  mov <- abs(pd) # margin of victory
  home_win <- ifelse(pd < 0, 1, 0)
  rank_A <- allgames[row_ind-1, col_ind_a]
  rank_B <- allgames[row_ind-1, col_ind_b]
  point_A <- game_all[i, 5]
  point_B <- game_all[i, 7]
  won_underdog <- ifelse((rank_A-rank_B)*(point_A-point_B)>0, 1, -1)
  elo_diff <- (abs(rank_A-rank_B) + 100*home_win)*won_underdog
  m <- (mov+3)^0.8/(7.5+0.006*elo_diff)
  return(m)
}

elo_rate_A <- function(i, row_ind, col_ind_a, col_ind_b) {
  R_O_i <- allgames[row_ind-1, col_ind_a]
  K = 20
  m <- M(i, row_ind, col_ind_a, col_ind_b)
  S_i_j <- ifelse(game_all[i, 5] < game_all[i, 7], 0, 1)
  R_O_j <- allgames[row_ind-1, col_ind_b]
  miu_i_j <- 1/(1+10^((R_O_i - R_O_j)/400))
  elo <- R_O_i + K *m*(S_i_j - miu_i_j)
  return(elo)
}

elo_rate_B <- function(i, row_ind, col_ind_a, col_ind_b) {
  R_O_i <- allgames[row_ind-1, col_ind_b]
  K = 20
  m <- M(i, row_ind, col_ind_a, col_ind_b)
  S_i_j <- ifelse(game_all[i, 7] < game_all[i, 5], 0, 1)
  R_O_j <- allgames[row_ind-1, col_ind_a]
  miu_i_j <- 1/(1+10^((R_O_i - R_O_j)/400))
  elo <- R_O_i + K *m*(S_i_j - miu_i_j)
  return(elo)
}

#----------------------------------------------------------------------
for (i in 1:nrow(game_all)) {
  # tian shangyihangnde jinlai
  TA_name = game_all[i,4]
  TB_name = game_all[i,6]
  tmp = game_all[i,2]
  TA_cindex = which(col_name==TA_name) # team A's column index in allgames
  TB_cindex = which(col_name==TB_name) # team B's column index in allgames
  game_index_old = game_index
  game_index = which(tmp == allgames[,1]) # row index in allgames that matches game date
  if (game_index - game_index_old != 0){
    allgames[game_index, -1] <- allgames[game_index-1, -1]
  }
  # tian liangge keng
  # do something
  allgames[game_index, TA_cindex] <- elo_rate_A(i, row_ind = game_index,
                                                col_ind_a = TA_cindex, 
                                                col_ind_b = TB_cindex)
  allgames[game_index, TB_cindex] <- elo_rate_B(i, row_ind = game_index,
                                                col_ind_a = TA_cindex, 
                                                col_ind_b = TB_cindex)
}

elo_2016 <- allgames
#------------------------------2017-----------------------
elo_init <- elo_2016[nrow(elo_2016),-1]
elo_init <- as.numeric(0.75*elo_init + 0.25*1505)
game_2017 <- read.csv("../data/nba_2017.csv", as.is = T)
game_2017 = game_2017[!is.na(as.numeric(game_2017$PTS)),]
game_2017[, 5] <- as.numeric(game_2017[, 5])
game_2017[, 7] <- as.numeric(game_2017[, 7])
dates_2017 <- unique(as.character(game_2017$Date))
game_all <- game_2017

# game_all <- rbind(game_2016, game_2017)
# game_all[, 5] <- as.numeric(game_all[, 5])
# game_all[, 7] <- as.numeric(game_all[, 7])
team_name <- sort(unique(as.character(game_2017$Visitor.Neutral))[-31])
#team_name

# build a matrix that contains all elo rating values
allgames <- matrix(0, nrow = (length(dates_2017)+1), ncol = 31)
allgames <- as.data.frame(allgames)
colnames(allgames) <- c("Date", sort(team_name))
col_name <- colnames(allgames)
allgames[, 1] <- c("Initial", dates_2017)

# initial elo rating got from 538's historical data
allgames[1, -1] <- elo_init

# Write a function to calculate elo_rating for every game played.
game_index = 0
#----------------------------------------------------------------------
# M <- function(i, row_ind) {
#   pd <- game_all[i, 5] - game_all[i, 7]
#   win_name <- ifelse(pd > 0, game_all[i, 4], game_all[i, 6])
#   lose_name <- ifelse(pd < 0, game_all[i, 4], game_all[i, 6])
#   R_w <- allgames[row_ind, which(win_name==col_name)]
#   R_l <- allgames[row_ind, which(lose_name==col_name)]
#   m <- log(abs(pd)) *2.2/((0.001*(R_w-R_l))+2.2)
#   return(m)
# }

M <- function(i, row_ind, col_ind_a, col_ind_b) {
  pd <- game_all[i, 5] - game_all[i, 7]
  mov <- abs(pd) # margin of victory
  home_win <- ifelse(pd < 0, 1, 0)
  rank_A <- allgames[row_ind-1, col_ind_a]
  rank_B <- allgames[row_ind-1, col_ind_b]
  point_A <- game_all[i, 5]
  point_B <- game_all[i, 7]
  won_underdog <- ifelse((rank_A-rank_B)*(point_A-point_B)>0, 1, -1)
  elo_diff <- (abs(rank_A-rank_B) + 100*home_win)*won_underdog
  m <- (mov+3)^0.8/(7.5+0.006*elo_diff)
  return(m)
}

elo_rate_A <- function(i, row_ind, col_ind_a, col_ind_b) {
  R_O_i <- allgames[row_ind-1, col_ind_a]
  K = 20
  m <- M(i, row_ind, col_ind_a, col_ind_b)
  S_i_j <- ifelse(game_all[i, 5] < game_all[i, 7], 0, 1)
  R_O_j <- allgames[row_ind-1, col_ind_b]
  miu_i_j <- 1/(1+10^((R_O_i - R_O_j)/400))
  elo <- R_O_i + K *m*(S_i_j - miu_i_j)
  return(elo)
}

elo_rate_B <- function(i, row_ind, col_ind_a, col_ind_b) {
  R_O_i <- allgames[row_ind-1, col_ind_b]
  K = 20
  m <- M(i, row_ind, col_ind_a, col_ind_b)
  S_i_j <- ifelse(game_all[i, 7] < game_all[i, 5], 0, 1)
  R_O_j <- allgames[row_ind-1, col_ind_a]
  miu_i_j <- 1/(1+10^((R_O_i - R_O_j)/400))
  elo <- R_O_i + K *m*(S_i_j - miu_i_j)
  return(elo)
}

#----------------------------------------------------------------------
for (i in 1:nrow(game_all)) {
  # tian shangyihangnde jinlai
  TA_name = game_all[i,4]
  TB_name = game_all[i,6]
  tmp = game_all[i,2]
  TA_cindex = which(col_name==TA_name) # team A's column index in allgames
  TB_cindex = which(col_name==TB_name) # team B's column index in allgames
  game_index_old = game_index
  game_index = which(tmp == allgames[,1]) # row index in allgames that matches game date
  if (game_index - game_index_old != 0){
    allgames[game_index, -1] <- allgames[game_index-1, -1]
  }
  # tian liangge keng
  # do something
  allgames[game_index, TA_cindex] <- elo_rate_A(i, row_ind = game_index,
                                                col_ind_a = TA_cindex, 
                                                col_ind_b = TB_cindex)
  allgames[game_index, TB_cindex] <- elo_rate_B(i, row_ind = game_index,
                                                col_ind_a = TA_cindex, 
                                                col_ind_b = TB_cindex)
}

elo_2017 <- allgames
write.csv(elo_2016, file = "../data/elo_2016.csv")
write.csv(elo_2017, file = "../data/elo_2017.csv")



  
  


