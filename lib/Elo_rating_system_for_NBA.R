BOS <- read.csv("../data/PerGame_2015/BOS_2015.csv")
colnames(BOS)
BOS[1,7]

elo_rating <- function(df, init_rank = 1500) {
  S <- ifelse(df[, 6]=="W", 1, 0)
  K <- 20 
  rank_new <- NA
  for (i in 1:nrow(df)) {
    # margin of victory multiplier
    # mv_mult = LN(ABS(PD)+1) * (2.2/((ELOW-ELOL)*.001+2.2))
    pd <- abs(df[i, 7] - df[i, 8])
    old.rank.w <- rank_new
    mv_mult <- log(pd +1) * (2.2/((old.rank.w - old.rank.l)*.001+2.2))
    
    rank_new[i] <- rank_new[i-1]
  }
}
