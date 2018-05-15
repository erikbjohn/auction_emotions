data_analysis <- function(){
  library(data.table)
  library(stringr)
  dt <- fread('~/Dropbox/pkg.data/auction_emotions/TickDataFull.csv')
  
  # First player
  dt_part <- dt[participant_id==1]
  setkey(dt_part, event_id, tick_id)
}
