tables_auction <- function(){
  
  dt <- dt_auction[, .(score=mean(Score_num)), by=.(EmotionType, AuctionType)]
  spread <- tidyr::spread(dt, AuctionType, score)
  setnames(spread, names(spread)[2:3], paste0(names(spread)[2:3], '_auction_segment'))
 
  write.csv(spread, file='~/Dropbox/pkg.data/auction_emotions/Clean/auction_segment_tables.csv')
}
