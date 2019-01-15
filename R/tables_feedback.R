tables_feedback <- function(){
  
  dt_in_the_money <- dt_close[winner_in_the_money == 1, .(score=mean(Score_num)), by=.(EmotionType, AuctionType)]
  spread_in_the_money <- tidyr::spread(dt_in_the_money, AuctionType, score)
  setnames(spread_in_the_money, names(spread_in_the_money)[2:3], paste0(names(spread_in_the_money)[2:3], '_winnner'))
 
  dt_loser_in_the_money <- dt_close[loser_in_the_money == 1, .(score=mean(Score_num)), by=.(EmotionType, AuctionType)]
  spread_loser_in_the_money <- tidyr::spread(dt_loser_in_the_money, AuctionType, score)
  setnames(spread_loser_in_the_money, names(spread_loser_in_the_money)[2:3], paste0(names(spread_loser_in_the_money)[2:3], '_loser_in_the_money'))
  
  dt_not_in_the_money <- dt_close[loser_in_the_money == 0 & winner_in_the_money == 0, .(score=mean(Score_num)), by=.(EmotionType, AuctionType)]
  spread_not_in_the_money <- tidyr::spread(dt_not_in_the_money, AuctionType, score)
  setnames(spread_not_in_the_money, names(spread_not_in_the_money)[2:3], paste0(names(spread_not_in_the_money)[2:3], '_not_in_the_money')) 

  setkey(spread_in_the_money, EmotionType)
  setkey(spread_loser_in_the_money, EmotionType)  
  setkey(spread_not_in_the_money, EmotionType)
  
  dt <- spread_in_the_money[spread_loser_in_the_money]
  dt <- spread_not_in_the_money[dt]
  
  write.csv(dt, file='~/Dropbox/pkg.data/auction_emotions/Clean/feedback_info_Transition_tables.csv')
}
