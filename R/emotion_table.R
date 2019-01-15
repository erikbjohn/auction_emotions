emotion_table <- function(){
  library(dplyr)
  library(data.table)
  library(tidyr)
  dt_scores_location <- '~/Dropbox/pkg.data/auction_emotions/Clean/dt_scores.rds'
  
  # scores <- readRDS(dt_scores_location)
  
  auction_stage <- scores[MarkerType == 'auction']
  
  auction_averages <- auction_stage[, .(score=mean(Score_num)) ,by=.(AuctionType, EmotionType)]
  auction_averages <- tidyr::spread(auction_averages, AuctionType, score)
  write.csv(auction_averages, '~/Dropbox/pkg.data/auction_emotions/Clean/auction_averages.csv')
  
  auction_sd <- auction_stage[, .(score_sd=sd(Score_num)) ,by=.(AuctionType, EmotionType)]
  auction_sd <- tidyr::spread(auction_sd, AuctionType, score_sd)
  write.csv(auction_sd, '~/Dropbox/pkg.data/auction_emotions/Clean/auction_sd.csv')
  
  feedback_stage <- scores[MarkerType == 'info']
  feedback_averages <- feedback_stage[, .(score=mean(Score_num)), by=.(AuctionType, EmotionType)]
  feedback_averages <- tidyr::spread(feedback_averages, AuctionType, score)
  write.csv(feedback_averages, '~/Dropbox/pkg.data/auction_emotions/Clean/feeback_averages.csv')
  
  feedback_sds <- feedback_stage[, .(score_sd=sd(Score_num)), by=.(AuctionType, EmotionType)]
  feedback_sds <- tidyr::spread(feedback_sds, AuctionType, score_sd)
  write.csv(feedback_sds, '~/Dropbox/pkg.data/auction_emotions/Clean/feeback_sds.csv')
  
  
  
}