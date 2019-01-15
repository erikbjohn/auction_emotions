reg_open_4 <- function(){
  dt_scores_location <- '~/Dropbox/pkg.data/auction_emotions/Clean/dt_scores.rds'
  reg_open_4_location <- '~/Dropbox/pkg.data/auction_emotions/Clean/reg_open_4.rds'
  
  if(!file.exists(reg_open_4_location)){
    dt_scores <- readRDS(dt_scores_location)
    # Time compression to quarter second intervals
    # Regression for first two seconds after value endowment
    dt_open <- dt_scores[marker_time_elapsed <= 4]
    
    emotions <- unique(dt_open$EmotionType)
    l_reg <- vector(mode='list', length=length(emotions))
    iter <- 0
    for(emotion in emotions){    
      iter <- iter + 1
      cat(emotion, '\n')
      dt_emotion <- dt_open[EmotionType %in% emotion]
      dt_emotion <- dt_emotion[, dutch:=0]
      dt_emotion <- dt_emotion[AuctionType=='dutch', dutch:=1]
      dt_emotion <- dt_emotion[, value_x_dutch := Value*dutch]
      l_reg[[iter]]$emotion <- emotion
      l_reg[[iter]]$lm <- lm(Score_num ~ Value + dutch + value_x_dutch + marker_time_elapsed + participant_id,
                             data=dt_emotion, qr=FALSE, model=FALSE)
    }
    saveRDS(l_reg, reg_open_4_location)
  } else {
    l_reg <- readRDS(reg_open_4_location)
  }
  
}