build_dt_reg <- function(dt=NULL){
  # Converts the full, raw dataset into regression usable format
  dt_reg_location <- '~/Dropbox/pkg.data/auction_emotions/Clean/dt_reg.rds'
  dt_scores_location <- '~/Dropbox/pkg.data/auction_emotions/Raw/dt_scores.rds'
  l_reg <- list()
  if(!file.exists(dt_reg_location)){
    if(is.null(dt)){
      if(!(file.exists(dt_scores_location))){
        dt <- build_events_emotions_payoffs()
        dt <- dt[, marker_start:=min(snap_start), by=c('Session', 'Participant', 'AuctionType', 'AuctionNumber', 'MarkerType')]
        dt <- dt[, marker_time_elapsed := snap_start-marker_start]
        dt_scores <- dt[!is.na(Score_num)]
        dt_scores <- dt_scores[, participant_id := paste0(Session, '_', Participant)]
        saveRDS(dt_scores, dt_scores_location)
      } else {
        dt_scores <- readRDS(dt_scores_location)
      }
      # Time responses
      secs <- c(1, 2, 5)
      
      for(sec in secs){
        # Two second response
        dt_shock_sec <- dt_scores[, marker_sec:=0][marker_time_elapsed < sec, marker_sec := 1]
       
        dt_shock_sec <- dt_shock_sec[marker_sec>0][, avg_emotion:=mean(Score_num), by=c('Session', 'Participant',
                                                                                             'AuctionType', 'AuctionNumber',
                                                                                             'MarkerType', 'EmotionType')]
        dt_shock_sec <- unique(dt_shock_sec[, .(participant_id, Session, Participant, AuctionType, AuctionNumber, MarkerType, 
                                                    AuctionTypeOrder, Group, Value, BidActual, BidNash, TimeToBid, 
                                                    Winner, Price, Profit, EmotionType, avg_emotion)])
        # Value Assignment results
        dt_shock_sec <- dt_shock_sec[MarkerType=='auction' & AuctionType=='dutch']
        
        # Value less or greater than 200
        dt_shock_sec <- dt_shock_sec[,Value_low_100 := 0][Value <= 100, Value_low_100 := 1]
        #dt_shock_sec <- dt_shock_sec[,Value_high_176 := 0][Value >= 176, Value_high_176 := 1]
        #dt_shock_sec <- dt_shock_sec[,Value_high_184 := 0][Value >= 184, Value_high_184 := 1]
        #dt_shock_sec <- dt_shock_sec[,Value_high_192 := 0][Value >= 192, Value_high_192 := 1]
        dt_shock_sec <- dt_shock_sec[,Value_high_200 := 0][Value >= 200, Value_high_200 := 1]
        #dt_shock_sec <- dt_shock_sec[,Value_high_208 := 0][Value >= 208, Value_high_208 := 1]
        #dt_shock_sec <- dt_shock_sec[,Value_high_216 := 0][Value >= 216,Value_high_216  := 1]
        dt_shock_sec <- dt_shock_sec[,p_high_value:=(Value/240)^3][,p_high_value_sq:=p_high_value^2]
        
        emotion_regressions <- function(dt_emotion){
          l_mod  <- list()
          l_mod$p_high_value <- lm(avg_emotion ~ p_high_value + factor(participant_id), data = dt_emotion)
          #l_mod$p_high_value_sq <- lm(avg_emotion ~ p_high_value + p_high_value_sq + factor(AuctionType) +factor(participant_id), data = dt_emotion)
          l_mod$Value_low_100 <- lm(avg_emotion ~ Value_low_100  + factor(participant_id), data = dt_emotion)
          #l_mod$Value_high_176 <- lm(avg_emotion ~ Value_high_176 + factor(AuctionType) +factor(participant_id), data = dt_emotion)
          #l_mod$Value_high_184 <- lm(avg_emotion ~ Value_high_184 + factor(AuctionType) +factor(participant_id), data = dt_emotion)
          #l_mod$Value_high_192 <- lm(avg_emotion ~ Value_high_192 + factor(AuctionType) +factor(participant_id), data = dt_emotion)
          l_mod$Value_high_200 <- lm(avg_emotion ~ Value_high_200 + factor(participant_id), data = dt_emotion)
          l_mod$NoValue <- lm(avg_emotion ~ factor(participant_id), data = dt_emotion)
          #l_mod$Value_high_208 <- lm(avg_emotion ~ Value_high_208 + factor(AuctionType) +factor(participant_id), data = dt_emotion)
          #l_mod$Value_high_216 <- lm(avg_emotion ~ Value_high_216 + factor(AuctionType) +factor(participant_id), data = dt_emotion)
          return(l_mod)
        }
        
        emotions <- unique(dt_shock_sec$EmotionType)
        l_emotion_regressions <- lapply(emotions, function(x) emotion_regressions(dt_emotion = dt_shock_sec[EmotionType==x]))
        
        for(iMod in 1:length(l_emotion_regressions)){
          l_emotion <- l_emotion_regressions[[iMod]]
          for(iEmotion in 1:length(l_emotion)){
          cat(paste(sec, 'Second Dependent Emotion:', emotions[iMod], 'Treatment:', names(l_emotion)[iEmotion]))
            cat('\n ------------------------------------ \n')
            print(summary(l_emotion[[iEmotion]])$coef[1:3,1:4])
            cat('\n ------------------------------------ \n')
            # Store information
            #dt_reg <- as.data.table(summary(l_emotion[[iEmotion]])$coef[1:3, 1:4])
            #dt_reg$coeff <- row.names(summary(l_emotion[[iEmotion]]$coef[1:3, 1:4]))
            #dt_reg$emotion <- emotions[iMod]
            #dt_reg$treatment <- names(l_emotion)[iEmotion]
            #dt_reg$seconds <- sec
          #  if(!exists('dt_regs')){
          #    dt_regs <- dt_reg 
          #  } else {
          #    dt_regs <- rbindlist(list(dt_regs, dt_reg), use.names = TRUE, fill=TRUE)
          #  }
          }
        }
      }
      
      
      # Five second response
      dt <- dt_scores[, marker_5sec:=0][marker_time_elapsed < 5, marker_5sec := 1][!(is.na(avg_emotion))]

    }
    saveRDS(dt_reg, dt_reg_location)
  } else {
    dt_reg <- readRDS(dt_reg_location)
  }
}