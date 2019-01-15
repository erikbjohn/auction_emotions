reg_feedback_info_only <- function(){
  # For conference (Cary presenting 1/14/2019)
  library(data.table)
  library(car)
  options(scipen=999)
  dt_scores_location <- '~/Dropbox/pkg.data/auction_emotions/Clean/dt_scores.rds'
  reg_feedback_location <- '~/Dropbox/pkg.data/auction_emotions/Clean/reg_feedback_info_only.rds'
  
  if(!file.exists(reg_feedback_location)){
    dt_scores <- readRDS(dt_scores_location)
    # Correct to show if dutch 'in the money'
    # dutch_exam <- dt_scores[Price == -77777]
    price_winning <- unique(dt_scores[AuctionType=='dutch' & Winner==1, .(Session, AuctionType, AuctionNumber, Group, dutch_price_winning=Price)])
    setkey(price_winning, Session, AuctionType, AuctionNumber, Group)
    setkey(dt_scores, Session, AuctionType, AuctionNumber, Group)
    dt_scores <- price_winning[dt_scores]
    dt_scores <- dt_scores[, dutch_money := Value - dutch_price_winning]
    dt_scores <- dt_scores[, first_price_money := Value - Price]
    
    dt_scores$winner_in_the_money <- 0
    dt_scores$loser_in_the_money <- 0
    dt_scores <- dt_scores[AuctionType=='dutch' &  Winner == 1,winner_in_the_money := 1]
    dt_scores <- dt_scores[AuctionType=='dutch' &  Winner == 0 & dutch_money > 0, loser_in_the_money := 1]
    dt_scores <- dt_scores[AuctionType=='first_price' & Winner ==1, winner_in_the_money := 1]
    dt_scores <- dt_scores[AuctionType=='first_price' & Winner ==0 & first_price_money > 0, loser_in_the_money:=1]
    
    # Time compression to quarter second intervals
    # Regression for first two seconds after value endowment
    dt_close <- dt_scores[MarkerType %in% c('transition', 'info')]
    saveRDS(dt_close, file='~/Dropbox/pkg.data/auction_emotions/Raw/dt_close.rds')
    dt_close <- dt_close[MarkerType %in% 'info']
    rm(dt_scores)
    #rm(dt_scores)
    emotions <- unique(dt_close$EmotionType)
    iter <- 0
    if(!(file.exists('~/Dropbox/pkg.data/auction_emotions/Raw/reg_feedback_info_only.rds'))){
    dt_emotions_reg <- data.table()
    } else {
      dt_emotions_reg <- readRDS('~/Dropbox/pkg.data/auction_emotions/Raw/reg_feedback_info_only.rds')
      emotions_done <- unique(dt_emotions_reg$emotion)
      emotions <- emotions[!(emotions %in% emotions_done)]
    }
    for(emotion in emotions){    
      iter <- iter + 1
      cat(emotion, '\n')
      dt_emotion <- dt_close[EmotionType %in% emotion]
      dt_emotion <- dt_emotion[, dutch:=0]
      dt_emotion <- dt_emotion[AuctionType=='dutch', dutch:=1]
      dt_emotion <- dt_emotion[, winner_x_dutch := Winner*dutch]
      dt_emotion <- dt_emotion[, loser_itm_x_dutch := loser_in_the_money*dutch]
      lm_mod <- lm(Score_num ~ dutch + winner_x_dutch + loser_itm_x_dutch +  
                     winner_in_the_money + loser_in_the_money +
                     participant_id,
                             data=dt_emotion)
      Averages (Info Only)						
      First Price			Dutch		
      EmotionType	not_in_the_money	loser_in_the_money	winnner	not_in_the_money	loser_in_the_money	winnner
      1	Angry	0.2195	0.2088	0.2139	0.2085	0.2251	0.2125
      2	Arousal	0.2834	0.2872	0.2888	0.2884	0.2965	0.2920
      3	Contempt	0.0869	0.1005	0.0986	0.0941	0.0918	0.0914
      4	Disgusted	0.0742	0.0550	0.0624	0.0634	0.0708	0.0531
      5	Happy	0.0436	0.0463	0.0419	0.0351	0.0476	0.0366
      6	Neutral	0.3795	0.4047	0.4031	0.4197	0.4005	0.4251
      7	Sad	0.0545	0.0677	0.0549	0.0514	0.0470	0.0551
      8	Scared	0.0143	0.0088	0.0143	0.0131	0.0087	0.0119
      9	Surprised	0.1324	0.1317	0.1219	0.1147	0.1058	0.1138
      10	Valence	-0.2800	-0.2589	-0.2667	-0.2652	-0.2701	-0.2616
      
      # Test for joint significance of value and value x dutch
      f_test <- linearHypothesis(lm_mod, c('winner_in_the_money = - loser_in_the_money'))
      f_stat <- f_test$F[2]
      p_val_f <- f_test$`Pr(>F)`[2]
      
      sum_lm_mod <- summary(lm_mod)
      mat_summary <- sum_lm_mod$coefficients[2:6,c(1,2)]
      sum_names <- rownames(mat_summary)
      dt_emotion_reg <- data.table(coeff = sum_names, mat_summary)
      dt_emotion_reg <- as.data.table(tidyr::gather(dt_emotion_reg, param, value, -coeff))
      setkey(dt_emotion_reg, coeff, param)  

      dt_emotion_f_test <- data.table(coeff='f_test', param='f_stat', value = f_stat)
      dt_emotion_reg <- rbindlist(list(dt_emotion_reg, dt_emotion_f_test), use.names = TRUE)
      
      #coefficients_individual <- which(grepl('participant', names(lm_mod$coefficients)))
      #individual_avg_effect <- sum(lm_mod$coefficients[coefficients_individual])/(length(coefficients_individual)+1)
      
      #l_predict <- list()
      #l_predict$dutch_v0 <- data.table(coeff='dutch_v0', param='prediction',
      #                                 value= individual_avg_effect + sum_lm_mod$coefficients[1] + 
      #                                   sum_lm_mod$coefficients[3])
      #l_predict$dutch_v240 <- data.table(coeff='dutch_v240', param='prediction',
      #                                   value=individual_avg_effect + sum_lm_mod$coefficients[1] + 
      #                                     sum_lm_mod$coefficients[2]*240 + 
      #                                     sum_lm_mod$coefficients[3] +
      #                                     sum_lm_mod$coefficients[4]*240)
      #l_predict$first_price_v0 <- data.table(coeff='first_price_v0', param='prediction',
      #                                       value = individual_avg_effect + 
      #                                         sum_lm_mod$coefficients[1])
      #l_predict$first_price_v240 <- data.table(coeff='first_price_v240', param='prediction',
      #                                         value= individual_avg_effect +
      #                                           sum_lm_mod$coefficients[1] +
      #                                           sum_lm_mod$coefficients[2]*240)
      #dt_predict <- rbindlist(l_predict, use.names=TRUE, fill=TRUE)
      
      #dt_emotion_reg <- rbindlist(list(dt_emotion_reg, dt_predict), use.names=TRUE, fill=TRUE)
      
      l_diag <- list()
      l_diag$rsquared <- data.table(coeff='adj R^2', param='diagnostics', value=sum_lm_mod$adj.r.squared)
      l_diag$dof <- data.table(coeff='dof', param='diagnostics', value=sum_lm_mod$df[2])
      dt_diag <- rbindlist(l_diag, use.names=TRUE)
      dt_emotion_reg <- rbindlist(list(dt_emotion_reg, dt_diag), use.names=TRUE, fill=TRUE)      
      dt_emotion_reg$emotion <- emotion
      dt_emotion_reg$index <- 1:nrow(dt_emotion_reg)
      
      dt_emotions_reg <- rbindlist(list(dt_emotions_reg, dt_emotion_reg), use.names = TRUE, fill=TRUE)
#      setnames(dt_emotion_reg, 'value', emotion) 
      saveRDS(dt_emotions_reg, '~/Dropbox/pkg.data/auction_emotions/Raw/reg_feedback+info_only.rds')
      rm(dt_emotion)
      rm(dt_emotion_reg)
      rm(lm_mod)
    }
    dt_spread <- data.table(tidyr::spread(dt_emotions_reg, emotion, value))
    dt_spread <- dt_spread[order(index)]
    write.csv(dt_spread, '~/Dropbox/pkg.data/auction_emotions/Clean/reg_feedback_info_only.csv')
    saveRDS(dt_spread, reg_feedback_location)
  } else {
    l_reg <- readRDS(reg_feedback_location)
  }
  
}