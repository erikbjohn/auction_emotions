reg_open_5 <- function(){
  # For conference (Cary presenting 1/14/2019)
  library(data.table)
  library(car)
  options(scipen=999)
  dt_scores_location <- '~/Dropbox/pkg.data/auction_emotions/Clean/dt_scores.rds'
  reg_open_5_location <- '~/Dropbox/pkg.data/auction_emotions/Clean/reg_open_5.rds'
  
  if(!file.exists(reg_open_5_location)){
    dt_scores <- readRDS(dt_scores_location)
    # Time compression to quarter second intervals
    # Regression for first two seconds after value endowment
    dt_open <- dt_scores[marker_time_elapsed <= 5 & MarkerType=='auction']
    
    emotions <- unique(dt_open$EmotionType)
    iter <- 0
    dt_emotions_reg <- data.table()
    for(emotion in emotions){    
      iter <- iter + 1
      cat(emotion, '\n')
      dt_emotion <- dt_open[EmotionType %in% emotion]
      dt_emotion <- dt_emotion[, dutch:=0]
      dt_emotion <- dt_emotion[AuctionType=='dutch', dutch:=1]
      dt_emotion <- dt_emotion[, value_x_dutch := Value*dutch]
      lm_mod <- lm(Score_num ~ Value + dutch + value_x_dutch + marker_time_elapsed + participant_id,
                             data=dt_emotion)
      
      # Test for joint significance of value and value x dutch
      f_test <- linearHypothesis(lm_mod, c('Value = - value_x_dutch'))
      f_stat <- f_test$F[2]
      p_val_f <- f_test$`Pr(>F)`[2]
      
      sum_lm_mod <- summary(lm_mod)
      mat_summary <- sum_lm_mod$coefficients[2:4,c(1,2)]
      sum_names <- rownames(mat_summary)
      dt_emotion_reg <- data.table(coeff = sum_names, mat_summary)
      dt_emotion_reg <- as.data.table(tidyr::gather(dt_emotion_reg, param, value, -coeff))
      setkey(dt_emotion_reg, coeff, param)  

      dt_emotion_f_test <- data.table(coeff='f_test', param='f_stat', value = f_stat)
      dt_emotion_reg <- rbindlist(list(dt_emotion_reg, dt_emotion_f_test), use.names = TRUE)
      
      coefficients_individual <- which(grepl('participant', names(lm_mod$coefficients)))
      individual_avg_effect <- sum(lm_mod$coefficients[coefficients_individual])/(length(coefficients_individual)+1)
      
      l_predict <- list()
      l_predict$dutch_v0 <- data.table(coeff='dutch_v0', param='prediction',
                                       value= individual_avg_effect + sum_lm_mod$coefficients[1] + 
                                         sum_lm_mod$coefficients[3])
      l_predict$dutch_v240 <- data.table(coeff='dutch_v240', param='prediction',
                                         value=individual_avg_effect + sum_lm_mod$coefficients[1] + 
                                           sum_lm_mod$coefficients[2]*240 + 
                                           sum_lm_mod$coefficients[3] +
                                           sum_lm_mod$coefficients[4]*240)
      l_predict$first_price_v0 <- data.table(coeff='first_price_v0', param='prediction',
                                             value = individual_avg_effect + 
                                               sum_lm_mod$coefficients[1])
      l_predict$first_price_v240 <- data.table(coeff='first_price_v240', param='prediction',
                                               value= individual_avg_effect +
                                                 sum_lm_mod$coefficients[1] +
                                                 sum_lm_mod$coefficients[2]*240)
      dt_predict <- rbindlist(l_predict, use.names=TRUE, fill=TRUE)
      
      dt_emotion_reg <- rbindlist(list(dt_emotion_reg, dt_predict), use.names=TRUE, fill=TRUE)
      
      l_diag <- list()
      l_diag$rsquared <- data.table(coeff='adj R^2', param='diagnostics', value=sum_lm_mod$adj.r.squared)
      l_diag$dof <- data.table(coeff='dof', param='diagnostics', value=sum_lm_mod$df[2])
      dt_diag <- rbindlist(l_diag, use.names=TRUE)
      dt_emotion_reg <- rbindlist(list(dt_emotion_reg, dt_diag), use.names=TRUE, fill=TRUE)      
      dt_emotion_reg$emotion <- emotion
      dt_emotion_reg$index <- 1:nrow(dt_emotion_reg)
      
      dt_emotions_reg <- rbindlist(list(dt_emotions_reg, dt_emotion_reg), use.names = TRUE, fill=TRUE)
#      setnames(dt_emotion_reg, 'value', emotion)
    
    }
    dt_spread <- data.table(tidyr::spread(dt_emotions_reg, emotion, value))
    dt_spread <- dt_spread[order(index)]
    write.csv(dt_spread, '~/Dropbox/pkg.data/auction_emotions/Clean/reg_open_5.csv')
    saveRDS(dt_spread, reg_open_5_location)
  } else {
    l_reg <- readRDS(reg_open_5_location)
  }
  
}