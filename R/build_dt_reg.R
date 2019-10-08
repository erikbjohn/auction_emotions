build_dt_reg <- function(dt=NULL){
  # Converts the full, raw dataset into regression usable format
  source('R/build_events_emotions_payoffs.R')
  dt_reg_base_location <- '~/Dropbox/pkg.data/auction_emotions/Clean/dt_reg_base.rds'
  dt_seconds_location <- '~/Dropbox/pkg.data/auction_emotions/Clean/dt_seconds.rds' # Data aggregated at the average emotion by second
  dt_scores_location <- '~/Dropbox/pkg.data/auction_emotions/Clean/dt_scores.rds'
  figures_location <- '~/Dropbox/pkg.data/auction_emotions/Figures_regression/'
  l_reg <- list()
  if(!file.exists(dt_reg_location)){
      if(!(file.exists(dt_seconds_location))){
        dt <- build_events_emotions_payoffs()
        dt_seconds <- dt[, .(Score_num=mean(Score_num,na.rm = TRUE)),
                         by=c('participant_id', 'marker_sec_elapsed', 'Session', 'Participant', 'AuctionType', 'AuctionNumber', 'MarkerType', 'EmotionType',
                              'AuctionTypeOrder', 'Group',  'TimeToBid', 'Winner', 'Value', 'BidActual', 'BidNash', 'Price', 'Profit')]
        dt_seconds <- dt_seconds[is.na(Score_num), na_score_num:=1][!is.na(Score_num), na_score_num:=0]
        
        dt_seconds$InTheMoney <- 0
        dt_seconds <- dt_seconds[Value>=Price, InTheMoney := 1]
        dt_seconds$Result <- 'Winner and In the Money'
        dt_seconds <- dt_seconds[Winner==1 & InTheMoney==0, Result:='Winner and Not In the Money']
        dt_seconds <- dt_seconds[Winner==0 & InTheMoney==0, Result:='Loser and Not In the Money']
        dt_seconds <- dt_seconds[Winner==0 & InTheMoney==1, Result:='Loser and In the Money']

        saveRDS(dt_seconds, dt_seconds_location)
        # Create plots to check consistency of NA results
        dt_combs <- as.data.table(expand.grid(unique(dt_seconds$AuctionType), unique(dt_seconds$MarkerType), stringsAsFactors = FALSE))
        setnames(dt_combs, c('AuctionType', 'MarkerType'))
        for(iComb in 1:nrow(dt_combs)){
          s_AuctionType <- dt_combs[iComb]$AuctionType
          s_MarkerType <- dt_combs[iComb]$MarkerType
          dt_plot <- dt_seconds[EmotionType=='Neutral' & AuctionType==s_AuctionType & MarkerType==s_MarkerType]
          dt_plot <- dt_plot[, n_row_plot := .N, by = marker_sec_elapsed][,n_na := sum(na_score_num), by=marker_sec_elapsed]
          dt_plot <- dt_plot[, .SD[1], by=marker_sec_elapsed]
          dt_plot <- dt_plot[, .(share_na=n_na/n_row_plot, n_row_plot, n_na), by=(marker_sec_elapsed)]
          setkey(dt_plot, marker_sec_elapsed)
          dt_plot <- dt_plot[, n_total := sum(n_row_plot)]
          dt_plot <- dt_plot[, n_cum_sum := cumsum(n_row_plot)]
          dt_plot <- dt_plot[, cum_share := n_cum_sum/n_total]
          dt_plot <- dt_plot[cum_share < 0.95]
          dt_plot <- dt_plot[, share_na:=n_na/n_row_plot]
          f_name <- paste0('~/Dropbox/pkg.data/auction_emotions/no_score_timing_figures/', s_AuctionType, '_', s_MarkerType, '_timing.jpg')
          ggplot2::ggplot(dt_plot, aes(marker_sec_elapsed, share_na)) +
            geom_line() +
            ylim(0, 1) +
            ggtitle(paste(' AuctionType:', s_AuctionType, '\n', 'MarkerType:', s_MarkerType, '\n', 'First seconds containing 95% of all observations'))
          ggsave(f_name)
        }
        
        # Now, for first price and dutch auctions, look at missing by endowment value
       # dt_combs <- dt_combs[MarkerType=='auction']
        for(iComb in 1:nrow(dt_combs)){
          s_AuctionType <- dt_combs[iComb]$AuctionType
          s_MarkerType <- dt_combs[iComb]$MarkerType
          dt_plot <- dt_seconds[EmotionType=='Neutral' & AuctionType==s_AuctionType & MarkerType==s_MarkerType]
          dt_plot <- dt_plot[, n_row_value := .N, by = Value][,n_row_na_value := sum(na_score_num), by=Value]
          dt_plot <- dt_plot[, share_na_value := n_row_na_value/n_row_value]
          dt_plot <- unique(dt_plot[, .(Value, share_na_value)])
          setkey(dt_plot,Value)
          f_name <- paste0('~/Dropbox/pkg.data/auction_emotions/no_score_value_figures/', s_AuctionType, '_', s_MarkerType, '_values.jpg')
          ggplot2::ggplot(dt_plot, aes(Value, share_na_value)) +
            geom_line() +
            ylim(0, 1) +
            ggtitle(paste(' AuctionType:', s_AuctionType, '\n', 'MarkerType:', s_MarkerType, '\n'))
          ggsave(f_name)
        }
       
      } else {
        dt_scores <- readRDS(dt_scores_location)
        dt_seconds <- readRDS(dt_seconds_location)
      }
      
    table(dt_seconds[, .(AuctionType, MarkerType)])
    # Clean up TimeToBid for Sessions 5, 6, 7, 8
    dt_seconds <- dt_seconds[AuctionType=='first_price' & Session %in% c(5, 6, 7, 8), TimeToBid:=TimeToBid+30]
    
    table(dt_seconds[AuctionType=='first_price', .(TimeToBid), by=Session])
    table(dt_seconds[AuctionType=='dutch', .(TimeToBid), by=Session])
    dt_time_to_bid <- dt_seconds[AuctionType=='first_price' & EmotionType == 'Neutral']
    dt_time_to_bid <- dt_time_to_bid[, .SD[1], by=c('participant_id', 'AuctionNumber')]
    
    ggplot(dt_time_to_bid[TimeToBid <100], aes(x=TimeToBid, fill=AuctionTypeOrder)) +
      geom_histogram(position='dodge2') +      theme_bw()
    
    dt_seconds <- dt_seconds[, marker_sec_elapsed_max := max(marker_sec_elapsed),
                             by=.(participant_id, MarkerType, AuctionType, AuctionNumber)]
    
    # First 2 seconds 
    dt_first_two <- dt_seconds[MarkerType=='auction' & marker_sec_elapsed <= 2 & !is.na(Score_num)]
    
    # Create t-test
    emotions <- unique(dt_first_two$EmotionType)
    l <- list()
    for(iEmotion in 1:length(emotions)){
      sEmotion <- emotions[iEmotion]
      t_test <- t.test(dt_first_two[AuctionType=='first_price' & EmotionType %in% sEmotion]$Score_num,
                       dt_first_two[AuctionType=='dutch' & EmotionType %in% sEmotion]$Score_num, var.equal = FALSE)
      dt_ttest <- data.table(emotion = sEmotion,
                             mean_first_price = t_test$estimate[1],
                             mean_dutch = t_test$estimate[2],
                             t_stat = t_test$statistic,
                             p_value = t_test$p.value)
      l[[iEmotion]] <- dt_ttest
    }
    dt_emotion_first_two_seconds <- data.table::rbindlist(l, use.names=TRUE, fill=TRUE)
    saveRDS(dt_emotion, '~/Dropbox/pkg.data/auction_emotions/Clean/dt_first_two_seconds.rds')
    
    # Feedback (results 2 seconds or less out)
    dt_feedback_dutch <- dt_seconds[AuctionType=='dutch' & MarkerType=='transition' & marker_sec_elapsed <= 2 & !is.na(Score_num)]
    dt_feedback_first_price <- dt_seconds[AuctionType=='first_price' & MarkerType=='info' & marker_sec_elapsed <=2 & !is.na(Score_num)]
    emotions <- unique(dt_first_two$EmotionType)
    l <- list()
    for(iEmotion in 1:length(emotions)){
      sEmotion <- emotions[iEmotion]
      t_test <- t.test(dt_feedback_first_price[EmotionType %in% sEmotion]$Score_num,
                       dt_feedback_dutch[EmotionType %in% sEmotion]$Score_num, var.equal = FALSE)
      dt_ttest <- data.table(emotion = sEmotion,
                             mean_first_price = t_test$estimate[1],
                             mean_dutch = t_test$estimate[2],
                             t_stat = t_test$statistic,
                             p_value = t_test$p.value)
      l[[iEmotion]] <- dt_ttest
    }
    dt_emotion_two_seconds_results <- data.table::rbindlist(l, use.names=TRUE, fill=TRUE)
    saveRDS(dt_emotion_two_seconds_results, '~/Dropbox/pkg.data/auction_emotions/Clean/dt_emotion_two_seconds_results.rds')  
    fwrite(dt_emotion_two_seconds_results, '~/Dropbox/pkg.data/auction_emotions/Clean/dt_emotion_two_seconds_results.csv')
    
    
    # Auction Phase
    dt_auction_dutch <- dt_seconds[AuctionType=='dutch' & MarkerType=='auction' & !is.na(Score_num)]
    dt_auction_first_price <- dt_seconds[AuctionType=='first_price' & MarkerType=='info' & !is.na(Score_num)]
    emotions <- unique(dt_first_two$EmotionType)
    l <- list()
    for(iEmotion in 1:length(emotions)){
      sEmotion <- emotions[iEmotion]
      t_test <- t.test(dt_auction_first_price[EmotionType %in% sEmotion]$Score_num,
                       dt_auction_dutch[EmotionType %in% sEmotion]$Score_num, var.equal = FALSE)
      dt_ttest <- data.table(emotion = sEmotion,
                             mean_first_price = t_test$estimate[1],
                             mean_dutch = t_test$estimate[2],
                             t_stat = t_test$statistic,
                             p_value = t_test$p.value)
      l[[iEmotion]] <- dt_ttest
    }
    dt_emotion_auction <- data.table::rbindlist(l, use.names=TRUE, fill=TRUE)
    saveRDS(dt_emotion_auction, '~/Dropbox/pkg.data/auction_emotions/Clean/dt_emotion_auction_results.rds')  
    fwrite(dt_emotion_auction, '~/Dropbox/pkg.data/auction_emotions/Clean/dt_emotion_auction_results.csv')
    
    
    
    
      # Time compression to quarter second intervals
      time_start <- 0
      time_end <- 40
      time_window <- 0.25
      dt_times <- data.table::data.table(begin=seq(0,(time_end-time_window), time_window),
                                         end = seq(time_window, time_end, time_window))
      
      # Regression for first two seconds after value endowment
      dt_open <- dt_scores[marker_time_elapsed <= 2]
      emotions <- unique(dt_open$EmotionType)
      l_reg <- vector(mode='list', length=length(emotions))
      iter <- 0
      for(emotion in emotions){    
        iter <- iter + 1
        dt_emotion <- dt_open[EmotionType %in% emotion]
        dt_emotion <- dt_emotion[, dutch:=0]
        dt_emotion <- dt_emotion[AuctionType==emotion, dutch:=1]
        dt_emotion <- dt_emotion[, value_x_dutch := Value*dutch]
        l_reg[[iter]]$emotion <- emotion
        l_reg[[iter]]$lm <- lm(Score_num ~ Value + dutch + value_x_dutch + marker_time_elapsed + participant_id, data=dt_emotion)
        summary(lm_angry)
      }
      
      
      # Measure emotional response to value draw
      sMarkerType <- 'auction' 
      dt_marker <- dt_scores[MarkerType==sMarkerType]
      
      l_window_results <- vector('list', length=nrow(dt_marker))
      
      for(iTime in 1:nrow(dt_times)){
        cat(iTime, ' of ', nrow(dt_times), ' windows ', '\n')
        
        # Quarter second response
        window_begin <- dt_times[iTime]$begin
        window_end <- dt_times[iTime]$end
        dt_window <- dt_marker[marker_time_elapsed >= window_begin &
                                 marker_time_elapsed <= window_end]
        dt_window <- dt_window[, avg_emotion:=mean(Score_num), by=c('Session','Participant',
                                                                    'AuctionType', 'AuctionNumber',
                                                                    'EmotionType')]
        dt_window <- unique(dt_window[, .(participant_id, Session, Participant, AuctionType, AuctionNumber, 
                                                    AuctionTypeOrder, Group, Value, BidActual, BidNash, TimeToBid, 
                                                    Winner, Price, Profit, EmotionType, avg_emotion)])
        
        # Value Assignment results
        dt_window <- dt_window[, Value_low_60 := 0][Value <= 60, Value_low_60 := 1]
        dt_window <- dt_window[, Value_high_180 := 0][Value >= 180, Value_high_180 := 1]
        dt_window <- dt_window[, p_high_value:=(Value/240)^3][, p_high_value_sq:=p_high_value^2]
        
        # Within auction type estimates
        value_emotion_regressions <- function(emotion_type, auction_type, dt_fun){
          l_mod  <- list()
          l_mod$value <- lm(avg_emotion ~ Value , data = dt_fun)
          l_mod$p_high_value <- lm(avg_emotion ~ p_high_value, data = dt_fun)
          l_mod$Value_low_60 <- lm(avg_emotion ~ Value_low_60, data = dt_fun)
          l_mod$Value_high_180 <- lm(avg_emotion ~ Value_high_180 , data = dt_fun)
          return(l_mod)
        }
        
        # Start Analysis
        emotions <- unique(dt_window$EmotionType)
        auction_types <- unique(dt_window$AuctionType)
        dt_args <- as.data.table(expand.grid(emotions, auction_types, stringsAsFactors=FALSE))
        setnames(dt_args, names(dt_args), c('emotion_type', 'auction_type'))
        l_value_emotion_regression <- vector('list', length=nrow(dt_args))
        
        # Iterate over auction type and emotion in dt_args
        for(iArg in 1:nrow(dt_args)){
          dt_reg_sub <- data.table::data.table(auction_type = as.character(),
                                               emotion_type = as.character(),
                                               window_begin = as.numeric(),
                                               window_end = as.numeric(),
                                               beta = as.numeric(), std_error = as.numeric())
          emotion_type <- dt_args[iArg]$emotion_type
          auction_type <- dt_args[iArg]$auction_type
          dt_fun <- dt_window[AuctionType == auction_type & EmotionType == emotion_type]
          l_mods <- value_emotion_regressions(emotion_type, auction_type, dt_fun)
          
          # Extract data from regression
          rows_count <- which(sapply(l_mods, function(x) nrow(coef(summary(x))))==2)
          l_mods <- l_mods[rows_count]
          l_mods_extract <- vector('list', length=length(l_mods))
          for(iMod in 1:length(l_mods)){
            mod <- l_mods[[iMod]]
            dt_mod <- data.table(auction_type = auction_type,
                                 emotion_type = emotion_type, 
                                 window_begin = window_begin,
                                 window_end = window_end,
                                 rank = mod$rank,
                                 treatment = names(mod$coefficients)[2],
                                 intercept = coef(summary(mod))[1,1],
                                 ittercept_std_error = coef(summary(mod))[1,2],
                                 beta=coef(summary(mod))[2,1],
                                 beta_std_error = coef(summary(mod))[2,2],
                                 p_val=coef(summary(mod))[2,4])
            dt_mod <- dt_mod[, predict:=intercept+beta]
            l_mods_extract[[iMod]] <- dt_mod
          }
          l_value_emotion_regression[[iArg]] <- rbindlist(l_mods_extract, use.names = TRUE, fill = TRUE)
        }
        l_window_results[[iTime]] <- rbindlist(l_value_emotion_regression, use.names=TRUE,fill=TRUE)
      }
      dt_reg_base <- rbindlist(l_window_results, use.names = TRUE, fill=TRUE)
      saveRDS(dt_reg_base, dt_reg_base_location)
    } else {
      dt_reg_base <- readRDS(dt_reg_base_location)
    }
}
   