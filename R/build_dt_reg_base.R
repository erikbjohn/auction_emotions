build_dt_reg_base <- function(dt=NULL){
  # Converts the full, raw dataset into regression usable format
  source('R/build_events_emotions_payoffs.R')
  dt_reg_base_location <- '~/Dropbox/pkg.data/auction_emotions/Clean/dt_reg_base.rds'
  dt_scores_location <- '~/Dropbox/pkg.data/auction_emotions/Clean/dt_scores.rds'
  figures_location <- '~/Dropbox/pkg.data/auction_emotions/Figures_regression/'
  l_reg <- list()
  if(!file.exists(dt_reg_location)){
      if(!(file.exists(dt_scores_location))){
        if(is.null(dt)){
        dt <- build_events_emotions_payoffs()
        }
        dt <- dt[, marker_start:=min(snap_start), by=c('Session', 'Participant', 'AuctionType', 'AuctionNumber', 'MarkerType')]
        dt <- dt[, marker_time_elapsed := snap_start-marker_start]
        dt_scores <- dt[!is.na(Score_num)]
        dt_scores <- dt_scores[, participant_id := paste0(Session, '_', Participant)]
        saveRDS(dt_scores, dt_scores_location)
      } else {
        dt_scores <- readRDS(dt_scores_location)
      }
      # Time compression to quarter second intervals
      time_start <- 0
      time_end <- 40
      time_window <- 0.25
      dt_times <- data.table::data.table(begin=seq(0,(time_end-time_window), time_window),
                                         end = seq(time_window, time_end, time_window))
      
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
   