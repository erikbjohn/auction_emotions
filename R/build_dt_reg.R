build_dt_reg <- function(dt=NULL){
  # Converts the full, raw dataset into regression usable format
  dt_reg_location <- '~/Dropbox/pkg.data/auction_emotions/Clean/dt_reg.rds'
  dt_scores_location <- '~/Dropbox/pkg.data/auction_emotions/Clean/dt_scores.rds'
  figures_location <- '~/Dropbox/pkg.data/auction_emotions/Figures_regression/'
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
      # Time dilation
      time_start <- 0
      time_end <- 20
      time_window <- 0.25
      dt_times <- data.table::data.table(begin=seq(0,(time_end-time_window), time_window),
                                         end = seq(time_window, time_end, time_window))
      sMarkerType <- 'auction' # Measure emotional response to value draw
      dt_marker <- dt_scores[MarkerType==sMarkerType]
      
      l_window_results <- vector('list', length=nrow(dt_marker))
      
      for(iTime in 1:nrow(dt_times)){
        cat(iTime, ' of ', nrow(dt_times), ' windows ', '\n')
        # Two second response
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
        dt_window <- dt_window[,p_high_value:=(Value/240)^3][,p_high_value_sq:=p_high_value^2]
        
        # Within auction type estimates
        value_emotion_regressions <- function(emotion_type, auction_type, dt_fun){
          l_mod  <- list()
          l_mod$value <- lm(avg_emotion ~ Value + factor(participant_id), data = dt_fun)
          l_mod$p_high_value <- lm(avg_emotion ~ p_high_value + factor(participant_id), data = dt_fun)
          l_mod$Value_low_60 <- lm(avg_emotion ~ Value_low_60  + factor(participant_id), data = dt_fun)
          l_mod$Value_high_180 <- lm(avg_emotion ~ Value_high_180 + factor(participant_id), data = dt_fun)
          return(l_mod)
        }
        
        emotions <- unique(dt_window$EmotionType)
        auction_types <- unique(dt_window$AuctionType)
        
        dt_args <- as.data.table(expand.grid(emotions, auction_types, stringsAsFactors=FALSE))
        setnames(dt_args, names(dt_args), c('emotion_type', 'auction_type'))
        l_value_emotion_regression <- vector('list', length=nrow(dt_args))
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
          l_mods_extract <- vector('list', length=length(l_mods))
          for(iMod in 1:length(l_mods)){
            mod <- l_mods[[iMod]]
            dt_mod <- data.table(auction_type = auction_type,
                                 emotion_type = emotion_type, 
                                 window_begin = window_begin,
                                 window_end = window_end,
                                 rank = mod$rank,
                                 treatment = names(mod$coefficients)[2],
                                 intercept = coef(summary(mod))[2,3],
                                 beta=coef(summary(mod))[2,1],
                                 std_error=2*coef(summary(mod))[2,2],
                                 p_val=coef(summary(mod))[2,4])
            dt_mod <- dt_mod[, predict:=intercept+beta]
            l_mods_extract[[iMod]] <- dt_mod
          }
          l_value_emotion_regression[[iArg]] <- rbindlist(l_mods_extract, use.names = TRUE, fill = TRUE)
        }
        l_window_results[[iTime]] <- rbindlist(l_value_emotion_regression, use.names=TRUE,fill=TRUE)
      }
      dt_reg <- rbindlist(l_window_results, use.names = TRUE, fill=TRUE)
      saveRDS(dt_reg, dt_reg_location)
    } else {
      dt_reg <- readRDS(dt_reg_location)
    }
    # Next produce plots
    emotions <- unique(dt_reg$emotion_type)
    treatments <- unique(dt_reg$treatment)
    dt_plots <- as.data.table(expand.grid(emotions, treatments, stringsAsFactors = FALSE))
    setnames(dt_plots, names(dt_plots), c('emotion', 'treatment'))
    
    dt_reg <- dt_reg[window_end < 15]
    
    # Marignal effect plots
    for(iPlot in 1:nrow(dt_plots)){
      emotion <- dt_plots[iPlot]$emotion
      sTreatment <- dt_plots[iPlot]$treatment
      dt_plot <- dt_reg[emotion_type == emotion & treatment == sTreatment]
      dt_plot <- dt_plot[, conf_low := beta-1.96*std_error]
      dt_plot <- dt_plot[, conf_high := beta+1.96*std_error]
      p <- ggplot() + 
        geom_ribbon(data=dt_plot, aes(x=window_end, ymin=conf_low, ymax=conf_high, group=auction_type, fill=auction_type), alpha = 0.3)+
        geom_line(data=dt_plot, aes(x=window_end, y=beta, group=auction_type, colour=auction_type)) +
        #geom_line(data=dt_plot, aes(x=window_end, y=conf_low, group=auction_type, colour=auction_type)) +
        #geom_line(data=dt_plot, aes(x=window_end, y=conf_high, group=auction_type, colour=auction_type)) +
        scale_color_manual(values=c("#CC6666", "#999999")) + 
        scale_fill_manual(values=c("#CC6666", "#999999")) +
        geom_hline(yintercept = 0)
      fName <- paste0(figures_location, 'Emotion-', emotion, '_Treatment-', stringr::str_replace_all(sTreatment, '\\_', ''), '_Window-', time_window,'sec')
      ggsave(filename=fName, device='pdf')
    }
      
    # prediction plots
    dt_pred_plots <- dt_plots[treatment=='Value_high_180']
    for(iPlot in 1:nrow(dt_pred_plots)){
      emotion <- dt_pred_plots[iPlot]$emotion
      sTreatment <- dt_pred_plots[iPlot]$treatment
      # Start working right here
      dt_plot <- dt_reg[emotion_type == emotion & treatment == sTreatment]
      dt_plot <- dt_plot[, conf_low := beta-1.96*std_error]
      dt_plot <- dt_plot[, conf_high := beta+1.96*std_error]
      p <- ggplot() + 
        geom_ribbon(data=dt_plot, aes(x=window_end, ymin=conf_low, ymax=conf_high, group=auction_type, fill=auction_type), alpha = 0.3)+
        geom_line(data=dt_plot, aes(x=window_end, y=beta, group=auction_type, colour=auction_type)) +
        #geom_line(data=dt_plot, aes(x=window_end, y=conf_low, group=auction_type, colour=auction_type)) +
        #geom_line(data=dt_plot, aes(x=window_end, y=conf_high, group=auction_type, colour=auction_type)) +
        scale_color_manual(values=c("#CC6666", "#999999")) + 
        scale_fill_manual(values=c("#CC6666", "#999999")) +
        geom_hline(yintercept = 0)
      fName <- paste0(figures_location, 'Emotion-', emotion, '_Treatment-', stringr::str_replace_all(sTreatment, '\\_', ''), '_Window-', time_window,'sec')
      ggsave(filename=fName, device='pdf')
    }
        
        
      
      
        
        
        
        l_emotion_regressions <- mapply(function(x,y) value_emotion_regressions(x, y), emotions, auction_types, MoreArgs = list(dt_fun=dt_window))
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
