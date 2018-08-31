# analysis_emotions_graphical <- function(dt){
#   # Next produce plots
#   
#   emotions <- unique(dt_reg$emotion_type)
#   treatments <- unique(dt_reg$treatment)
#   dt_plots <- as.data.table(expand.grid(emotions, treatments, stringsAsFactors = FALSE))
#   setnames(dt_plots, names(dt_plots), c('emotion', 'treatment'))
#   
#   #dt_reg <- dt_reg[window_end < 15]
#   
#   # Marignal effect plots
#   for(iPlot in 1:nrow(dt_plots)){
#     emotion <- dt_plots[iPlot]$emotion
#     sTreatment <- dt_plots[iPlot]$treatment
#     dt_plot <- dt_reg[emotion_type == emotion & treatment == sTreatment]
#     dt_plot <- dt_plot[, conf_low := beta-1.96*std_error]
#     dt_plot <- dt_plot[, conf_high := beta+1.96*std_error]
#     p <- ggplot() + 
#       geom_ribbon(data=dt_plot, aes(x=window_end, ymin=conf_low, ymax=conf_high, group=auction_type, fill=auction_type), alpha = 0.3)+
#       geom_line(data=dt_plot, aes(x=window_end, y=beta, group=auction_type, colour=auction_type)) +
#       #geom_line(data=dt_plot, aes(x=window_end, y=conf_low, group=auction_type, colour=auction_type)) +
#       #geom_line(data=dt_plot, aes(x=window_end, y=conf_high, group=auction_type, colour=auction_type)) +
#       scale_color_manual(values=c("#CC6666", "#999999")) + 
#       scale_fill_manual(values=c("#CC6666", "#999999")) +
#       geom_hline(yintercept = 0)
#     fName <- paste0(figures_location, 'Emotion-', emotion, '_Treatment-', stringr::str_replace_all(sTreatment, '\\_', ''), '_Window-', time_window,'sec')
#     ggsave(filename=fName, device='pdf')
#   }
#   
#   # prediction plots
#   dt_pred_plots <- dt_plots[treatment=='p_high_value']
#   for(iPlot in 1:nrow(dt_pred_plots)){
#     emotion <- dt_pred_plots[iPlot]$emotion
#     sTreatment <- dt_pred_plots[iPlot]$treatment
#     # Start working right here
#     dt_pred_plot <- dt_reg[emotion_type == emotion & treatment==sTreatment]
#     intercept_outlier_dutch_high <- mean(dt_pred_plot[auction_type=='dutch']$intercept) + 4*sd(dt_pred_plot[auction_type=='dutch']$intercept)
#     intercept_outlier_dutch_low <- mean(dt_pred_plot[auction_type=='dutch']$intercept) - 4*sd(dt_pred_plot[auction_type=='dutch']$intercept)
#     dt_pred_plot <- dt_pred_plot[intercept > intercept_outlier_dutch_low]
#     dt_pred_plot <- dt_pred_plot[, predicted_hi_val:=intercept+beta*0.75]
#     dt_pred_plot <- dt_pred_plot[, predicted_low_val:=intercept+beta*0.25]
#     dt_pred_plot <- tidyr::gather(dt_pred_plot, key=predict_type, value=predict_value, predicted_hi_val, predicted_low_val)
#     dt_pred_plot <- as.data.table(dt_pred_plot)
#     dt_pred_plot <- dt_pred_plot[, predicts := interaction(auction_type, predict_type)]
#     dt_pred_plot$predicted <- sapply(dt_pred_plot$predicts, function(x) stringr::str_replace_all(x,'\\.predicted', ''))
#     
#     ggplot() + 
#       geom_line(data=dt_pred_plot, aes(x=window_end, y=predict_value,
#                                        group=predicted, 
#                                        colour=predicted)) +
#       #geom_line(data=dt_plot, aes(x=window_end, y=conf_low, group=auction_type, colour=auction_type)) +
#       #geom_line(data=dt_plot, aes(x=window_end, y=conf_high, group=auction_type, colour=auction_type)) +
#       scale_color_manual(values=c("#ff0000", "#999999","#CC6666", "#000000")) + 
#       scale_fill_manual(values=c("#CC6666", "#999999")) +
#       geom_hline(yintercept = 0) +
#       ggtitle(paste0(emotion, '_', stringr::str_replace_all(sTreatment, '\\_', '')))
#     fName <- paste0(figures_location, 'Prediction__Emotion-', emotion, '_Treatment-', stringr::str_replace_all(sTreatment, '\\_', ''))
#     ggsave(filename=fName, device='pdf')
#   }
#   
#   
#   
#   # Circular plots
#   ## Build circle
#   x_coords <- seq(-pi,pi,length=120) 
#   dt_coords  <- data.table(t(rbind(sin(x_coords), cos(x_coords))))
#   setnames(dt_coords, names(dt_coords), c('x_cord', 'y_cord'))
#   dt_coords_neg <- copy(dt_coords)
#   dt_coords_neg <- dt_coords_neg[, y_cord:=-y_cord]
#   dt_coords <- rbindlist(list(dt_coords, dt_coords_neg))
#   dt_cord_1 <- dt_coords[x_cord > 0 & y_cord > 0][order(x_cord)]
#   dt_cord_2 <- dt_coords[x_cord < 0 & y_cord > 0][order(x_cord)]
#   dt_cord_3 <- dt_coords[x_cord < 0 & y_cord < 0][order(x_cord)]
#   dt_cord_4 <- dt_coords[x_cord > 0 & y_cord < 0][order(x_cord)]
#   
#   for(sTreatment in treatments){
#     dt_cont <- dt_reg[treatment==sTreatment]
#     dt_cont <- dt_cont[window_end<40]
#     dt_cont <- dt_cont[emotion_type %in% c('Valence','Arousal')]
#     dt_cont <- dt_cont[, .(auction_type, window_end, emotion_type, predict)]
#     dt_sprd <- data.table::as.data.table(tidyr::spread(dt_cont, emotion_type, predict))
#     dt_sprd <- dt_sprd[Valence > -1 & Valence < 1]
#     dt_sprd <- dt_sprd[Arousal > -1 & Arousal < 1]
#     dt_sprd <- dt_sprd[, Arousal := (Arousal*2)-1]
#     dt_sprd <- dt_sprd[, smArousal:=smooth.spline(Arousal, spar=0.1)$y]
#     dt_sprd <- dt_sprd[, smValence:=smooth.spline(Valence, spar=0.1)$y]
#     setkey(dt_sprd, auction_type, window_end)
#     
#     ggplot2::ggplot(dt_sprd, aes(x=Valence, y=Arousal, group=auction_type, color=auction_type)) +
#       geom_segment(aes(x=-1.25, y=0, xend=1.25, yend=0), color='black', size=0.1) + 
#       geom_segment(aes(x=0, y=-1.25, xend=0, yend=1.25), color='black', size=0.1) +
#       geom_path() + 
#       geom_path(data=dt_coords, aes(x=x_cord, y=y_cord), col='black', size=0.1) +
#       # Quadrant 1
#       geom_segment(aes(x=0, y=0, xend=dt_cord_1[15]$x_cord, yend=dt_cord_1[15]$y_cord), color='grey', size=0.1, linetype='dashed') +
#       geom_text(aes(x=dt_cord_1[7]$x_cord + 0.05, y=dt_cord_1[7]$y_cord + 0.075, label='Alert'), colour='black') +
#       geom_segment(aes(x=0, y=0, xend=dt_cord_1[30]$x_cord, yend=dt_cord_1[30]$y_cord), color='grey', size=0.1, linetype='dashed') +
#       geom_text(aes(x=dt_cord_1[22]$x_cord + 0.1, y=dt_cord_1[22]$y_cord + 0.075, label='Excited'), colour='black') +
#       geom_segment(aes(x=0, y=0, xend=dt_cord_1[45]$x_cord, yend=dt_cord_1[45]$y_cord), color='grey', size=0.1, linetype='dashed') +
#       geom_text(aes(x=dt_cord_1[37]$x_cord + 0.125, y=dt_cord_1[37]$y_cord + 0.05, label='Elated'), colour='black') +
#       geom_text(aes(x=dt_cord_1[52]$x_cord + 0.2, y=dt_cord_1[52]$y_cord, label='Pleasant'), colour='black') +
#       # Quadrant 2
#       geom_segment(aes(x=0, y=0, xend=dt_cord_2[15]$x_cord, yend=dt_cord_2[15]$y_cord), color='grey', size=0.1, linetype='dashed') +
#       geom_text(aes(x=dt_cord_2[7]$x_cord - 0.15, y=dt_cord_2[7]$y_cord + 0.05, label='Upset'), colour='black') +
#       geom_segment(aes(x=0, y=0, xend=dt_cord_2[30]$x_cord, yend=dt_cord_2[30]$y_cord), color='grey', size=0.1, linetype='dashed') +
#       geom_text(aes(x=dt_cord_2[22]$x_cord - 0.2, y=dt_cord_2[22]$y_cord + 0.05, label='Stressed'), colour='black') +
#       geom_segment(aes(x=0, y=0, xend=dt_cord_2[45]$x_cord, yend=dt_cord_2[45]$y_cord), color='grey', size=0.1, linetype='dashed') +
#       geom_text(aes(x=dt_cord_2[37]$x_cord - 0.15, y=dt_cord_2[37]$y_cord + 0.1, label='Nervous'), colour='black') +
#       geom_text(aes(x=dt_cord_2[52]$x_cord, y=dt_cord_2[52]$y_cord + 0.075, label='Tense'), colour='black') +
#       # Quadrant 3
#       geom_segment(aes(x=0, y=0, xend=dt_cord_3[20]$x_cord, yend=dt_cord_3[20]$y_cord), color='grey', size=0.1, linetype='dashed') +
#       geom_text(aes(x=dt_cord_3[10]$x_cord - 0.15, y=dt_cord_3[10]$y_cord -0.05, label='Sad'), colour='black') +
#       geom_segment(aes(x=0, y=0, xend=dt_cord_3[40]$x_cord, yend=dt_cord_3[40]$y_cord), color='grey', size=0.1, linetype='dashed') +
#       geom_text(aes(x=dt_cord_3[30]$x_cord - 0.325, y=dt_cord_3[30]$y_cord, label='Depressed'), colour='black') +
#       geom_text(aes(x=dt_cord_3[50]$x_cord, y=dt_cord_3[50]$y_cord -0.1, label='Bored'), colour='black') +
#       # Quadrant 4
#       geom_segment(aes(x=0, y=0, xend=dt_cord_4[15]$x_cord, yend=dt_cord_4[15]$y_cord), color='grey', size=0.1, linetype='dashed') +
#       geom_text(aes(x=dt_cord_4[7]$x_cord + 0.025, y=dt_cord_4[7]$y_cord -0.1, label='Calm'), colour='black') +
#       geom_segment(aes(x=0, y=0, xend=dt_cord_4[30]$x_cord, yend=dt_cord_4[30]$y_cord), color='grey', size=0.1, linetype='dashed') +
#       geom_text(aes(x=dt_cord_4[22]$x_cord + 0.2, y=dt_cord_4[22]$y_cord - 0.05, label='Relaxed'), colour='black') +
#       geom_segment(aes(x=0, y=0, xend=dt_cord_4[45]$x_cord, yend=dt_cord_4[45]$y_cord), color='grey', size=0.1, linetype='dashed') +
#       geom_text(aes(x=dt_cord_4[37]$x_cord + 0.2, y=dt_cord_4[37]$y_cord, label='Serene'), colour='black') +
#       geom_text(aes(x=dt_cord_4[52]$x_cord + 0.2, y=dt_cord_4[52]$y_cord, label='Content'), colour='black') +
#       # Title
#       geom_text(aes(x=0, y= -1.4, label=paste0('Treatment Type:', sTreatment)), colour='black') +
#       scale_x_continuous(limits = c(-1.5, 1.5)) +
#       scale_y_continuous(limits = c(-1.5,1.5)) + 
#       theme_void() + 
#       theme(legend.position = 'none')
#     
#     fName <- paste0(figures_location, 'Circles-', '_Treatment:', sTreatment,'.pdf')
#     ggsave(filename=fName, device='pdf')
#   }
#   
#   # Summary statistics of emotions by auction and stage (auction, transition, information)
#   dt_sum <- dt_scores[AuctionType == 'dutch' & MarkerType %in% c('transition', 'info'), stage:='Feedback Stage']
#   dt_sum <- dt_sum[AuctionType == 'first_price' & MarkerType %in% c('info'), stage:='Feedback Stage']
#   dt_sum <- dt_sum[MarkerType %in% 'auction', stage:='Auction Stage']
#   dt_sum <- dt_sum[MarkerType %in% 'start', stage:='Start Stage']
#   dt_sum <- dt_sum[!is.na(stage)]
#   dt_sum_full <- copy(dt_sum)
#   dt_sum_full <- dt_sum_full[, c('mean', 'sd', 'N') := list(mean(Score_num), sd(Score_num), .N), by=c('AuctionType', 'stage', 'EmotionType')]
#   dt_sum_full <- unique(dt_sum_full[, .(AuctionType, stage, EmotionType, mean, sd, N)])
#   setkey(dt_sum_full, EmotionType, stage, AuctionType)
#   dt_sum_money <- copy(dt_sum)
#   dt_sum_money <- dt_sum_money[Value>180, c('mean', 'sd') := list(mean(Score_num), sd(Score_num)), by=c('AuctionType', 'stage', 'EmotionType')]
#   dt_sum_money <- unique(dt_sum_money[, .(AuctionType, stage, EmotionType, mean, sd)])
#   setkey(dt_sum_money, EmotionType, stage, AuctionType)
# }
# 
# 
# }