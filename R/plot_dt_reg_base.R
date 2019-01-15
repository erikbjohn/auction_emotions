plot_dt_reg_base <- function(){
  library(ggplot2)
  library(data.table)
  source('R/build_dt_reg_base.R')
  plot_dt_reg_base_location <- '~/Dropbox/pkg.data/auction_emotions/Figures_regression/dt_reg_base_location.rds'
  if(!file.exists(plot_dt_reg_base_location)){
     dt_reg_base <- build_dt_reg_base()
     dt_reg_base <- dt_reg_base[abs(beta)<1 & window_begin<10]
     dt_reg_base <- dt_reg_base[, conf_low:=beta-2*beta_std_error]
     dt_reg_base <- dt_reg_base[, conf_hi:=beta+2*beta_std_error]
     dt_reg_base$sig <- FALSE
     dt_reg_base <- dt_reg_base[p_val < 0.05, sig:=TRUE]
     emotions <- unique(dt_reg_base$emotion_type)
     treatments <- unique(dt_reg_base$treatment)
     dt_plots <- data.table::data.table(expand.grid(emotions, treatments, stringsAsFactors = FALSE))
     names(dt_plots) <- c('emotion', 'treatment')
     for(iPlot in 1:nrow(dt_plots)){
       iEmotion <- dt_plots[iPlot]$emotion
       iTreatment <- dt_plots[iPlot]$treatment
       dt_plot <- dt_reg_base[emotion_type %in% iEmotion & treatment %in% iTreatment]
       y_max <- max(dt_plot$beta)
       y_min <- min(dt_plot$beta)
       y_bounds <- c(-max(abs(y_min), abs(y_max)), max(abs(y_min), abs(y_max)))
       dt_plot <- dt_plot[, conf_hi := pmin(conf_hi, y_bounds[2])]
       dt_plot <- dt_plot[, conf_low := pmax(conf_low, y_bounds[1])]
       gg_out <- ggplot2::ggplot(dt_plot, aes(x=window_begin, y=beta, group=auction_type)) +
         geom_line(aes(colour=auction_type)) + 
         geom_ribbon(aes(ymin=conf_low, ymax=conf_hi, fill=auction_type),alpha=0.1) +
         #geom_ribbon(data=dt_reg_base[sig==TRUE], aes(ymin=conf_low, ymax=conf_hi, fill=auction_type),alpha=1.0) +
         ylim(y_bounds) +
         geom_hline(yintercept=0, alpha=0.5) +
         ggtitle(paste('Emotion:    ', iEmotion, '\nTreatement:', iTreatment))
        
       print(gg_out)
      ggsave(paste0('~/Dropbox/pkg.data/auction_emotions/Figures_regression/dt_reg_base/',iEmotion, iTreatment, '.pdf'))     
     }
  } else {
    plot_dt_reg_base <- readRDS(plot_dt_reg_base_location)
  }
}