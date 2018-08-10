build_dt_reg <- function(dt=NULL){
  # Converts the full, raw dataset into regression usable format
  dt_reg_location <- '~/Dropbox/pkg.data/auction_emotions/Clean/dt_reg.rds'
  if(!file.exists(dt_reg_location)){
    if(is.null(dt)){
      dt <- build_events_emotions_payoffs()
      dt <- dt[, marker_start:=min(snap_start), by=c('Session', 'Participant', 'AuctionType', 'AuctionNumber', 'MarkerType')]
      dt <- dt[, marker_time_elapsed := snap_start-marker_start]
      dt <- dt[, marker_2sec:=0][marker_time_elapsed < 2, marker_2sec := 1]
      dt <- dt[, marker_5sec:=0][marker_time_elapsed < 5, marker_5sec := 1]
    }
    saveRDS(dt_reg, dt_reg_location)
  } else {
    dt_reg <- readRDS(dt_reg_location)
  }
}