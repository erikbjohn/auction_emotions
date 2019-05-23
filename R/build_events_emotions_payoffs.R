build_events_emotions_payoffs <- function(){
  ## Main function for joining all raw data into one dataset
  source('R/lube_time.R')
  source('R/events.R')
  source('R/payoffs.R')
  source('R/emotions.R')
  source('R/split_time_hours_check.R')
  
  # First, join emotions to events
  emotions_events_location <- '~/Dropbox/pkg.data/auction_emotions/Raw/emotions_events.rds'
  events_location <- '~/Dropbox/pkg.data/auction_emotions/Raw/events.rds'
  emotions_location <- '~/Dropbox/pkg.data/auction_emotions/Raw/emotions.rds'
  payoffs_location <- '~/Dropbox/pkg.data/auction_emotions/Raw/payoffs.rds'
  
  if(!file.exists(emotions_events_location)){
    if(!file.exists(events_location)){
      events_files <- list.files('~/Dropbox/pkg.data/auction_emotions/Raw/', pattern='^ESI.+[0-9]{1,1}\\.csv', full.names = TRUE, recursive = TRUE)#session1/ESI-115-02.csv'
      l_events <- lapply(events_files, events)
      dt_events <- rbindlist(l_events, use.names = TRUE, fill = TRUE)
      saveRDS(dt_events, events_location)
    } else {route214621801
      dt_events <- readRDS(events_location)
    }
    if(!file.exists(emotions_location)){
      emotions_files <- list.files('~/Dropbox/pkg.data/auction_emotions/Raw/', pattern='detailed.csv', full.names = TRUE, recursive = TRUE)
      l_emotions <- lapply(emotions_files, emotions)
      dt_emotions <- rbindlist(l_emotions, use.names=TRUE, fill=TRUE)
      saveRDS(dt_emotions, emotions_location)
    } else {
      dt_emotions <- readRDS(emotions_location)
    }
    
    # Make sure that total event time match toal emotion time (emotions record for longer duration than events)
    dt_events <- dt_events[nrow(dt_events), event_end:= dt_emotions[nrow(dt_emotions)]$snap_end]
    
    setkey(dt_events, Session, Participant, event_start, event_end)
    setkey(dt_emotions, Session, Participant, snap_start, snap_end)
    dt <- foverlaps(dt_events, dt_emotions) 
    dt <- dt[, .(Session, Participant, snap_start, snap_end, event_start, event_end, AuctionType, AuctionNumber, MarkerType,
                 Neutral, Happy, Sad, Angry, Surprised, Scared, Disgusted, Contempt, Valence, Arousal, Stimulus)]
    dt <- as.data.table(tidyr::gather(dt, key=EmotionType, value=Score,
                                      -c(Session, Participant, snap_start, snap_end, event_start, event_end,
                                         AuctionType, AuctionNumber, MarkerType)))
    dt$Score_num <- suppressWarnings(as.numeric(dt$Score))
    if (!file.exists(payoffs_location)){
      dt_payoffs <- payoffs()
      saveRDS(dt_payoffs, payoffs_location)
    } else {
      dt_payoffs <- readRDS(payoffs_location)
    }
    setkey(dt, Session, AuctionType, AuctionNumber, Participant)
    setkey(dt_payoffs, Session, AuctionType, AuctionNumber, Participant)
    dt <- dt[dt_payoffs]
    saveRDS(dt, emotions_events_location)
  } else {
    dt <- readRDS(emotions_events_location)
  }  
  return(dt)
}