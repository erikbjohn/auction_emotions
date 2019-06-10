build_events_emotions_payoffs <- function(){
  ## Main function for joining all raw data into one dataset
  source('R/lube_time.R')
  source('R/events.R')
  source('R/payoffs.R')
  source('R/emotions.R')
  source('R/split_time_hours_check.R')
  
  # First, join emotions to events
  emotions_events_location <- '~/Dropbox/pkg.data/auction_emotions/Raw/emotions_events.rds'
  if(!file.exists(emotions_events_location)){
    dt_events <- events()
    setkey(dt_events, Session, Participant, event_start, event_end)
    dt_emotions <- emotions()
    setkey(dt_emotions, Session, Participant, snap_start, snap_end)
    dt_payoffs <- payoffs()
    
    # Make sure that total event time match toal emotion time (emotions record for longer duration than events)
    dt <- foverlaps(dt_events, dt_emotions) 
    dt <- dt[, .(Session, Participant, snap_start, snap_end, event_start, event_end, AuctionType, AuctionNumber, MarkerType,
                 Neutral, Happy, Sad, Angry, Surprised, Scared, Disgusted, Contempt, Valence, Arousal, Stimulus)]
    dt <- as.data.table(tidyr::gather(dt, key=EmotionType, value=Score,
                                      -c(Session, Participant, snap_start, snap_end, event_start, event_end,
                                         AuctionType, AuctionNumber, MarkerType)))
    dt$Score_num <- suppressWarnings(as.numeric(dt$Score))

    setkey(dt, Session, AuctionType, AuctionNumber, Participant)
    setkey(dt_payoffs, Session, AuctionType, AuctionNumber, Participant)
    dt <- dt[dt_payoffs]
    dt <- dt[, marker_start:=min(snap_start), by=c('Session', 'Participant', 'AuctionType', 'AuctionNumber', 'MarkerType')]
    dt <- dt[, marker_time_elapsed := snap_start-marker_start]
    dt <- dt[, marker_sec_elapsed := floor(marker_time_elapsed)]
    dt <- dt[, participant_id := paste0(Session, '_', Participant)]
    
    saveRDS(dt, emotions_events_location)
  } else {
    dt <- readRDS(emotions_events_location)
  }  
  return(dt)
}