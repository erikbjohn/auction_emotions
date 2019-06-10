events <- function(file_location){ 
  events_location <- '~/Dropbox/pkg.data/auction_emotions/Raw/events.rds'
  if(!file.exists(events_location)){
  source('R/lube_time.R')
  source('R/extract_marker_info.R')
  library(data.table)
  library(lubridate)
  library(dplyr)
  # file_location <- '~/Dropbox/pkg.data/auction_emotions/Raw/session1/ESI-115-02.csv'
  dt <- suppressWarnings(fread(file_location))
  SplitTimes <- stringr::str_split(dt$TimecodeSession, ':')
  dt_events_actual = data.table(l_hours = as.numeric(sapply(SplitTimes, '[[', 1)))
  dt_events_actual$l_minutes <- as.numeric(sapply(SplitTimes, '[[', 2))
  dt_events_actual$l_seconds <- as.numeric(sapply(SplitTimes, '[[', 3))
  start_times <- sapply(1:nrow(dt_events_actual), function(x) lube_time(dt_events_actual[x]), simplify = FALSE)
  dt$dt_start_time <- do.call('c', start_times)
  dt$dt_end_time <- lead(dt$dt_start_time)
  dt <- dt[, .(REF_MarkerName, dt_start_time, dt_end_time)]
  dt_groups <- dt[, .(start.point = .I[1], end.point = .I[.N]), by = .(REF_MarkerName, rleid(REF_MarkerName))]
  dt_groups$dt_start_time <- dt[dt_groups$start.point]$dt_start_time
  dt_groups$dt_end_time <- dt[dt_groups$end.point]$dt_end_time
  dt <- dt_groups[, .(REF_MarkerName, dt_start_time, dt_end_time)]
  dt_marker <- extract_marker_info(dt$REF_MarkerName)
  dt <- data.table(dt, dt_marker)
  # Fill undefined markers with info
  dt$lead_MarkerType <- lead(dt$MarkerType)
  dt$lead_AuctionType <- lead(dt$AuctionType)
  dt$lag_AuctionType <- lag(dt$AuctionType)
  dt$lead_AuctionNumber <- lead(dt$AuctionNumber)
  dt$lag_AuctionNumber <- lag(dt$AuctionNumber)
  
  dt <- dt[Event_Marker %in% 'undefined marker' & lead(MarkerType) %in% 'auction', MarkerType:='start']
  dt <- dt[Event_Marker %in% 'undefined marker' & lead(MarkerType) %in% 'info', MarkerType:='transition']
  
  dt <- dt[MarkerType %in% c('start', 'transition'), AuctionType := lead_AuctionType]
  dt <- dt[MarkerType %in% c('start'), AuctionNumber:=lead_AuctionNumber]
  dt <- dt[MarkerType %in% c('transition'), AuctionNumber:=lag_AuctionNumber]
  
  # clean up first and last 
  if(dt[nrow(dt)]$Event_Marker=='undefined marker'){
    dt[nrow(dt)-1]$dt_end_time <- dt[nrow(dt)]$dt_start_time
    dt <- dt[1:(nrow(dt)-1)]
  }
  dt$lead_MarkerType <- lead(dt$MarkerType)
  dt$lead_AuctionType <- lead(dt$AuctionType)
  dt$lag_AuctionType <- lag(dt$AuctionType)
  dt$lead_AuctionNumber <- lead(dt$AuctionNumber)
  dt$lag_AuctionNumber <- lag(dt$AuctionNumber)
  
  dt <- dt[Event_Marker %in% 'camera start', AuctionNumber:=lead_AuctionNumber]
  dt <- dt[Event_Marker %in% 'camera start', AuctionType:=lead_AuctionType]
  dt <- dt[MarkerType %in% 'final payment', AuctionNumber:=lag_AuctionNumber]
  
  # Clean and normalize seconds
  dt <- dt[, .(dt_start_time, dt_end_time, AuctionType, AuctionNumber, MarkerType)]
  sec_start <- seconds(dt$dt_start_time)[1]
  dt$event_start <- as.numeric(seconds(dt$dt_start_time-sec_start))
  dt$event_end <- as.numeric(seconds(dt$dt_end_time-sec_start))
  dt <- dt[,.(event_start, event_end, AuctionType, AuctionNumber, MarkerType)]
  
  # Include Session, Participant
  dt$Session <- stringr::str_extract(file_location, stringr::regex('(?<=session)[0-9]{1,2}', perl=TRUE))
  dt$Participant <- stringr::str_extract(file_location, stringr::regex('[0-9]{1,2}(?=\\.csv)'))
  dt$Participant <- stringr::str_replace(dt$Participant, stringr::regex('0(?=[0-9]{1,1}$)'), '')
  dt <- dt[nrow(dt_events), event_end:= dt_emotions[nrow(dt_emotions)]$snap_end]
  saveRDS(dt, events_location)
  } else {
    dt <- readRDS(events_location)
  }
  # For first price auctions, include the bid submission time
  return(dt)
}


