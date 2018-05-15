dutch_datawork <- function(){

library(data.table)
library(stringr)

data_directory <- '~/Dropbox/Research/Deck'

time_seconds_time <- function(x){
  x_split <- stringr::str_split(x,':', simplify=TRUE)
  hours <- as.integer(x_split[1,1])
  minutes <- as.integer(x_split[1,2])
  seconds <- as.numeric(x_split[1,3])
  return(hours*360 + minutes*60 + seconds)
}

time_seconds_detail <- function(x){
  x_split <- stringr::str_split(x,':', simplify=TRUE)
  minutes <- as.integer(x_split[1,1])
  seconds <- as.numeric(x_split[1,2])
  return(minutes*60 + seconds)
}

fList <- list.files(data_directory, full.names = TRUE)

# Find first participant csv file
# Each participant has an id. 
# Easy to loop over all. here, we just use first participant
l.participant <- list()
participant_ids <- 1:172
for (participant_id in 1:participant_ids){
  detail_file <- names(which(sapply(fList, function(x) stringr::str_detect(x, paste0('Participant ', participant_id, '_'))))[1])
  esi_split <- stringr::str_split(detail_file, '-', simplify = TRUE)
  esi_split_2 <- stringr::str_split(esi_split[3], '_', simplify=TRUE)
  esi_key <- paste0('ESI','-',esi_split[2], '-', esi_split_2[1,1])
  time_file <- names(which(sapply(fList, function(x) stringr::str_detect(x, paste0(esi_key, '.csv'))))[1])

  dt_detail <- data.table::fread(detail_file)
  dt_time <- data.table::fread(time_file)
  
  # Format files for times
  dt_detail$time_stamp <- sapply(dt_detail$Video_Time, time_seconds_detail, USE.NAMES=FALSE)
  dt_time$time_stamp <- sapply(dt_time$TimecodeSession, time_seconds_time, USE.NAMES=FALSE)
  
  events <- unique(dt_time$REF_MarkerName)
  events <- events[!(events %in% c('camera start', 'undefined marker'))]
  events_da <- events[stringr::str_detect(events, 'da')]
  l.events <- list()
  for (iEvent in 1:length(events_da)){
    #cat(iEvent)
    event <- events_da[iEvent]
    dt_detail_event <- dt_detail[Event_Marker == event]
    dt_time_event <- dt_time[REF_MarkerName == event]
    time_start_actual <- dt_time_event$time_stamp
    info_event <- paste0('info', stringr::str_replace(event, 'a', ''))
    time_end_time <- dt_time[REF_MarkerName == info_event]$time_stamp
    time_end_detail <- max(dt_detail_event$time_stamp)
    time_end <- max(time_end_time, time_end_detail)Target4,973
    # Break up into 0.5 second segments
    time_floor <- seq(time_start_actual, time_end, 0.5)
    time_ceil <- c(time_floor[2:(length(time_floor))]-0.001, time_end)
    dt_event <- data.table(tick_id = 1:length(time_ceil), time_floor= time_floor, time_ceil=time_ceil)
    dt_event$event <- event
    dt_event$participant_id <- participant_id
    dt_event$esi_key <- esi_key
    dt_event$clock_price <- 240 - (dt_event$tick_id-1)*3
    # Cheap and easy but computationally expensive solution
    for(iTick in 1:nrow(dt_event)){
      tick_detail <- dt_detail[time_stamp >= dt_event[iTick]$time_floor & time_stamp <= dt_event[iTick]$time_ceil & Event_Marker == event]
      if (nrow(tick_detail)>0){
        dt_event <- dt_event[iTick, Neutral := mean(as.numeric(tick_detail$Neutral))]
        dt_event <- dt_event[iTick, Happy := mean(as.numeric(tick_detail$Happy))]
        dt_event <- dt_event[iTick, Sad := mean(as.numeric(tick_detail$Sad))]
        dt_event <- dt_event[iTick, Angry := mean(as.numeric(tick_detail$Angry))]
        dt_event <- dt_event[iTick, Surprised := mean(as.numeric(tick_detail$Surprised))]
        dt_event <- dt_event[iTick, Scared := mean(as.numeric(tick_detail$Scared))]
        dt_event <- dt_event[iTick, Disgusted := mean(as.numeric(tick_detail$Disgusted))]
        dt_event <- dt_event[iTick, Contempt := mean(as.numeric(tick_detail$Contempt))]
        dt_event <- dt_event[iTick, Valence := mean(as.numeric(tick_detail$Valence))]
        dt_event <- dt_event[iTick, Arousal := mean(as.numeric(tick_detail$Arousal))]
      }
    }
    l.events[[iEvent]] <- dt_event 
  }
  l.participant[[participant_id]] <- data.table::rbindlist(l.events, use.names = TRUE, fill=TRUE)
}
dt_tickData <- data.table::rbindlist(l.participant, use.names = TRUE, fill=TRUE)
write.csv(dt_tickData, file=paste0(data_directory,'/tickDataNew.csv'))
return(dt_tickdata)

}
