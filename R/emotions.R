emotions <- function(f_name){
  emotions_location <- '~/Dropbox/pkg.data/auction_emotions/Raw/emotions.rds'
  if(!file.exists(emotions_location)){
    source('R/impute_times.R')
    #f_name <- '~/Dropbox/pkg.data/auction_emotions/Raw/session1/Participant 2_ESI-115-02_Analysis 1_video_20170802_162439_detailed.csv'
    dt <- suppressWarnings(fread(f_name))
    
    # Impute Times
    dt$snap_start <- impute_times(dt$Video_Time)
    dt$snap_end <- dt$snap_start
    dt <- dt[, .(snap_start, snap_end, Neutral, Happy, Sad, Angry, Surprised, Scared, Disgusted, Contempt, Valence, Arousal, Stimulus)]
    dt$Participant <- stringr::str_extract(f_name, '(?<=Participant )[0-9]{1,2}')
    dt$Session <- stringr::str_extract(f_name, stringr::regex('(?<=session)[0-9]{1,2}', perl=TRUE))
  } else {
    dt <- readRDS(emotions_location)
  }
  return(dt)
}