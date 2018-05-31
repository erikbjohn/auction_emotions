emotions <- function(f_name){
  #f_name <- '~/Dropbox/pkg.data/auction_emotions/Raw/session1/Participant 2_ESI-115-02_Analysis 1_video_20170802_162439_detailed.csv'
  dt <- suppressWarnings(fread(f_name))
  
  # Impute Times
  dt$snap_start <- impute_times(dt$Video_Time)
  dt$snap_end <- dt$snap_start
  dt <- dt[, .(snap_start, snap_end, Neutral, Happy, Sad, Angry, Surprised, Scared, Disgusted, Contempt, Valence, Arousal, Stimulus)]
  return(dt)
}