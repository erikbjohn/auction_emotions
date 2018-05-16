split_time_hours_check <- function(split_time){
  # Used for empirical (emotional data) date stamps
  if(length(split_time)==2){
    l_hours <- 0
    l_minutes <- as.numeric(split_time[1])
    l_seconds <- as.numeric(split_time[2])
  }
  if(length(split_time)==3){
    l_hours <- as.numeric(split_time[1])
    l_minutes <- as.numeric(split_time[2])
    l_seconds <- as.numeric(split_time[3])
  }
  dt_lube <- data.table(l_hours, l_minutes, l_seconds)
  return(lube_time(dt_lube))
}
