impute_times <- function(x){
  SplitTimes <- stringr::str_split(x, ':')
  n_times <- length(SplitTimes)
  SplitTimes <- c(SplitTimes[1], SplitTimes[length(SplitTimes)])
  l_times <- lapply(SplitTimes, split_time_hours_check)
  times <- do.call('c', l_times)
  secs_start <- seconds(times[1])
  secs <- as.numeric(seconds(times)-secs_start)
  # Now, assume even breaks
  snap_secs <- secs[2]/(n_times-1)
  secs_breaks <- seq(0, secs[2], snap_secs)
  return(secs_breaks)
}