lube_time <- function(dt_lube){
  l_hours <- dt_lube$l_hours
  l_minutes <- dt_lube$l_minutes
  l_seconds <- dt_lube$l_seconds
  times <- make_datetime(year = 2017, month = 1, day = 1,hour = l_hours,min = l_minutes, sec = l_seconds,
                         tz = "UTC")
  return(times)
}