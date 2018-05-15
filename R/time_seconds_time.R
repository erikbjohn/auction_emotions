time_seconds_time <- function(x){
  x_split <- stringr::str_split(x,':', simplify=TRUE)
  hours <- as.integer(x_split[1,1])
  minutes <- as.integer(x_split[1,2])
  seconds <- as.numeric(x_split[1,3])
  return(hours*360 + minutes*60 + seconds)
}
