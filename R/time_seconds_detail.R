time_seconds_detail <- function(x){
  x_split <- stringr::str_split(x,':', simplify=TRUE)
  minutes <- as.integer(x_split[1,1])
  seconds <- as.numeric(x_split[1,2])
  return(minutes*60 + seconds)
}
