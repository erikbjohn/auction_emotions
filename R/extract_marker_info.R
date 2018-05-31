extract_marker_info <- function(x){
  dt <- data.table(Event_Marker = x)
  
  # Create auction number 
  dt <- dt[, AuctionNumber:=stringr::str_extract(Event_Marker, '[0-9]{1,2}(?=$)')]
  
  # Clean up REF_MarkerName
  regex_infoFlag <- '(i[0-9]{1,2}(?=$)|info)'
  dt <- dt[stringr::str_detect(Event_Marker, regex_infoFlag), MarkerType:='info']
  regex_auctionFlag <- '(fpa|da)'
  dt <- dt[stringr::str_detect(Event_Marker, regex_auctionFlag), MarkerType:='auction']
  dt <- dt[grep('finalpayment', Event_Marker), MarkerType:='final payment']
  dt <- dt[grep('camera start', Event_Marker), MarkerType:='camera start']
  
  # Create auction type flags
  regex_dutch <- '(?i)(da|infod)' #da1 infod1
  regex_fp <- '(?i)fp' #fpa1, fpi1
  dt <- dt[stringr::str_detect(Event_Marker, regex_dutch), AuctionType:='dutch']
  dt <- dt[stringr::str_detect(Event_Marker, regex_fp), AuctionType:='first_price']
  return(dt)
}