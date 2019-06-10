payoffs <- function(){
  # f_name <- '~/Dropbox/pkg.data/auction_emotions/Raw/session1/170508_1006.xls'
   payoffs_location <- '~/Dropbox/pkg.data/auction_emotions/Clean/payoffs.rds'
   if(!file.exists(payoffs_location)){
    dt_files <- data.table(payoff_f_path = list.files('~/Dropbox/pkg.data/auction_emotions/Raw', recursive = TRUE, full.names=TRUE, pattern='.xls'),
                           payoff_f_name = list.files('~/Dropbox/pkg.data/auction_emotions/Raw', recursive = TRUE, pattern='.xls'))
    dt_files$session <- stringr::str_extract(dt_files$payoff_f_name, '(?i)(?<=Session)[0-9]{1,3}')
    full_payoffs <- list()
    for(i_file in 1:nrow(dt_files)){
      session <- dt_files[i_file]$session
      cat(session)
      f_lines <- readLines(dt_files[i_file]$payoff_f_path)
      
      l_lines <-stringr::str_split(f_lines, '\\\t')
      # Find Dutch and First Price Auction Ranges
      break_line <- min(which(lapply(l_lines, '[[', 3)=='summary'))
      blocks <- list()
      blocks[[1]] <- 1:break_line
      blocks[[2]] <- (break_line+1):length(l_lines)
      bid_lines <- which(sapply(l_lines, function(x) length(which(stringr::str_detect(x, 'Bid'))))>0)[1]
      lines <- list()
      if(bid_lines %in% blocks[[1]]){
        lines$first_price <- l_lines[blocks[[1]]]
        lines$dutch <- l_lines[blocks[[2]]]
      } else {
        lines$first_price <- l_lines[blocks[[2]]]
        lines$dutch <- l_lines[blocks[[1]]]
      }
      l_payoffs <- lapply(lines, extract_subjects)
      auction_types <- names(l_payoffs)
      payoffs <- mapply(function(x, y) data.table(x, AuctionType=y), l_payoffs, auction_types)
      payoffs <- lapply(payoffs, function(x) data.table(x, Session=session))
      payoffs <- lapply(payoffs, function(x) setnames(x, c('Period', 'Subject'), c('AuctionNumber', 'Participant')))
      payoffs <- lapply(payoffs, function(x) setnames(x, names(x)[2], 'AuctionTypeOrder'))
      
      # First Price Auction data
      payoffs$first_price$BidNash <- (3/4)*as.integer(payoffs$first_price$Value)
      payoffs$first_price <- payoffs$first_price[, .(Session, AuctionType='first_price', 
                                                     AuctionNumber, AuctionTypeOrder, 
                                                     Participant, Group, 
                                                     TimeToBid = 30-as.integer(TimeSubmitBidDecisionOK), 
                                                     Winner=as.integer(Winner),
                                                     Value=as.integer(Value),
                                                     BidActual=as.integer(Bid),
                                                     BidNash,
                                                     Price=as.integer(Price),
                                                     Profit=as.integer(Profit)
                                                     )]
      payoffs$dutch$BidNash <- (3/4)*as.integer(payoffs$dutch$Value)
      payoffs$dutch <- payoffs$dutch[, .(Session, AuctionType='dutch',
                                         AuctionNumber, AuctionTypeOrder, 
                                         Participant, Group, 
                                         TimeToBid = TimeBuyAuction1OK,
                                         BidNash,
                                         Value=as.integer(Value),
                                         Price=as.integer(Price),
                                         Profit=as.integer(Profit))]
      
      full_payoffs[[i_file]] <- list(first_price = payoffs$first_price,
                                   dutch = payoffs$dutch)
      names(full_payoffs)[i_file] <- paste0('Session', session)
    }
    l <- list()
    l$first_price <- rbindlist(lapply(full_payoffs, '[[', which(names(full_payoffs[[1]])=='first_price')))
    l$dutch <- rbindlist(lapply(full_payoffs, '[[', which(names(full_payoffs[[1]])=='dutch')))
    l$dutch <- l$dutch[TimeToBid>0, Winner:=1][TimeToBid==0, Winner:=0]
    dt<-rbindlist(l, use.names = TRUE, fill=TRUE)
    # Fix the time to bid issue in the dutch auctions
    dt <- dt[, TimeToBid:=as.numeric(TimeToBid)]
    dt <- dt[Winner==1 & AuctionType=='dutch', TimeToBid := ((240-as.integer(Price))/3)*.5]
    saveRDS(dt, payoffs_location)
   } else {
    dt <- readRDS(payoffs_location)
   }
   return(dt)
} 

extract_subjects <- function(lines_x){
  # Break out by subject lines
  subjects_lines <- which(lapply(lines_x, '[[', 3) %in% 'subjects')
  l_subjects <- lines_x[subjects_lines]
  subjects_header <- l_subjects[which(lapply(l_subjects, '[[', 4)=='Period')][[1]]
  subjects_data <- l_subjects[which(lapply(l_subjects, '[[', 4)!='Period')]
  subjects_data <- lapply(subjects_data, function(x) data.table::as.data.table(t(x)))
  dt_subjects <- rbindlist(subjects_data)
  names(dt_subjects) <- subjects_header
  return(dt_subjects)
}
