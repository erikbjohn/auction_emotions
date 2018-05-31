payoffs_summarize <- function(){
  source('R/payoffs.R')
  l <- payoffs()
  # First Price Summary
  
  dt_dutch <- l$dutch
  dt_dutch <- dt_dutch[TimeToBid>0]
  dt_dutch$BidActual <- dt_dutch$Price
  dt_dutch$type <- 'dutch'
  dt_dutch <- dt_dutch[, .(BidNash, BidActual, AuctionType)]
  
  dt_fp <- l$first_price
  dt_fp <- dt_fp[BidActual<500 & Winner == 1]
  dt_fp <- dt_fp[, .(BidNash, BidActual, AuctionType)]
  
  dt_nash <- data.table(BidNash = unique(dt_fp$BidNash))
  dt_nash$BidActual <- dt_nash$BidNash
  dt_nash$AuctionType <- 'nash'
  
  dt <- rbindlist(list(dt_dutch, dt_fp, dt_nash), use.names = TRUE)
  p <- ggplot(dt, aes(x=as.factor(BidNash), y=BidActual)) +
    geom_boxplot(aes(fill=AuctionType), outlier.size = 0.1) +
    geom_line(aes(x=as.factor(BidNash), y=BidNash)) + 
    theme_bw()
  ggsave('~/Dropbox/pkg.data/auction_emotions/Figures/payoffs_type.pdf', plot=p,width=10, height=6, units = 'in')
    
}

