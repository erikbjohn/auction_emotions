payoffs_summarize <- function(){
  source('R/payoffs.R')
  dt_payoffs <- payoffs()
  # First Price Summary
  
  dt_dutch <- dt_payoffs[AuctionType=='dutch']
  dt_dutch <- dt_dutch[TimeToBid>0]
  dt_dutch$BidActual <- dt_dutch$Price
  dt_dutch$type <- 'dutch'
  dt_dutch <- dt_dutch[, .(BidNash, AuctionTypeOrder, BidActual, AuctionType, AuctionNumber)]
  
  dt_fp <- dt_payoffs[AuctionType == 'first_price']
  dt_fp <- dt_fp[BidActual<500 & Winner %in% c(1)]
  dt_fp <- dt_fp[, .(BidNash, AuctionTypeOrder, BidActual, AuctionType, AuctionNumber)]
  
  dt_nash <- data.table(BidNash = unique(dt_fp$BidNash))
  dt_nash$BidActual <- dt_nash$BidNash
  dt_nash$AuctionType <- 'nash'
  
  dt <- rbindlist(list(dt_dutch, dt_fp, dt_nash), use.names = TRUE, fill=TRUE)
  p <- ggplot(dt, aes(x=as.factor(BidNash), y=BidActual)) +
    geom_boxplot(aes(fill=AuctionType), outlier.size = 0.1) +
    geom_line(aes(x=as.factor(BidNash), y=BidNash)) + 
    theme_bw()
  plot(p)
  ggsave('~/Dropbox/pkg.data/auction_emotions/Figures/payoffs_type.pdf', plot=p,width=10, height=6, units = 'in')
  
  
  # Check this out for the first ten auctions
  dt <- dt[, AuctionNumberInteger:=as.integer(AuctionNumber)]
  dt <- dt[,pctOfNash:=BidActual/BidNash]
  dt <- dt[!is.na(AuctionTypeOrder)]
  
  dt_auctionNumberIntegers <- data.table(high=sort(unique(dt_cut$AuctionNumberInteger)))
  dt_auctionNumberIntegers <- dt_auctionNumberIntegers[, low:=high-1]
  dt_auctionNumberIntegers <- dt_auctionNumberIntegers[low>0]
  
  dt_auctionNumberIntegers <- data.table(low=c(0, 22), high=c(3, 25))
  
  l_dt_orders <- vector(mode = 'list', length=length(unique(dt$AuctionTypeOrder)))
  for(iAuctionOrder in 1:length(unique(dt$AuctionTypeOrder))){
    auctionOrder <- unique(dt$AuctionTypeOrder)[iAuctionOrder]
    dt_order <- dt[AuctionTypeOrder %in% auctionOrder]
    # 95% of Nash quantiles
    quants <- quantile(dt_order$pctOfNash, c(0.025, 0.975))
    dt_cut <- dt[pctOfNash > quants[1] & pctOfNash < quants[2]]
    dt_cut <- dt_cut[!is.na(AuctionNumberInteger)]
    # l_ttest <- vector(mode='list', length=length(AuctionNumberIntegers))
    fun_l_ttest_to_dt <- function(l_in){
      dt_ttest <- data.table(t_stat=l_in$statistic)
      dt_ttest$df <- l_in$parameter
      dt_ttest$p_value <- l_in$p.value
      dt_ttest$conf_95_low <- l_in$conf.int[1]
      dt_ttest$conf_95_hi <- l_in$conf.int[2]
      dt_ttest$dutch <- l_in$estimate[1]
      dt_ttest$first_price <- l_in$estimate[2]
      dt_ttest$null_value <- l_in$null.value
      dt_ttest$test_type <- l_in$alternative
      return(dt_ttest)
    }
    l_dts <- vector(mode='list', length=nrow(dt_auctionNumberIntegers))
    for(iCut in 1:nrow(dt_auctionNumberIntegers)){
      auctionNumCutLow <- dt_auctionNumberIntegers[iCut]$low
      auctionNumCutHigh <- dt_auctionNumberIntegers[iCut]$high
      dt_start <- dt_cut[AuctionNumberInteger >= auctionNumCutLow &
                           AuctionNumberInteger <= auctionNumCutHigh &
                           AuctionTypeOrder==1]
      l_ttest <- list()
      l_ttest$twoSided <- t.test(pctOfNash ~ AuctionType, data=dt_start)
      l_ttest$dutchLessThan <- t.test(pctOfNash ~ AuctionType, data=dt_start, alternative='less')
      l_ttest$dutchGreaterThan <- t.test(pctOfNash ~ AuctionType, data=dt_start, alternative='greater')
      dt_ttest <- rbindlist(lapply(l_ttest, fun_l_ttest_to_dt), use.names = TRUE, fill=TRUE)  
      dt_ttest$AuctionNumCutLow <- auctionNumCutLow
      dt_ttest$AuctionNumCutHigh <- auctionNumCutHigh
      l_dts[[iCut]] <- dt_ttest
    }
    dt_auction_order <- rbindlist(l_dts, use.names=TRUE, fill=TRUE)
    dt_auction_order$auctionOrder <- auctionOrder
    l_dt_orders[[iAuctionOrder]] <- dt_auction_order
  }
  dt_ttests <- rbindlist(l_dt_orders, use.names=TRUE, fill=TRUE)
  dt_ttests <- dt_ttests[order(test_type)]
  #View(dt_ttests[order(test_type)][test_type %in% c('greater', 'less')])
  
  
  for(sAuctionOrder in unique(dt_ttests$auctionOrder)){
    dt_ttests_auctionOrder <- dt_ttests[auctionOrder %in% sAuctionOrder]
    
    dt_greater <- dt_ttests_auctionOrder[test_type=='greater']
    ppval_greater <- ggplot(dt_greater, aes(x=AuctionNumCutHigh, y=p_value)) + 
      geom_line() +
      geom_hline(yintercept=0.05) +
      ggtitle(paste0('Pvalue for t-test mean Dutch > mean First Price, auction order:', sAuctionOrder))
    plot(ppval_greater)
    ggsave(paste0('~/Dropbox/pkg.data/auction_emotions/Figures/ppval_greater_auctionOrder', sAuctionOrder, '.pdf'),
           plot=ppval_greater, width=10, height=6, units = 'in')
    
    dt_less <-dt_ttests_auctionOrder[test_type=='less']
    dt_less <- dt_less[order(AuctionNumCutHigh)]
    dt_less <- tidyr::gather(dt_less, key=auction_type, value=mean_pct_of_nash, dutch, first_price)
    ppval_less <- ggplot(dt_less, aes(x=AuctionNumCutHigh, y=p_value)) + 
      geom_line() +
      geom_hline(yintercept = 0.05)+
      ggtitle(paste0('Pvalue for t-test mean Dutch < mean First Price, auction order:', sAuctionOrder))
    plot(ppval_less)
    ggsave(paste0('~/Dropbox/pkg.data/auction_emotions/Figures/ppval_less_auctionOrder', sAuctionOrder, '.pdf'),
           plot=ppval_less, width=10, height=6, units = 'in')
    
    dt_two.sided <-dt_ttests_auctionOrder[test_type=='two.sided']
    dt_two.sided <- dt_two.sided[order(AuctionNumCutHigh)]
    dt_two.sided <- tidyr::gather(dt_two.sided, key=auction_type, value=mean_pct_of_nash, dutch, first_price)
    ppval_two.sided <- ggplot(dt_two.sided, aes(x=AuctionNumCutHigh, y=p_value)) + 
      geom_line() +
      ggtitle(paste0('Pvalue for two.sided t-test, auction order:', sAuctionOrder))
    plot(ppval_two.sided)
    ggsave(paste0('~/Dropbox/pkg.data/auction_emotions/Figures/ppval_two_sided_auctionOrder', sAuctionOrder, '.pdf'),
           plot=ppval_two.sided, width=10, height=6, units = 'in')
    
    dt_gather <- tidyr::gather(dt_ttests_auctionOrder,
                               key=auction_type, value=mean_pct_of_nash, dutch, first_price)
    dt_gather <- as.data.table(dt_gather)
    pbidpct <- ggplot(dt_gather, aes(x=AuctionNumCutHigh, y=mean_pct_of_nash, group=auction_type, color=auction_type)) +
      geom_line() +
      ggtitle(paste0('Mean pct of nash bid, Auction Order:', sAuctionOrder))
    plot(pbidpct)
    ggsave(paste0('~/Dropbox/pkg.data/auction_emotions/Figures/meanPctOfnashBid_auctionOrder', sAuctionOrder, '.pdf'),
           plot=pbidpct, width=10, height=6, units = 'in')
    
  }
  dt <- dt[, AuctionClass := interaction(AuctionTypeOrder, AuctionType)]
  
  # Plot bid amounts by auction number
  dt_means <- dt[, meanBidActual:=mean(BidActual), by=c('AuctionClass', 'AuctionNumber')]
  dt_means <- dt_means[, sdBidActual:=sd(BidActual), by=c('AuctionClass', 'AuctionNumber')]
  dt_means <- dt_means[, conf_above := meanBidActual + 2*sdBidActual]
  dt_means <- dt_means[, conf_below := meanBidActual - 2*sdBidActual]
  
  dt_means <- dt_means[, pctOfNash := BidActual/BidNash]
  dt_means <- dt_means[pctOfNash < 2]
  dt_means <- dt_means[, meanPctOfNash := mean(pctOfNash), by=c('AuctionClass', 'AuctionNumber')]
  dt_means <- dt_means[, sdPctOfNash := sd(pctOfNash), by=c('AuctionClass', 'AuctionNumber')]
  dt_means <- dt_means[, confPctOfNash_above := meanPctOfNash + 2*sdPctOfNash]
  dt_means <- dt_means[, confPctOfNash_below := meanPctOfNash - 2*sdPctOfNash]
  
  dt_start <- dt_means[as.integer(AuctionNumber)<100]
  p1 <- ggplot(dt_start[AuctionType=='first_price'], aes(x=as.integer(AuctionNumber), y=meanPctOfNash, ymax=0.7, ymin=1.7, group=AuctionClass, color=AuctionClass)) +
    #geom_line() + 
    geom_ribbon(aes(ymin=confPctOfNash_below, ymax=confPctOfNash_above, group=AuctionClass, fill=AuctionClass), alpha = 0.1)
  ggsave('~/Dropbox/pkg.data/auction_emotions/Figures/Auction_num_first_price_pctOfNash.pdf', plot=p1,width=10, height=6, units = 'in')
  
  p2 <- ggplot(dt_start[AuctionType=='dutch'], aes(x=as.integer(AuctionNumber), y=meanPctOfNash, ymax=0.7, ymin=1.7, group=AuctionClass, color=AuctionClass)) +
    #geom_line() + 
    geom_ribbon(aes(ymin=confPctOfNash_below, ymax=confPctOfNash_above, group=AuctionClass, fill=AuctionClass), alpha = 0.1)
  ggsave('~/Dropbox/pkg.data/auction_emotions/Figures/Auction_num_dutch_pctOfNash.pdf', plot=p2,width=10, height=6, units = 'in')
  
  p3 <- ggplot(dt_start[AuctionType=='first_price'], aes(x=as.integer(AuctionNumber), y=sdPctOfNash, group=AuctionClass, color=AuctionClass)) +
    geom_line()
  ggsave('~/Dropbox/pkg.data/auction_emotions/Figures/Auction_num_first_price_SDpctOfNash.pdf', plot=p3,width=10, height=6, units = 'in')
  
  p4 <- ggplot(dt_start[AuctionType=='dutch'], aes(x=as.integer(AuctionNumber), y=sdPctOfNash, group=AuctionClass, color=AuctionClass)) +
    geom_line()
  ggsave('~/Dropbox/pkg.data/auction_emotions/Figures/Auction_num_dutch_SDpctOfNash.pdf', plot=p4,width=10, height=6, units = 'in')
  
}

