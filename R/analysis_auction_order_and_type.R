analysis_auction_order_and_type <- function(){
  analysis_auction_order_and_type_location <- '~/Dropbox/pkg.data/auction_emotions/Clean/analysis_auction_order_and_type.rds'
  if(!(file.exists(analysis_auction_order_and_type_location))){
    
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
    
    # dt comes from payoffs summarize but need to double check. This is the data used for this analysis
    dt <- readRDS('~/Dropbox/pkg.data/auction_emotions/Clean/dt_analysis_auction_order_and_type.rds')
    dt <- dt[ , AuctionNumberInteger:=as.integer(AuctionNumber)]
    dt <- dt[ ,pctOfNash:=BidActual/BidNash]
    dt <- dt[!is.na(AuctionTypeOrder)]
    dt <- dt[, BidRational:=BidActual<((4/3)*BidNash)]
    dt <- dt[which(BidRational)]
    dt <- dt[!is.na(AuctionNumberInteger)]
    
    dt_auctionNumberIntegers <- data.table(high=sort(unique(dt$AuctionNumberInteger)))
    dt_auctionNumberIntegers <- dt_auctionNumberIntegers[, low:=high-1]
    dt_auctionNumberIntegers <- dt_auctionNumberIntegers[low>0]
    intLower_low <- 0
    intLower_high <- 8
    intMid_low <- 9
    intMid_high <- 16
    intUpper_low <- 17
    intUpper_high <- 25
    dt_auctionNumberIntegers <- data.table(low=c(intLower_low, intMid_low, intUpper_low),
                                           high=c(intLower_high, intMid_high, intUpper_high))
    
    dt <- dt[AuctionNumberInteger >= dt_auctionNumberIntegers[1]$low & AuctionNumberInteger <= dt_auctionNumberIntegers[1]$high,
             number_cat := 'first']
    dt <- dt[AuctionNumberInteger >= dt_auctionNumberIntegers[2]$low & AuctionNumberInteger <= dt_auctionNumberIntegers[2]$high,
             number_cat := 'amiddle']
    dt <- dt[AuctionNumberInteger >= dt_auctionNumberIntegers[3]$low & AuctionNumberInteger <= dt_auctionNumberIntegers[3]$high,
             number_cat := 'last']
    dt$last_order_one_type <- 'aOmit'
    dt <- dt[AuctionTypeOrder=='1' & number_cat == 'last' & AuctionType == 'dutch', last_order_one_type :=  'dutch']
    dt <- dt[AuctionTypeOrder=='1' & number_cat == 'last' & AuctionType == 'first_price', last_order_one_type := 'first_price']
    l_dt_orders <- vector(mode = 'list', length=length(unique(dt$AuctionTypeOrder)))
    #quants <- quantile(dt$pctOfNash, c(0.025, 0.975))
    for(iAuctionOrder in 1:length(unique(dt$AuctionTypeOrder))){
      auctionOrder <- unique(dt$AuctionTypeOrder)[iAuctionOrder]
      dt_order <- dt[AuctionTypeOrder %in% auctionOrder]
      
      # 95% of Nash quantiles
      #dt_cut <- dt[pctOfNash > quants[1] & pctOfNash < quants[2]]
      #dt_cut <- dt_cut[!is.na(AuctionNumberInteger)]
      # l_ttest <- vector(mode='list', length=length(AuctionNumberIntegers))
      
      l_dts <- vector(mode='list', length=nrow(dt_auctionNumberIntegers))
      for(iCut in 1:nrow(dt_auctionNumberIntegers)){
        auctionNumCutLow <- dt_auctionNumberIntegers[iCut]$low
        auctionNumCutHi <- dt_auctionNumberIntegers[iCut]$high
        
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
          dtgh <- dt_auctionNumberIntegers[iCut]$high
          dt_start <- dt_order[AuctionNumberInteger >= auctionNumCutLow &
                                 AuctionNumberInteger <= auctionNumCutHigh &
                                 AuctionTypeOrder %in% auctionOrder]
          l_ttest <- list()
          l_ttest$twoSided <- t.test(pctOfNash ~ AuctionType, data=dt_start)
          l_ttest$dutchLessThan <- t.test(pctOfNash ~ AuctionType, data=dt_start, alternative='less')
          l_ttest$dutchGreaterThan <- t.test(pctOfNash ~ AuctionType, data=dt_start, alternative='greater')
          dt_ttest <- rbindlist(lapply(l_ttest, fun_l_ttest_to_dt), use.names = TRUE, fill=TRUE)  
          dt_ttest$AuctionNumCutLow <- auctionNumCutLow
          dt_ttest$AuctionNumCutHigh <- auctionNumCutHigh
          dt_ttest$test_crit <- 'AuctionType'
          l_dts[[iCut]] <- dt_ttest
        }
        dt_auction_order <- rbindlist(l_dts, use.names=TRUE, fill=TRUE)
      dt_auction_order$auctionOrder <- auctionOrder
      l_dt_orders[[iAuctionOrder]] <- dt_auction_order
    }
    dt_ttests <- rbindlist(l_dt_orders, use.names=TRUE, fill=TRUE)
    dt_ttests <- dt_ttests[order(test_type)]
    
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
    
    ## Auction Type Order analysis
    
    fun_l_ttest_type_to_dt <- function(l_in){
      dt_ttest <- data.table(t_stat=l_in$statistic)
      dt_ttest$df <- l_in$parameter
      dt_ttest$p_value <- l_in$p.value
      dt_ttest$conf_95_low <- l_in$conf.int[1]
      dt_ttest$conf_95_hi <- l_in$conf.int[2]
      dt_ttest$auctionOrder_1 <- l_in$estimate[1]
      dt_ttest$auctionOrder_2 <- l_in$estimate[2]
      dt_ttest$null_value <- l_in$null.value
      dt_ttest$test_type <- l_in$alternative
      return(dt_ttest)
    }
    
    auctionTypes <- unique(dt$AuctionType)
    l_dt_AuctionTypes <- vector(mode='list', length=length(auctionTypes))
    for(iAuctionType in 1:length(auctionTypes)){
      sAuctionType <- auctionTypes[iAuctionType]
      dt_type <- dt[AuctionType %in% sAuctionType]
      l_dts_type <- vector(mode='list', length=nrow(dt_auctionNumberIntegers))
      for(iCut in 1:nrow(dt_auctionNumberIntegers)){
        auctionNumCutLow <- dt_auctionNumberIntegers[iCut]$low
        auctionNumCutHigh <- dt_auctionNumberIntegers[iCut]$high
        dt_type_start <- dt_type[AuctionNumberInteger >= auctionNumCutLow &
                                   AuctionNumberInteger <= auctionNumCutHigh]
        l_ttest_type <- list()
        l_ttest_type$twoSided <- t.test(pctOfNash ~ AuctionTypeOrder, data=dt_type_start)
        l_ttest_type$dutchLessThan <- t.test(pctOfNash ~ AuctionTypeOrder, data=dt_type_start, alternative='less')
        l_ttest_type$dutchGreaterThan <- t.test(pctOfNash ~ AuctionTypeOrder, data=dt_type_start, alternative='greater')
        dt_ttest_type <- rbindlist(lapply(l_ttest_type, fun_l_ttest_type_to_dt), use.names = TRUE, fill=TRUE)  
        dt_ttest_type$AuctionNumCutLow <- auctionNumCutLow
        dt_ttest_type$AuctionNumCutHigh <- auctionNumCutHigh
        dt_ttest_type$test_crit <- 'AuctionTypeOrder'
        l_dts_type[[iCut]] <- dt_ttest_type
      }
      dt_auction_order <- rbindlist(l_dts_type, use.names=TRUE, fill=TRUE)
      dt_auction_order$auctionType <- sAuctionType
      l_dt_AuctionTypes[[iAuctionType]] <- dt_auction_order
    }
    dt_ttests_type <- rbindlist(l_dt_AuctionTypes, use.names=TRUE, fill=TRUE)
    dt_ttests_type <- dt_ttests_type[order(test_type)]
    View(dt_ttests_type)
    
    #View(dt_ttests[order(test_type)][test_type %in% c('greater', 'less')])
    lowCut <- min(dt_ttests$AuctionNumCutLow)
    highCut <- max(dt_ttests$AuctionNumCutHigh)
    midCut <- dt_auctionNumberIntegers[2]$low
    
    dt_print_greater_auctionType <- dt_ttests[test_type %in% 'two.sided' & AuctionNumCutLow == lowCut,.(auctionOrder,
                                                                                                        dutch, first_price, p_value)][order(as.integer(auctionOrder))]
    dt_print_greater_auctionOrder <- dt_ttests_type[test_type %in% 'two.sided'& AuctionNumCutLow == lowCut, .(auctionType, p_value)]
    dt_print_greater_bind <- data.table(auctionOrder='p_value',
                                        dutch=dt_print_greater_auctionOrder[auctionType=='dutch']$p_value,
                                        first_price=dt_print_greater_auctionOrder[auctionType=='first_price']$p_value,
                                        p_value=NA)
    dt_print_first_8 <- rbindlist(list(dt_print_greater_auctionType, dt_print_greater_bind), use.names = TRUE, fill=TRUE)
    
    print(dt_print_first_8)
    
    dt_print_mid <- dt_ttests[test_type %in% 'two.sided' & AuctionNumCutLow == midCut,.(auctionOrder=as.integer(auctionOrder),
                                                                                        dutch, first_price, p_value = p_value)][order(auctionOrder)]
    dt_print_mid_auctionOrder <- dt_ttests_type[test_type %in% 'two.sided'& AuctionNumCutLow == midCut, .(auctionType, p_value)]
    dt_print_mid_bind <- data.table(auctionOrder='p_value',
                                    dutch=dt_print_mid_auctionOrder[auctionType=='dutch']$p_value,
                                    first_price=dt_print_mid_auctionOrder[auctionType=='first_price']$p_value,
                                    p_value=NA)
    dt_print_mid_8 <- rbindlist(list(dt_print_mid, dt_print_mid_bind), use.names = TRUE, fill=TRUE)
    
    print(dt_print_mid_8)
    
    dt_print_last <- dt_ttests[test_type %in% 'less' & AuctionNumCutHigh== highCut,.(auctionOrder=as.integer(auctionOrder),
                                                                                     dutch, first_price, p_value)][order(auctionOrder)]
    dt_print_last_auctionOrder <- dt_ttests_type[test_type %in% 'two.sided'& AuctionNumCutHigh == highCut, .(auctionType, p_value)]
    dt_print_last_bind <- data.table(auctionOrder='p_value',
                                     dutch=dt_print_last_auctionOrder[auctionType=='dutch']$p_value,
                                     first_price=dt_print_last_auctionOrder[auctionType=='first_price']$p_value,
                                     p_value=NA)
    dt_print_last_9 <- rbindlist(list(dt_print_last, dt_print_last_bind), use.names = TRUE, fill=TRUE)
    
    print(dt_print_last_9)
    
    ## AuctionType analysis
    l_auctionType <- t.test(pctOfNash ~ AuctionType, data=dt)
    dt_ttest_base_type<- data.table(t_stat=l_auctionType$statistic)
    dt_ttest_base_type$df <- l_auctionType$parameter
    dt_ttest_base_type$p_value <- l_auctionType$p.value
    dt_ttest_base_type$conf_95_low <- l_auctionType$conf.int[1]
    dt_ttest_base_type$conf_95_hi <- l_auctionType$conf.int[2]
    dt_ttest_base_type$dutch <- l_auctionType$estimate[1]
    dt_ttest_base_type$first_price <- l_auctionType$estimate[2]
    dt_ttest_base_type$null_value <- l_auctionType$null.value
    dt_ttest_base_type$test_type <- l_auctionType$alternative
    dt_print_base_type <- dt_ttest_base_type[, .(dutch, first_price, t_stat, p_value)]
    
    ## AuctionOrder analysis
    l_auctionOrder <- t.test(pctOfNash ~ AuctionTypeOrder, data=dt)
    dt_ttest_base_order<- data.table(t_stat=l_auctionOrder$statistic)
    dt_ttest_base_order$df <- l_auctionOrder$parameter
    dt_ttest_base_order$p_value <- l_auctionOrder$p.value
    dt_ttest_base_order$conf_95_low <- l_auctionOrder$conf.int[1]
    dt_ttest_base_order$conf_95_hi <- l_auctionOrder$conf.int[2]
    dt_ttest_base_order$auctionOrder_1 <- l_auctionOrder$estimate[1]
    dt_ttest_base_order$auctionOrder_2 <- l_auctionOrder$estimate[2]
    dt_ttest_base_order$null_value <- l_auctionOrder$null.value
    dt_ttest_base_order$test_order <- l_auctionOrder$alternative
    dt_print_base_order <- dt_ttest_base_order[, .(auctionOrder_1, auctionOrder_2,t_stat, p_value)]
    
    ## Regression Analysis
    
    ##  Auction 0rder 1
    #summary(lm(pctOfNash ~ AuctionType * number_cat , data=dt[AuctionTypeOrder %in% '1']))
    
    
    ## first 8 auctions
    #summary(lm(pctOfNash ~ AuctionType * AuctionTypeOrder , data=dt[number_cat %in% 'first']))
    
    
    l_reg <- list()
    l_reg$Type <- lm(pctOfNash ~ AuctionType, data=dt)
    l_reg$TypeNum <- lm(pctOfNash ~ AuctionType + AuctionNumberInteger, data = dt)
    l_reg$TypeNumOrder <- lm(pctOfNash ~ AuctionType + AuctionNumberInteger + AuctionTypeOrder, data=dt)
    l_reg$TypeOrder <- lm(pctOfNash ~ AuctionType + AuctionTypeOrder, data = dt)
    l_reg$TypeOrderNum <- lm(pctOfNash ~ AuctionType + AuctionTypeOrder +
                               AuctionNumberInteger * AuctionType +
                               AuctionNumberInteger * AuctionTypeOrder +
                               AuctionType * AuctionTypeOrder, data=dt)
    l_reg$LastOrder <- lm(pctOfNash ~ 
                                  AuctionType + AuctionTypeOrder + last_order_one_type,
                                data=dt)
    #l_reg$TypeOrderNum <- lm(pctOfNash ~ AuctionType + AuctionTypeOrder + AuctionNumberInteger * AuctionType +
    #                           AuctionNumberInteger * AuctionTypeOrder , data=dt)
    lapply(l_reg, summary)
    
    l_reg_learn <- list()
    l_reg_learn$base <- lm(pctOfNash ~ 
                             last_order_one_type,
                           data=dt)
    l_reg_learn$type <- lm(pctOfNash ~
                             AuctionType + last_order_one_type,
                           data=dt)
    l_reg_learn$typeOrder <- lm(pctOfNash ~ 
                                  AuctionType + AuctionTypeOrder + last_order_one_type,
                                data=dt)
    lapply(l_reg_learn, summary)
    
    l_analysis <- list(first =dt_print_first_8,
                       mid = dt_print_mid_8,
                       last = dt_print_last_9,
                       auction_type = dt_print_base_type,
                       auction_order = dt_print_base_order,
                       regression_pctOfNash = l_reg)
    saveRDS(l_analysis, file=analysis_auction_order_and_type_location)
  } else {
    l_analysis <- readRDS(analysis_auction_order_and_type_location)
  }
  return(l_analysis)
}
