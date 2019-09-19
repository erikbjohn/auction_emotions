analysis_na <- function(){
  analysis_na_location <- '~/Dropbox/pkg.data/auction_emotions/Clean/analysis_na.rds'
  if(!file.exists(analysis_na_location)){
    dt_scores_location <- '~/Dropbox/pkg.data/auction_emotions/Clean/dt_scores.rds'
    dt_scores <- readRDS(dt_scores_location)
    na_scores <- dt_scores[, .(participant_id, Score, Score_num)]
    na_scores <- na_scores[, n_tics:=.N, by=participant_id]
    na_scores <- na_scores[is.na(Score_num)]
    na_scores <- na_scores[, n_na:=.N, by=participant_id]
    na_scores <- na_scores[, n_score:=.N, by=.(participant_id, Score)]
    na_scores <- unique(na_scores[, .(participant_id, Score, Score_num, n_tics, n_na, n_score)])
    na_scores <- na_scores[!is.na(Score)]
    cast_scores <- dcast(na_scores, participant_id + n_tics + n_na ~ Score, value.var='n_score')
    cast_scores <- cast_scores[, n_tics := n_tics-`No Stimulus`]
    cast_scores <- cast_scores[, n_na := n_na - `No Stimulus`]
    cast_scores <- cast_scores[, share_na:= n_na/n_tics]
    cast_scores <- cast_scores[, share_na_find_failed:=FIND_FAILED/n_na]
    cast_scores <- cast_scores[, share_na_fit_failed:=FIT_FAILED/n_na]
    cast_scores$`No Stimulus` <- NULL
    saveRDS(cast_scores, file=analysis_na_location)
  } else {
    dt <- readRDS(analysis_na_location)
  }
  return(dt)
}