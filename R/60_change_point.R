# R/60_change_point.R
suppressPackageStartupMessages({library(data.table); library(changepoint)})
change_point_labels <- function(feats){
  dt <- data.table::copy(feats)[symbol=="SPY", .(date, ret_d)]
  x <- dt$ret_d
  cp <- cpt.meanvar(na.omit(x), method = "PELT", penalty = "MBIC")
  idx <- cpts(cp)
  labs <- rep("Regime 1", nrow(dt))
  for (i in seq_along(idx)) labs[(idx[i]+1):length(labs)] <- paste0("Regime ", i+1)
  data.table(date = dt$date, regime = labs)
}
