# R/20_features.R — features multi-actifs
suppressPackageStartupMessages({library(data.table); library(TTR); library(PerformanceAnalytics)})

build_features <- function(prices_dt){
  setDT(prices_dt); setorder(prices_dt, symbol, date)
  prices_dt[, `:=`(
    mom_21 = TTR::ROC(Adjusted, n = 21, type="continuous"),
    mom_63 = TTR::ROC(Adjusted, n = 63, type="continuous"),
    vol_21 = runSD(ret_d, n = 21)*sqrt(252),
    vol_63 = runSD(ret_d, n = 63)*sqrt(252),
    rsi_14  = TTR::RSI(Adjusted, n=14),
    skew_63 = frollapply(ret_d, 63, PerformanceAnalytics::skewness, align="right"),
    kurt_63 = frollapply(ret_d, 63, PerformanceAnalytics::kurtosis, align="right")
  ), by = symbol]
  prices_dt
}
