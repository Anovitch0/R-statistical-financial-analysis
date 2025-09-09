# R/40_risk_models.R — DCC/ES (simple)
suppressPackageStartupMessages({library(data.table); library(PerformanceAnalytics); library(rugarch); library(rmgarch)})

risk_models <- function(feats){
  dt <- data.table::copy(feats)[, .(date, symbol, ret_d)]
  top <- dt[, .N, by=symbol][order(-N)][1:6, symbol]
  W <- data.table::dcast(dt[symbol %in% top], date ~ symbol, value.var="ret_d")
  W <- na.omit(W); X <- as.matrix(W[, -1, with=FALSE])
  ug <- ugarchspec(mean.model=list(armaOrder=c(0,0)), variance.model=list(model="sGARCH", garchOrder=c(1,1)), distribution.model="std")
  mspec <- multispec(replicate(ncol(X), ug))
  dcc <- dccspec(uspec = mspec, dccOrder = c(1,1), distribution = "mvt")
  fit <- tryCatch(dccfit(dcc, data = X), error=function(e) NULL)
  ES <- apply(X, 2, function(x) PerformanceAnalytics::ES(x, p=0.975, method="historical"))
  list(dcc_fit = fit, es = ES, universe = colnames(X))
}
