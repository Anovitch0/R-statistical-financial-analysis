# _targets.R — pipeline {targets} (pro)
library(targets); library(tarchetypes)
source("R/00_utils.R")
source("R/10_ingest.R")
source("R/20_features.R")
source("R/30_dependence.R")
source("R/31_glasso_cv.R")
source("R/40_risk_models.R")
source("R/42_var_es_backtests.R")
source("R/50_portfolio.R")
source("R/51_portfolio_shrink.R")
source("R/52_portfolio_oos.R")
source("R/60_change_point.R")

tar_option_set(
  packages = c(
    "data.table","arrow","yaml","quantmod","xts","zoo","PerformanceAnalytics",
    "ggplot2","patchwork","glasso","rmgarch","riskParityPortfolio",
    "igraph","ggraph","changepoint","strucchange"
  )
)

list(
  tar_target(cfg,        read_config("config.yml")),
  tar_target(prices_raw, ingest_prices(cfg), format = "file"),
  tar_target(macro_raw,  ingest_macro(cfg),  format = "file"),
  tar_target(prices_dt,  load_parquet_dt(prices_raw)),
  tar_target(macro_dt,   load_parquet_dt(macro_raw)),
  tar_target(feats,      build_features(prices_dt)),
  tar_target(dep_cv,     glasso_cv(feats, rhos = cfg$glasso$rhos)),
  tar_target(risk,       risk_models(feats)),
  tar_target(backtests,  var_es_backtests(prices_dt, level = 0.975, window = 252)),
  tar_target(ptf_shrunk, hrp_alloc_shrunk(dep_cv$cov, alpha = cfg$hrp$alpha)),
  tar_target(ptf_glasso, hrp_alloc_glasso(dep_cv$cov, P = dep_cv$precision)),
  tar_target(oos,        oos_backtest(prices_dt, method = "HRP_shrunk",
                                      window = cfg$oos$window, alpha = cfg$hrp$alpha,
                                      costs_bps = cfg$oos$costs_bps))
)
