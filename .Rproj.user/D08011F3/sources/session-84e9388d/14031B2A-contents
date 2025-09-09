# R/10_ingest.R — Yahoo & FRED -> Parquet
suppressPackageStartupMessages({library(quantmod); library(data.table); library(xts); library(zoo)})

ingest_prices <- function(cfg){
  syms <- unlist(cfg$tickers)
  start <- as.Date(cfg$dates$start); end <- if (is.null(cfg$dates$end)) Sys.Date() else as.Date(cfg$dates$end)
  out <- list()
  for (s in syms){
    cat("Téléchargement:", s, "\n")
    xt <- tryCatch(getSymbols(s, src="yahoo", from=start, to=end, auto.assign=FALSE), error=function(e) NULL)
    if (!is.null(xt)){
      dt <- data.table(date = as.Date(index(xt)), coredata(xt))
      setnames(dt, old = names(dt), new = gsub(paste0("^", s, "\\."), "", names(dt)))
      dt[, symbol := s]
      out[[s]] <- dt
    }
  }
  prices <- rbindlist(out, use.names=TRUE, fill=TRUE)
  prices[, ret_d := log(Adjusted/shift(Adjusted)), by = symbol]
  save_parquet_dt(prices, "data/processed/prices.parquet")
}

ingest_macro <- function(cfg){
  fred_ids <- unlist(cfg$macro_fred)
  env <- new.env()
  getSymbols(fred_ids, src="FRED", env = env, auto.assign = TRUE)
  lst <- lapply(fred_ids, function(id){
    xt <- get(id, envir = env)
    data.table(date = as.Date(index(xt)), value = as.numeric(xt), series = id)
  })
  dt <- rbindlist(lst)
  save_parquet_dt(dt, "data/processed/macro.parquet")
}
