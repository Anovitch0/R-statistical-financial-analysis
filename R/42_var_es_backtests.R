# R/42_var_es_backtests.R — backtests (Kupiec/Christoffersen) pour VaR 97.5%
suppressPackageStartupMessages({library(data.table)})

lr_uc <- function(hits, alpha){
  T <- length(hits); x <- sum(hits, na.rm=TRUE)
  phat <- x / T
  LRuc <- -2 * ( (T - x)*log(1 - alpha) + x*log(alpha) - (T - x)*log(1 - phat) - x*log(phat) )
  pval <- 1 - pchisq(LRuc, df = 1)
  list(stat = LRuc, p.value = pval, x = x, T=T, phat = phat)
}

lr_ind <- function(hits){
  h <- as.integer(hits)
  n00 <- sum(h[-length(h)]==0 & h[-1]==0, na.rm=TRUE)
  n01 <- sum(h[-length(h)]==0 & h[-1]==1, na.rm=TRUE)
  n10 <- sum(h[-length(h)]==1 & h[-1]==0, na.rm=TRUE)
  n11 <- sum(h[-length(h)]==1 & h[-1]==1, na.rm=TRUE)
  p01 <- ifelse(n00+n01>0, n01/(n00+n01), 0)
  p11 <- ifelse(n10+n11>0, n11/(n10+n11), 0)
  p1  <- (n01+n11)/(n00+n01+n10+n11)
  L1 <- ((1-p01)^(n00))*(p01^(n01))*((1-p11)^(n10))*(p11^(n11))
  L0 <- ((1-p1)^(n00+n10))*(p1^(n01+n11))
  LRind <- -2*log(L0/L1)
  pval <- 1 - pchisq(LRind, df = 1)
  list(stat = LRind, p.value = pval)
}

var_es_backtests <- function(prices_dt, level = 0.975, window = 252){
  dt <- data.table::copy(prices_dt)[symbol=="SPY", .(date, ret = ret_d)][!is.na(ret)]
  alpha <- 1 - level
  q_roll <- zoo::rollapplyr(dt$ret, width = window, FUN = function(x) quantile(x, probs = alpha, na.rm=TRUE), fill = NA)
  hits <- dt$ret < q_roll
  out <- list(
    level = level,
    window = window,
    LRuc = lr_uc(hits, alpha = alpha),
    LRind = lr_ind(hits),
    LRcc = NULL
  )
  out$LRcc <- list(stat = out$LRuc$stat + out$LRind$stat,
                   p.value = 1 - pchisq(out$LRuc$stat + out$LRind$stat, df = 2))
  tab <- data.table::data.table(
    metric = c("Violations","Rate","LRuc_p","LRind_p","LRcc_p"),
    value  = c(out$LRuc$x, out$LRuc$phat, out$LRuc$p.value, out$LRind$p.value, out$LRcc$p.value)
  )
  
  ensure_dir("tables")  # <-- crée le dossier si besoin
  data.table::fwrite(tab, "tables/var_backtests_spy.csv")
  
  out
}
