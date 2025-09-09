# R/52_portfolio_oos.R — OOS backtest (rolling window + coûts)
suppressPackageStartupMessages({library(data.table); library(xts); library(PerformanceAnalytics); library(glasso)})

oos_backtest <- function(prices_dt, method = c("HRP_shrunk","HRP_glasso"),
                         window = 756, alpha = 0.15, rho = 0.02, costs_bps = 5){
  method <- match.arg(method)
  dt <- data.table::copy(prices_dt)[, .(date, symbol, ret = ret_d)]
  wide <- data.table::dcast(dt, date ~ symbol, value.var = "ret")
  wide <- na.omit(wide)
  dates <- wide$date; Rm <- as.matrix(wide[,-1,with=FALSE])
  p <- ncol(Rm)

  X <- xts::xts(Rm, dates)
  ep <- xts::endpoints(X, on = "months")
  rebd_idx <- ep[ep > 0]
  rebd_dates <- zoo::index(X)[rebd_idx]
  rebd_dates <- rebd_dates[rebd_dates > dates[window]]

  W_all <- matrix(NA_real_, nrow = length(rebd_dates), ncol = p); colnames(W_all) <- colnames(Rm)
  prev_w <- rep(0, p); k <- 0
  costs_vec <- numeric(length(rebd_dates))

  for (d in rebd_dates){
    k <- k + 1
    idx <- which(dates <= d)
    if (length(idx) < window) next
    win_idx <- (idx[length(idx)]-window+1):idx[length(idx)]
    M <- Rm[win_idx, , drop=FALSE]
    C <- cov(M, use="pairwise.complete.obs") * 252

    if (method == "HRP_shrunk"){
      w <- hrp_alloc_shrunk(C, alpha = alpha)
    } else {
      S <- cor(M, use="pairwise.complete.obs")
      P <- glasso::glasso(S, rho = rho)$wi
      w <- hrp_alloc_glasso(C, P = P)
    }
    cost <- (costs_bps/10000) * sum(abs(w - prev_w))
    costs_vec[k] <- cost
    W_all[k, ] <- w
    prev_w <- w
  }
  # 1) retours quotidiens complets (et pas uniquement aux dates de rebal)
  R_xts <- X  # toute la série quotidienne
  
  # 2) les poids sous forme d'un xts aligné sur les dates de rebalancement
  W_xts <- xts::xts(W_all, order.by = rebd_dates)
  
  # 3) portefeuille : utilisent des poids datés -> Return.portfolio applique jusqu'au prochain signal
  port <- PerformanceAnalytics::Return.portfolio(R = R_xts, weights = W_xts, verbose = FALSE)
  
  # 4) coûts : série xts avec zéros et coûts aux dates de rebal
  costs_xts <- xts::xts(costs_vec, order.by = rebd_dates)
  # étend à la fréquence quotidienne en remplissant les jours sans coût à 0
  all_idx <- zoo::index(R_xts)
  costs_full <- xts::xts(rep(0, length(all_idx)), order.by = all_idx)
  costs_full[rebd_dates] <- as.numeric(costs_xts)
  
  # 5) on retire les coûts (mêmes dates/indice que port)
  port_adj <- port - costs_full
  colnames(port_adj) <- "OOS"
  
  list(rebalance_dates = rebd_dates, weights = W_all, returns = port_adj, costs = costs_xts)
}
