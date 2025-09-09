# R/30_dependence.R — corr & glasso (rho fixe)
suppressPackageStartupMessages({library(data.table); library(glasso)})

dependence_objects <- function(feats, rho = 0.01){
  dt <- data.table::copy(feats)[, .(date, symbol, ret_d)]
  wide <- data.table::dcast(dt, date ~ symbol, value.var = "ret_d")
  wide <- na.omit(wide)
  M <- as.matrix(wide[, -1, with = FALSE])
  syms <- colnames(M)

  C <- stats::cov(M, use = "pairwise.complete.obs") * 252
  S <- stats::cor(M, use = "pairwise.complete.obs")
  dimnames(C) <- list(syms, syms)
  dimnames(S) <- list(syms, syms)

  gl <- glasso::glasso(S, rho = rho)
  P  <- gl$wi
  dimnames(P) <- list(syms, syms)

  list(cov = C, cor = S, precision = P, S = S)
}
