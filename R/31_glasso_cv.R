# R/31_glasso_cv.R — sélection de rho par CV (log-vraisemblance)
suppressPackageStartupMessages({library(data.table); library(glasso)})

loglik_gaussian <- function(P, S){
  ld <- determinant(P, logarithm = TRUE)$modulus[1]
  as.numeric(ld - sum(S * P))
}

glasso_cv <- function(feats, rhos = c(0.005, 0.01, 0.02, 0.03)){
  dt <- data.table::copy(feats)[, .(date, symbol, ret_d)]
  wide <- data.table::dcast(dt, date ~ symbol, value.var = "ret_d")
  wide <- na.omit(wide)
  M <- as.matrix(wide[, -1, with = FALSE])
  syms <- colnames(M)

  n <- nrow(M); n_tr <- floor(0.7 * n)
  M_tr <- M[1:n_tr, , drop=FALSE]; M_val <- M[(n_tr+1):n, , drop=FALSE]
  S_tr <- stats::cor(M_tr, use = "pairwise.complete.obs")
  S_va <- stats::cor(M_val, use = "pairwise.complete.obs")

  scores <- data.table(rho = rhos, score = NA_real_)
  best <- -Inf; bestP <- NULL; best_rho <- rhos[1]
  for (i in seq_along(rhos)){
    fit <- glasso::glasso(S_tr, rho = rhos[i])
    P <- fit$wi
    sc <- loglik_gaussian(P, S_va)
    scores$score[i] <- sc
    if (sc > best){ best <- sc; bestP <- P; best_rho <- rhos[i] }
  }
  S_full <- stats::cor(M, use = "pairwise.complete.obs")
  fit_full <- glasso::glasso(S_full, rho = best_rho)
  P_full <- fit_full$wi
  C_full <- stats::cov(M, use = "pairwise.complete.obs") * 252
  dimnames(P_full) <- list(syms, syms); dimnames(C_full) <- list(syms, syms)
  list(rho = best_rho, scores = scores, precision = P_full, cov = C_full, cor = S_full)
}
