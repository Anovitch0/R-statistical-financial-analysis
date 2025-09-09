# R/50_portfolio.R — HRP (corr) + HRP (glasso)
suppressPackageStartupMessages({library(data.table); library(PerformanceAnalytics)})

partial_cor <- function(P){
  D <- diag(1/sqrt(diag(P)))
  R <- - D %*% P %*% D
  diag(R) <- 1
  R
}

hrp_alloc_core <- function(cov_mat, corr_mat){
  dist <- as.dist(sqrt(0.5 * (1 - corr_mat)))
  hc   <- hclust(dist, method = "ward.D2")
  ord  <- hc$order
  V    <- cov_mat[ord, ord, drop = FALSE]

  get_cluster_var <- function(cov, w) as.numeric(t(w) %*% cov %*% w)
  bisect <- function(cov){
    n <- nrow(cov); if (n == 1) return(1)
    k  <- floor(n/2)
    c1 <- cov[1:k, 1:k, drop = FALSE]
    c2 <- cov[(k+1):n, (k+1):n, drop = FALSE]
    w1 <- bisect(c1); w2 <- bisect(c2)
    v1 <- get_cluster_var(c1, w1); v2 <- get_cluster_var(c2, w2)
    a  <- 1 - v1/(v1 + v2)
    c(a*w1, (1-a)*w2)
  }
  w_ord <- bisect(V)
  w <- rep(NA_real_, length(ord)); w[ord] <- w_ord
  names(w) <- colnames(cov_mat)
  w / sum(w)
}

hrp_alloc_glasso <- function(C, P){
  R <- abs(partial_cor(P))
  hrp_alloc_core(C, R)
}
