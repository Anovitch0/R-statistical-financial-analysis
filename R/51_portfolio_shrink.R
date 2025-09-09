# R/51_portfolio_shrink.R — HRP + shrinkage alpha
shrink_cov <- function(C, alpha = 0){
  stopifnot(alpha >= 0, alpha <= 0.8)
  (1 - alpha) * C + alpha * diag(diag(C))
}

hrp_alloc_shrunk <- function(C, alpha = 0){
  C2 <- shrink_cov(C, alpha)
  R  <- cov2cor(C2)
  hrp_alloc_core(C2, R)
}
