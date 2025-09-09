test_that("hrp_alloc_shrunk returns valid weights", {
  C <- matrix(c(0.1,0.02,0.02,0.2),2,2); colnames(C)<-rownames(C)<-c("A","B")
  source("R/51_portfolio_shrink.R")
  source("R/50_portfolio.R")
  w <- hrp_alloc_shrunk(C, alpha = 0.1)
  expect_equal(sum(w), 1, tolerance = 1e-8)
  expect_true(all(w >= 0))
})
