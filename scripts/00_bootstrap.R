# scripts/00_bootstrap.R — install & load
options(stringsAsFactors = FALSE, scipen = 999)
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))

pkgs <- c(
  "data.table","arrow","yaml","quantmod","TTR","xts","zoo","PerformanceAnalytics",
  "targets","tarchetypes","ggplot2","patchwork","ggraph","igraph","pheatmap","DT",
  "glasso","copula","rugarch","rmgarch","riskParityPortfolio",
  "changepoint","strucchange","quarto","testthat","knitr"
)
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, Ncpus = max(1, parallel::detectCores()-1))
invisible(lapply(pkgs, require, character.only = TRUE))
message("Bootstrap OK — packages installés/chargés.")
