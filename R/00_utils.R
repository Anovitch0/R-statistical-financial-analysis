# R/00_utils.R
suppressPackageStartupMessages({
  library(data.table); library(arrow); library(yaml); library(ggplot2); library(PerformanceAnalytics)
})

read_config <- function(path = "config.yml") as.list(yaml::read_yaml(path))

ensure_dir <- function(path) if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)

save_parquet_dt <- function(dt, path){ ensure_dir(dirname(path)); arrow::write_parquet(dt, path); normalizePath(path, winslash = "/") }
load_parquet_dt <- function(path){ as.data.table(arrow::read_parquet(path)) }

theme_alpine <- function(){
  theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face="bold", size=14),
          axis.title = element_text(face="bold"),
          legend.position="bottom")
}

plot_save <- function(p, file, w=10, h=6){ ensure_dir(dirname(file)); ggsave(file, p, width=w, height=h, dpi=150) }
