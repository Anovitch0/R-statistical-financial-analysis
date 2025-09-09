# scripts/99_build_all.R — rejoue tout & rend les rapports
source("scripts/00_bootstrap.R")
targets::tar_make()
quarto::quarto_render("reports/analysis_pro.qmd")
