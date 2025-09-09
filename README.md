# alpine-financeR-pro — Projet R (finance + stats) 

> validation rigoureuse (VaR/ES), CV glasso, OOS backtests,
> rapport Quarto pro, tests unitaires, style clean et reproductible.

## Points forts
- **Pipeline** reproductible `{targets}` + **stockage Parquet/Arrow** (rapide, portable).
- **Dépendances** : corrélations + **glasso** avec **sélection de ρ par CV** (log-vraisemblance).
- **Risque** : DCC-GARCH, **VaR/ES backtesting** (Kupiec, Christoffersen), ES proxy et tables de couverture.
- **Portefeuilles** : **HRP** avec **shrinkage α** (contrôle diversification), variante **HRP “glasso”** (ordre via corr. partielle).
- **Backtest OOS** : fenêtrage roulant, rebal mensuel, **coûts de transaction**, turnover, métriques.
- **Reporting** : `reports/analysis_pro.qmd` (executive summary + diagnostics & tableaux), prêt pour PDF/HTML.
- **Qualité** : `testthat`, lint config simple, script `scripts/99_build_all.R` pour tout rejouer.

## Démarrage
```r
# 1) bootstrap
source("scripts/00_bootstrap.R")

# 2) pipeline (données + features + modèles)
targets::tar_make()

# 3) rapports
quarto::quarto_render("reports/analysis_pro.qmd")
# (l'ancien "analysis.qmd" reste compatible si tu en as besoin)
```

## Paramètres
- `config.yml` : univers de tickers, dates, params (α shrinkage, ρ grid CV, fenêtre OOS...).

---

