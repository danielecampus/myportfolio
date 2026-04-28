# =============================================================================
# 06_backtesting.R
# Standalone runner: walk-forward backtest comparing for each portfolio:
#   - Current weights (from config)
#   - Markowitz max-Sharpe (re-optimized each rebalance)
#   - Markowitz min-variance
#   - Equal-weight (1/N)
#   - 60/40 benchmark (broad equity / bonds)
# Annual rebalancing, 36-month rolling lookback.
# =============================================================================

source("library.R")
source("R/fun_ptf_analysis.R")
source("R/fun_optimization.R")
source("R/fun_backtesting.R")

cfg         <- yaml::read_yaml("config.yaml")
input_path  <- cfg$paths$input
output_path <- cfg$paths$output

ticker_df <- read_parquet(paste0(input_path, "data_ticker_df.parquet"))
prices    <- read_parquet(paste0(input_path, "data_prices.parquet"))
returns   <- read_parquet(paste0(input_path, "data_returns.parquet"))

bt_cfg <- cfg$backtest
cat("== Backtesting ==\n")
cat(sprintf("Lookback: %d months  |  Rebalance: every %d months  |  Risk-free: %.2f%%\n\n",
            bt_cfg$lookback, bt_cfg$rebalance_freq, cfg$global$risk_free * 100))

ptf_names <- names(cfg$portfolios)
all_metrics <- list()

for (ptf_name in ptf_names) {
  cat("---", ptf_name, "---\n")
  ptf <- build_portfolio(ptf_name, cfg, returns, prices, ticker_df)

  # Build returns matrix (ascending dates) with rownames = dates
  rets_mat <- ptf$returns %>% arrange(Dates) %>% select(-Dates) %>% as.matrix()
  rownames(rets_mat) <- as.character(ptf$returns %>% arrange(Dates) %>% pull(Dates))

  # --- Define strategies for this portfolio --------------------------------

  # Equal-weight: 1/N across all assets in the portfolio
  ew_weights <- rep(1 / length(ptf$assets), length(ptf$assets))
  names(ew_weights) <- ptf$assets

  # 60/40: aggregate equity / aggregate bonds within the portfolio universe.
  # Equity assets: any of the World/MSCI/Europe/Health/Staples/Small Cap labels.
  # Bond assets: gov / corp / inflation-linked / high-yield labels.
  # Cash and gold are excluded from the 60/40 benchmark.
  is_equity <- grepl(
    paste0("MSCI World|World Momentum|World Quality|World Low Volatility|",
           "World Value|Europe RAFI|World Health Care|World Consumer Staples|World Small Cap|",
           "MSCI Emerging Markets|Stoxx Europe 600|World Small Cap|",
           "China Tech|MSCI All Country"),
    ptf$assets, ignore.case = TRUE)
  is_bond   <- grepl("Gov bonds|Corp.*bonds|High-Yield|Inflation-Linked|TIPS|linker",
                     ptf$assets, ignore.case = TRUE)

  benchmark_weights <- rep(0, length(ptf$assets))
  if (any(is_equity)) benchmark_weights[is_equity] <- 0.60 / sum(is_equity)
  if (any(is_bond))   benchmark_weights[is_bond]   <- 0.40 / sum(is_bond)
  if (sum(benchmark_weights) > 0) {
    benchmark_weights <- benchmark_weights / sum(benchmark_weights)
  } else {
    benchmark_weights <- ew_weights   # fallback if no equity/bond mapping
  }
  names(benchmark_weights) <- ptf$assets

  strategies <- list(
    Current      = strategy_fixed_weights(ptf$quotes),
    Max_Sharpe   = strategy_max_sharpe(risk_free = cfg$global$risk_free),
    Min_Variance = strategy_min_variance(),
    Equal_Weight = strategy_fixed_weights(ew_weights),
    Benchmark_60_40 = strategy_fixed_weights(benchmark_weights)
  )

  # Add MSCI World buy-and-hold as market benchmark (if present in this portfolio)
  msci_idx <- which(ptf$assets == "MSCI World")
  if (length(msci_idx) == 1L) {
    w_msci <- rep(0, length(ptf$assets))
    w_msci[msci_idx] <- 1
    strategies[["MSCI_World_BnH"]] <- strategy_fixed_weights(w_msci)
  }

  comparison <- run_strategy_comparison(
    returns_matrix  = rets_mat,
    strategies      = strategies,
    lookback        = bt_cfg$lookback,
    rebalance_freq  = bt_cfg$rebalance_freq,
    risk_free       = cfg$global$risk_free
  )

  comparison$metrics$Portfolio <- ptf_name
  all_metrics[[ptf_name]] <- comparison$metrics

  # Print metrics
  print_metrics <- comparison$metrics %>%
    transmute(
      Strategy,
      `Tot Ret`   = sprintf("%.1f%%", Total_Return * 100),
      `Ann Ret`   = sprintf("%.2f%%", Ann_Return * 100),
      `Vol`       = sprintf("%.2f%%", Ann_Volatility * 100),
      `Sharpe`    = sprintf("%.2f",  Sharpe),
      `Max DD`    = sprintf("%.1f%%", Max_Drawdown * 100),
      `Calmar`    = sprintf("%.2f",  Calmar),
      `Hit`       = sprintf("%.0f%%", Hit_Rate * 100)
    )
  print(print_metrics, row.names = FALSE)

  # Save outputs
  write_parquet(comparison$paths,
                paste0(output_path, ptf_name, "_bt_paths.parquet"))
  write_parquet(comparison$metrics,
                paste0(output_path, ptf_name, "_bt_metrics.parquet"))

  # Save plot
  gfx_dir <- paste0(output_path, "gfx/")
  if (!dir.exists(gfx_dir)) dir.create(gfx_dir, recursive = TRUE)
  png(paste0(gfx_dir, ptf_name, "_backtest.png"), width = 1100, height = 650)
  print(plot_backtest(comparison$paths,
                      title = paste0("Backtest comparison: ", ptf_name)))
  dev.off()

  cat("\n")
}

# Combined metrics across all portfolios
combined <- do.call(rbind, all_metrics)
write_parquet(combined, paste0(output_path, "backtest_all_portfolios.parquet"))

cat("Backtesting complete.\n")
