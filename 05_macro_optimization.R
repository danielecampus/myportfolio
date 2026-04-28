# =============================================================================
# 05_macro_optimization.R
# Standalone runner: for each portfolio, fetches macro indicators from FRED,
# generates rule-based views, applies Black-Litterman, and computes max-Sharpe
# weights conditioned on the current macro regime.
# =============================================================================

source("library.R")
source("R/fun_ptf_analysis.R")
source("R/fun_optimization.R")

cfg         <- yaml::read_yaml("config.yaml")
input_path  <- cfg$paths$input
output_path <- cfg$paths$output

ticker_df <- read_parquet(paste0(input_path, "data_ticker_df.parquet"))
prices    <- read_parquet(paste0(input_path, "data_prices.parquet"))
returns   <- read_parquet(paste0(input_path, "data_returns.parquet"))

ptf_names <- names(cfg$portfolios)
cat("== Macro-tilted optimization (Black-Litterman) ==\n\n")

for (ptf_name in ptf_names) {

  cat("---", ptf_name, "---\n")
  ptf <- build_portfolio(ptf_name, cfg, returns, prices, ticker_df)

  result <- optimize_macro(ptf, cfg)

  # Print active views
  cat("  Active macro views:\n")
  if (is.null(result$views$P) || length(result$views$narrative) == 0) {
    cat("    (none — all indicators neutral or unavailable)\n")
  } else {
    for (n in result$views$narrative) cat("    -", n, "\n")
  }

  # Optimal weights
  cat(sprintf("\n  Expected return: %.2f%%  | Vol: %.2f%%  | Sharpe: %.2f\n",
              result$expected_return * 100,
              result$volatility * 100,
              result$sharpe))

  cat("  Optimal weights:\n")
  weights_pct <- round(result$weights * 100, 1)
  for (i in seq_along(weights_pct)) {
    if (weights_pct[i] > 0.05) {
      cat(sprintf("    %-30s %5.1f%%\n", names(weights_pct)[i], weights_pct[i]))
    }
  }

  # Compare prior vs posterior expected returns
  cat("\n  Prior vs posterior expected returns:\n")
  comparison <- data.frame(
    Asset     = names(result$mu_prior),
    Prior_pct = round(result$mu_prior * 100, 2),
    BL_pct    = round(result$mu_posterior * 100, 2)
  )
  comparison$Delta_bps <- round((comparison$BL_pct - comparison$Prior_pct) * 100)
  print(comparison, row.names = FALSE)
  cat("\n")

  # Save outputs
  weights_df <- data.frame(
    Asset  = names(result$weights),
    Weight = as.numeric(result$weights)
  )
  write_parquet(weights_df,
                paste0(output_path, ptf_name, "_macro_weights.parquet"))

  summary_df <- data.frame(
    Expected_Return = result$expected_return,
    Volatility      = result$volatility,
    Sharpe          = result$sharpe,
    N_active_views  = length(result$views$narrative)
  )
  write_parquet(summary_df,
                paste0(output_path, ptf_name, "_macro_summary.parquet"))

  # Save narrative
  narrative_lines <- as.character(unlist(result$views$narrative))
  if (length(narrative_lines) == 0) narrative_lines <- "(no active macro views)"
  writeLines(narrative_lines, paste0(output_path, ptf_name, "_macro_views.txt"))
}

cat("Macro optimization complete.\n")
