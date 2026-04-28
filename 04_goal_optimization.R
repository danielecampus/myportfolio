# =============================================================================
# 04_goal_optimization.R
# Standalone runner: for each portfolio in config, computes the required
# return to reach the financial goal and finds the min-variance allocation
# that achieves it. Saves results to output/.
# =============================================================================

source("library.R")
source("R/fun_ptf_analysis.R")
source("R/fun_optimization.R")

cfg         <- yaml::read_yaml("config.yaml")
input_path  <- cfg$paths$input
output_path <- cfg$paths$output

# Load shared data once
ticker_df <- read_parquet(paste0(input_path, "data_ticker_df.parquet"))
prices    <- read_parquet(paste0(input_path, "data_prices.parquet"))
returns   <- read_parquet(paste0(input_path, "data_returns.parquet"))

ptf_names <- names(cfg$portfolios)
cat("== Goal-based optimization ==\n\n")

for (ptf_name in ptf_names) {

  ptf_cfg <- cfg$portfolios[[ptf_name]]
  if (is.null(ptf_cfg$goal)) {
    cat("---", ptf_name, "--- (no goal defined, skipping)\n\n")
    next
  }

  cat("---", ptf_name, "---\n")

  ptf  <- build_portfolio(ptf_name, cfg, returns, prices, ticker_df)
  goal <- ptf_cfg$goal

  result <- optimize_for_goal(ptf, goal, risk_free = cfg$global$risk_free)

  cat(sprintf("  Goal: PV=%s | FV=%s | t=%d yrs | CF=%s/month\n",
              format(goal$PV, big.mark = ","),
              format(goal$FV, big.mark = ","),
              goal$t,
              format(goal$CF, big.mark = ",")))

  if (result$status == "UNREACHABLE") {
    cat(sprintf("  Status: UNREACHABLE — required %.2f%% > frontier max %.2f%%\n",
                ifelse(is.na(result$required_return), NA, result$required_return * 100),
                result$frontier_range[2] * 100))
    cat("  Suggestion: increase CF, extend t, or lower FV.\n\n")
    next
  }

  if (result$status == "TRIVIAL") {
    cat(sprintf("  Status: TRIVIAL — required %.2f%% < frontier min %.2f%%\n",
                result$required_return * 100,
                result$frontier_range[1] * 100))
    cat("  Recommendation: use global min-variance portfolio.\n")
  }

  cat(sprintf("  Required return: %.2f%%\n",  result$required_return * 100))
  cat(sprintf("  Optimized return: %.2f%%  | Vol: %.2f%%  | Sharpe: %.2f\n",
              result$realized_return * 100,
              result$realized_vol * 100,
              result$sharpe))

  cat("  Optimal weights:\n")
  weights_pct <- round(result$weights * 100, 1)
  for (i in seq_along(weights_pct)) {
    if (weights_pct[i] > 0.05) {
      cat(sprintf("    %-30s %5.1f%%\n", names(weights_pct)[i], weights_pct[i]))
    }
  }

  if (!is.null(result$projection)) {
    p <- result$projection
    cat(sprintf("  Projection: TV %s | contributions %s | gains %s\n",
                format(round(p$Terminal_value), big.mark = ","),
                format(round(p$Total_contributions), big.mark = ","),
                format(round(p$Total_gains), big.mark = ",")))
  }
  cat("\n")

  # Save outputs
  weights_df <- data.frame(
    Asset  = names(result$weights),
    Weight = as.numeric(result$weights)
  )
  write_parquet(weights_df,
                paste0(output_path, ptf_name, "_goal_weights.parquet"))

  summary_df <- data.frame(
    Status          = result$status,
    Required_Return = result$required_return,
    Realized_Return = result$realized_return,
    Volatility      = result$realized_vol,
    Sharpe          = result$sharpe
  )
  write_parquet(summary_df,
                paste0(output_path, ptf_name, "_goal_summary.parquet"))
}

cat("Goal optimization complete.\n")
