# =============================================================================
# R/fun_backtesting.R
# Walk-forward backtesting and performance metrics.
# =============================================================================


#' Walk-forward backtest of a weights-generating strategy
#'
#' For each rebalance date:
#'   1. Take the lookback window of past returns
#'   2. Call strategy_fn(returns_window) -> weights
#'   3. Apply those weights for the next rebalance_freq months
#'   4. Record monthly portfolio returns
#'
#' Returns are MONTHLY simple returns. The function expects the input
#' returns matrix to be sorted ASCENDING by date.
#'
#' @param returns_matrix  Matrix of monthly returns (rows = months ascending,
#'                        columns = assets, must have rownames = dates)
#' @param strategy_fn     Function(returns_window) returning a weight vector
#' @param lookback        Lookback window in months (e.g. 36)
#' @param rebalance_freq  Rebalance frequency in months (e.g. 12 = annual)
#' @param strategy_name   Label for the output
#' @return data.frame with Date, Return, Cum_Value (rebased 100), Strategy
walk_forward_backtest <- function(returns_matrix, strategy_fn,
                                  lookback = 36, rebalance_freq = 12,
                                  strategy_name = "Strategy") {

  n_obs       <- nrow(returns_matrix)
  asset_names <- colnames(returns_matrix)
  dates       <- as.Date(rownames(returns_matrix))

  if (n_obs < lookback + rebalance_freq) {
    stop(sprintf("Not enough data: need >= %d months, have %d.",
                 lookback + rebalance_freq, n_obs))
  }

  # Indices of rebalance dates (start of each held window)
  rebal_start <- seq(from = lookback + 1, to = n_obs, by = rebalance_freq)

  monthly_records <- list()

  for (rs in rebal_start) {
    # Lookback window
    window <- returns_matrix[(rs - lookback):(rs - 1), , drop = FALSE]

    # Compute weights
    w <- tryCatch(
      strategy_fn(window),
      error = function(e) {
        warning(sprintf("Strategy failed at %s: %s; using equal weights",
                        dates[rs], e$message))
        rep(1 / ncol(returns_matrix), ncol(returns_matrix))
      }
    )

    # Sanity: long-only, sum to 1
    if (any(w < -1e-6)) w <- pmax(w, 0)
    if (abs(sum(w) - 1) > 1e-6) w <- w / sum(w)

    # Hold for rebalance_freq months (or until data ends)
    hold_end <- min(rs + rebalance_freq - 1, n_obs)
    for (m in rs:hold_end) {
      port_ret <- sum(returns_matrix[m, ] * w)
      monthly_records[[length(monthly_records) + 1]] <- data.frame(
        Date     = dates[m],
        Return   = port_ret,
        Strategy = strategy_name
      )
    }
  }

  result <- do.call(rbind, monthly_records)
  result$Cum_Value <- 100 * cumprod(1 + result$Return)
  result
}


#' Compute summary metrics from a backtest path
#'
#' @param path data.frame with Date, Return, Cum_Value, Strategy
#' @param risk_free Annual risk-free rate
#' @return Single-row data.frame with metrics
compute_backtest_metrics <- function(path, risk_free = 0.03) {
  rets   <- path$Return
  values <- path$Cum_Value
  n      <- length(rets)

  total_return <- tail(values, 1) / 100 - 1
  ann_return   <- (1 + total_return)^(12 / n) - 1
  ann_vol      <- sd(rets) * sqrt(12)
  sharpe       <- (ann_return - risk_free) / ann_vol

  # Drawdown from running peak
  peak     <- cummax(values)
  dd       <- values / peak - 1
  max_dd   <- min(dd)

  hit_rate <- mean(rets > 0)
  calmar   <- if (isTRUE(max_dd < 0)) ann_return / abs(max_dd) else NA_real_

  data.frame(
    Strategy        = unique(path$Strategy),
    N_months        = n,
    Total_Return    = total_return,
    Ann_Return      = ann_return,
    Ann_Volatility  = ann_vol,
    Sharpe          = sharpe,
    Max_Drawdown    = max_dd,
    Calmar          = calmar,
    Hit_Rate        = hit_rate
  )
}


# -----------------------------------------------------------------------------
# Strategy factory functions
# -----------------------------------------------------------------------------

#' Static fixed-weights strategy (current portfolio, equal-weight, 60/40, etc.)
strategy_fixed_weights <- function(weights) {
  function(returns_window) weights
}

#' Markowitz max-Sharpe strategy (re-estimated at each rebalance)
strategy_max_sharpe <- function(risk_free = 0.03) {
  function(returns_window) {
    mu    <- (1 + colMeans(returns_window))^12 - 1
    Sigma <- cov(returns_window) * 12
    res   <- markowitz_max_sharpe(mu, Sigma, risk_free = risk_free, long_only = TRUE)
    if (is.null(res)) rep(1 / length(mu), length(mu)) else res$weights
  }
}

#' Markowitz min-variance strategy
strategy_min_variance <- function() {
  function(returns_window) {
    Sigma <- cov(returns_window) * 12
    markowitz_min_variance(Sigma, long_only = TRUE)
  }
}


# -----------------------------------------------------------------------------
# Run a full comparison across multiple strategies
# -----------------------------------------------------------------------------

#' Run backtest across multiple strategies and combine results
#'
#' @param returns_matrix Monthly returns matrix (rows ascending)
#' @param strategies     Named list of strategy functions
#' @param lookback       Months
#' @param rebalance_freq Months
#' @param risk_free      Annual risk-free rate
#' @return List with paths (combined data.frame) and metrics (data.frame)
run_strategy_comparison <- function(returns_matrix, strategies,
                                    lookback = 36, rebalance_freq = 12,
                                    risk_free = 0.03) {
  paths <- lapply(names(strategies), function(name) {
    walk_forward_backtest(
      returns_matrix, strategies[[name]],
      lookback = lookback, rebalance_freq = rebalance_freq,
      strategy_name = name
    )
  })
  names(paths) <- names(strategies)

  # Align: all paths start from the same date (first rebalance hold start)
  combined_paths <- do.call(rbind, paths)

  metrics <- do.call(rbind, lapply(paths, compute_backtest_metrics, risk_free = risk_free))
  rownames(metrics) <- NULL

  list(paths = combined_paths, metrics = metrics)
}


#' Save backtest results (paths + metrics) to parquet and plot to PNG
#'
#' @param ptf_name    Portfolio name
#' @param backtest    Output of run_strategy_comparison()
#' @param output_path Path to output folder (must contain "gfx/" subfolder)
#' @return Vector of saved file paths
save_backtest_output <- function(ptf_name, backtest, output_path) {
  gfx_dir <- paste0(output_path, "gfx/")
  if (!dir.exists(gfx_dir)) dir.create(gfx_dir, recursive = TRUE)

  paths <- c(
    bt_paths   = paste0(output_path, ptf_name, "_bt_paths.parquet"),
    bt_metrics = paste0(output_path, ptf_name, "_bt_metrics.parquet"),
    bt_plot    = paste0(gfx_dir,     ptf_name, "_backtest.png")
  )

  arrow::write_parquet(backtest$paths,   paths["bt_paths"])
  arrow::write_parquet(backtest$metrics, paths["bt_metrics"])

  png(paths["bt_plot"], width = 1100, height = 650)
  print(plot_backtest(backtest$paths,
                      title = paste0("Backtest comparison: ", ptf_name)))
  dev.off()

  unname(paths)
}


#' Plot cumulative-value paths for compared strategies
plot_backtest <- function(paths_df, title = "Backtest comparison") {
  ggplot(paths_df, aes(x = Date, y = Cum_Value, color = Strategy)) +
    geom_line(linewidth = 0.9) +
    labs(title = title, x = "Date", y = "Portfolio value (rebased 100)") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
}
