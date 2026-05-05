# =============================================================================
# R/fun_montecarlo.R
# Functions for Monte Carlo portfolio simulation using historical bootstrap.
# =============================================================================

#' Run a Monte Carlo simulation on a portfolio using historical bootstrap
#'
#' Resamples entire rows from the historical returns matrix to preserve
#' empirical correlation structure and tail behaviour without distributional
#' assumptions (preferred over multivariate normal).
#'
#' @param ptf Portfolio object (output of build_portfolio)
#' @param cfg Config list
#' @param n_months Optional override for simulation horizon (months)
#' @return List with forecast_summary (1-row data.frame of metrics) and
#'         forecast_ts (data.frame with historical + forecast rebased to 100)
run_montecarlo <- function(ptf, cfg, n_months = NULL) {

  # Resolve simulation parameters: portfolio override > global default
  ptf_cfg       <- cfg$portfolios[[ptf$name]]
  n_period      <- n_months %||% ptf_cfg$n_months %||% cfg$simulation$n_months
  n_sim         <- cfg$simulation$n_sim
  seed          <- cfg$simulation$seed
  initial_value <- ptf$initial_value
  monthly_cf    <- ptf_cfg$monthly_cf %||% 0

  set.seed(seed)

  # Historical returns sorted ascending for time-series construction
  ret_pure <- ptf$returns %>%
    arrange(Dates) %>%
    select(-Dates) %>%
    as.matrix()
  n_obs <- nrow(ret_pure)

  # ---- Historical bootstrap ------------------------------------------------
  # Resample WHOLE row vectors so that the cross-sectional correlation 
  # between assets at any given month is preserved as observed in
  # the historical data, including stress periods.
  idx <- matrix(
    sample(n_obs, size = n_sim * n_period, replace = TRUE),
    nrow = n_sim,
    ncol = n_period
  )

  portfolio_returns <- matrix(NA_real_, nrow = n_sim, ncol = n_period)
  for (s in seq_len(n_sim)) {
    ret_block             <- ret_pure[idx[s, ], , drop = FALSE]
    portfolio_returns[s, ] <- as.vector(ret_block %*% ptf$quotes)
  }

  # Cumulative portfolio value path for each simulation
  portfolio_values        <- matrix(NA_real_, nrow = n_sim, ncol = n_period)
  portfolio_values[, 1]   <- initial_value * (1 + portfolio_returns[, 1])
  for (t in 2:n_period) {
    portfolio_values[, t] <- portfolio_values[, t - 1] * (1 + portfolio_returns[, t])
  }

  final_values <- portfolio_values[, n_period]
  mean_value   <- mean(final_values)
  sd_value     <- sd(final_values)
  quantiles    <- quantile(final_values, probs = c(0.05, 0.95))

  # ---- Risk metrics on cumulative-horizon return ---------------------------
  # VaR / ES are computed on the total return over the FULL horizon
  # (final_value / initial_value - 1), not on the last single period.
  cumulative_returns <- final_values / initial_value - 1
  VaR_pct            <- quantile(cumulative_returns, probs = 1 - cfg$global$var_level)
  ES_pct             <- mean(cumulative_returns[cumulative_returns <= VaR_pct])

  # Annualized expected return: convert total cumulative return into an
  # annualized rate using the geometric formula. Required because n_period
  # may not equal 12 months.
  expected_return <- (mean_value / initial_value)^(12 / n_period) - 1

  forecast_summary <- data.frame(
    Horizon_period  = n_period,
    Expected_Return = expected_return,
    Std_Dev         = sd_value / mean_value,    # coefficient of variation
    VaR             = as.numeric(VaR_pct),
    ES              = ES_pct,
    Initial_Value   = initial_value,
    Expected_Value  = mean_value,
    Q_5             = as.numeric(quantiles[1]),
    Q_95            = as.numeric(quantiles[2]),
    Monthly_CF      = monthly_cf,
    row.names       = "Forecast"
  )

  # ---- Time series for plotting (historical + forecast band) ---------------
  historical_returns <- ret_pure %*% ptf$quotes
  historical_data <- data.frame(
    Dates = ptf$returns %>% arrange(Dates) %>% pull(Dates)
  ) %>%
    mutate(
      Returns          = as.vector(historical_returns),
      Cumulative_Value = 100 * cumprod(1 + historical_returns)
    )

  last_value <- tail(historical_data$Cumulative_Value, 1)

  forecast_cumulative      <- matrix(NA_real_, nrow = n_sim, ncol = n_period)
  forecast_cumulative[, 1] <- last_value * (1 + portfolio_returns[, 1])
  for (t in 2:n_period) {
    forecast_cumulative[, t] <- forecast_cumulative[, t - 1] * (1 + portfolio_returns[, t])
  }

  forecast_df <- data.frame(
    Dates = seq.Date(from = max(historical_data$Dates) + 1,
                     by = "month", length.out = n_period),
    Mean  = apply(forecast_cumulative, 2, mean),
    P5    = apply(forecast_cumulative, 2, quantile, probs = 0.05),
    P95   = apply(forecast_cumulative, 2, quantile, probs = 0.95)
  )

  forecast_ts <- rbind(
    data.frame(Dates = historical_data$Dates,
               Mean  = historical_data$Cumulative_Value,
               P5    = NA_real_, P95 = NA_real_),
    forecast_df
  ) %>%
    arrange(desc(Dates))

  list(forecast_summary = forecast_summary, forecast_ts = forecast_ts)
}


#' Save Monte Carlo outputs to parquet
#'
#' @param ptf_name    Portfolio name
#' @param mc_result   Output of run_montecarlo
#' @param output_path Path to output folder
#' @return Vector of saved file paths
save_mc_output <- function(ptf_name, mc_result, output_path) {
  paths <- c(
    summary  = paste0(output_path, ptf_name, "_mc_summary.parquet"),
    forecast = paste0(output_path, ptf_name, "_mc_forecast.parquet")
  )
  write_parquet(mc_result$forecast_summary, paths["summary"])
  write_parquet(mc_result$forecast_ts,      paths["forecast"])
  unname(paths)
}


#' Save Monte Carlo forecast plot to PNG
#'
#' @param ptf_name    Portfolio name
#' @param mc_result   Output of run_montecarlo
#' @param output_path Path to output folder (must contain "gfx/" subfolder)
#' @return Path of saved file
save_mc_plot <- function(ptf_name, mc_result, output_path) {
  gfx_dir <- paste0(output_path, "gfx/")
  if (!dir.exists(gfx_dir)) dir.create(gfx_dir, recursive = TRUE)

  path <- paste0(gfx_dir, ptf_name, "_mc_forecast.png")
  png(path, width = 1000, height = 600)
  print(plot_simulation(
    mc_result$forecast_ts,
    mc_result$forecast_summary,
    ptf_name,
    mc_result$forecast_summary$Horizon_period
  ))
  dev.off()
  path
}


# --- Plot helpers -------------------------------------------------------------

plot_simulation <- function(forecast_ts, forecast_summary, name, n_period) {
  ggplot(forecast_ts, aes(x = Dates)) +
    geom_line(aes(y = Mean), color = "blue", linewidth = 1) +
    geom_ribbon(aes(ymin = P5, ymax = P95), fill = "blue", alpha = 0.2) +
    labs(
      title    = paste0("Forecast of the multiasset portfolio returns (", name, ")"),
      subtitle = paste(
        "t:", n_period,
        "months | E[R] =", round(forecast_summary$Expected_Return * 100, 2),
        "% | VaR:", round(forecast_summary$VaR * 100, 2),
        "% | ES:", round(forecast_summary$ES * 100, 2), "%"
      ),
      x = "Dates",
      y = "Portfolio Value (rebased 100)"
    ) +
    theme_minimal() +
    theme(
      plot.title    = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title    = element_text(size = 12)
    )
}

# Null-coalesce: x %||% y returns x if not NULL, else y
`%||%` <- function(x, y) if (!is.null(x)) x else y