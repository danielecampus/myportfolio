# =============================================================================
# R/fun_ptf_analysis.R
# Functions for portfolio analysis: risk metrics, weighted returns, plots.
# =============================================================================

#' Build the full portfolio object from config and loaded data
#'
#' @param ptf_name  Portfolio name (key in config$portfolios)
#' @param cfg       Config list (from yaml::read_yaml)
#' @param returns   Monthly returns data.frame (sourcing output)
#' @param prices    Monthly prices data.frame
#' @param ticker_df Ticker / index table
#' @return List with portfolio metadata, data slices, and risk_portfolio output
build_portfolio <- function(ptf_name, cfg, returns, prices, ticker_df) {

  ptf_cfg <- cfg$portfolios[[ptf_name]]
  assets  <- ptf_cfg$assets
  quotes  <- unlist(ptf_cfg$quotes)
  names(quotes) <- assets

  # --- Validation -----------------------------------------------------------
  if (length(assets) != length(quotes)) {
    stop(sprintf("Portfolio '%s': assets length (%d) != quotes length (%d)",
                 ptf_name, length(assets), length(quotes)))
  }
  if (abs(sum(quotes) - 1) > 1e-6) {
    stop(sprintf("Portfolio '%s': quotes sum = %.8f (expected 1)",
                 ptf_name, sum(quotes)))
  }
  missing_assets <- setdiff(assets, names(returns))
  if (length(missing_assets) > 0) {
    stop(sprintf("Portfolio '%s': missing assets in returns data: %s",
                 ptf_name, paste(missing_assets, collapse = ", ")))
  }

  # --- Assemble portfolio object --------------------------------------------
  ptf <- list(
    name       = ptf_name,
    horizon    = ptf_cfg$horizon,
    time_decay = ptf_cfg$horizon / cfg$global$ideal_horizon,
    assets     = assets,
    quotes     = quotes,
    initial_value = ptf_cfg$initial_value
  )

  ptf$tickers <- ticker_df %>% filter(Index %in% assets)
  ptf$returns <- returns   %>% select(Dates, all_of(assets))
  ptf$prices  <- prices    %>% select(Dates, all_of(assets))

  ptf$ret_pure    <- ptf$returns %>% select(-Dates) %>% as.data.frame()
  ptf$var_cov     <- cov(ptf$ret_pure)
  ptf$corr_matrix <- cor(ptf$ret_pure)
  ptf$avg_returns <- ptf$ret_pure %>% summarise(across(everything(), mean))

  # --- Risk analysis --------------------------------------------------------
  ptf$ptf_output <- risk_portfolio(
    quotes      = ptf$quotes,
    var_cov     = ptf$var_cov,
    avg_returns = ptf$avg_returns,
    asset_names = ptf$assets,
    returns     = ptf$returns,
    risk_free   = cfg$global$risk_free,
    var_level   = cfg$global$var_level
  )

  return(ptf)
}

#' Compute portfolio risk metrics and per-asset risk decomposition
#'
#' @param quotes      Weights vector (must sum to 1)
#' @param var_cov     Monthly covariance matrix
#' @param avg_returns 1-row data.frame with monthly mean returns per asset
#' @param asset_names Character vector of asset names
#' @param returns     Returns data.frame including Dates column
#' @param risk_free   Annual risk-free rate
#' @param var_level   Confidence level for VaR / ES
#' @return List with Ptf_Analysis (per-asset table) and Ptf_Summary (aggregate)
risk_portfolio <- function(quotes, var_cov, avg_returns, asset_names, returns,
                           risk_free = 0.03, var_level = 0.95) {

  var_ptf <- as.numeric(t(quotes) %*% var_cov %*% quotes)
  sd_ptf  <- sqrt(var_ptf)              # MONTHLY standard deviation

  # Annualize: var_cov is built on monthly returns, so multiply sd by sqrt(12)
  # before computing the Sharpe ratio (otherwise Sharpe is inflated by ~3.46)
  sd_ptf_annual <- sd_ptf * sqrt(12)

  ptf_tbl <- tibble(Quotes = quotes) %>%
    mutate(
      VarCov_weighted = as.vector(var_cov %*% quotes),
      Marginal_RC     = VarCov_weighted / sd_ptf,
      Total_RC        = quotes * Marginal_RC,
      Expected_RC     = sd_ptf / length(quotes),
      Squared_Errors  = (Total_RC - Expected_RC)^2,
      Ret_Avg         = as.vector(t(avg_returns)),
      Ret_Weighted    = quotes * Ret_Avg,
      Assets          = asset_names
    ) %>%
    relocate(Assets, .before = everything())

  # Convert to xts: PerformanceAnalytics requires a time series object
  # with dates as the row index, not a plain data.frame
  returns_xts <- xts::xts(
    returns %>% select(-Dates),
    order.by = returns$Dates
  )
  VaR_out <- VaR(returns_xts, p = var_level, method = "historical",
                 portfolio_method = "component", weights = quotes)
  ES_out  <- ES(returns_xts,  p = var_level, method = "historical",
                portfolio_method = "component", weights = quotes)

  # Defensive extraction: column names vary across PerformanceAnalytics versions
  extract_pa_scalar <- function(x, preferred_name) {
    if (!is.null(x[[preferred_name]])) return(as.numeric(x[[preferred_name]]))
    nums <- Filter(is.numeric, x)
    if (length(nums) > 0) return(as.numeric(nums[[1]]))
    NA_real_
  }
  var_scalar <- extract_pa_scalar(VaR_out, "hVaR")
  es_scalar  <- extract_pa_scalar(ES_out,  "-r_exceed/c_exceed")

  Monthly_Ret <- sum(ptf_tbl$Ret_Weighted)
  Annual_Ret  <- (1 + Monthly_Ret)^12 - 1

  ptf_tot <- tibble(
    Tot_Quotes   = sum(quotes),
    Var_ptf      = var_ptf,
    Std_Dev_M    = sd_ptf,
    Std_Dev_A    = sd_ptf_annual,
    SSE          = sum(ptf_tbl$Squared_Errors),
    Monthly_Ret  = Monthly_Ret,
    Annual_Ret   = Annual_Ret,
    Sharpe_ratio = (Annual_Ret - risk_free) / sd_ptf_annual,
    VaR          = var_scalar,
    ES           = es_scalar
  )

  list(Ptf_Analysis = ptf_tbl, Ptf_Summary = ptf_tot)
}

#' Save portfolio analysis outputs to parquet
#'
#' @param ptf_name    Portfolio name
#' @param ptf         Portfolio object (output of build_portfolio)
#' @param output_path Path to output folder
#' @return Vector of saved file paths
save_ptf_output <- function(ptf_name, ptf, output_path) {
  paths <- c(
    analysis  = paste0(output_path, ptf_name, "_analysis.parquet"),
    summary   = paste0(output_path, ptf_name, "_summary.parquet"),
    corr_mat  = paste0(output_path, ptf_name, "_corr_matrix.parquet"),
    var_cov   = paste0(output_path, ptf_name, "_var_cov_ann.parquet")
  )
  write_parquet(ptf$ptf_output$Ptf_Analysis, paths["analysis"])
  write_parquet(ptf$ptf_output$Ptf_Summary,  paths["summary"])

  # Correlation matrix in long format for heatmap (Page 3)
  corr_long <- as.data.frame(as.table(ptf$corr_matrix))
  names(corr_long) <- c("Asset1", "Asset2", "Correlation")
  corr_long$Correlation <- as.numeric(corr_long$Correlation)
  write_parquet(corr_long, paths["corr_mat"])

  # Annualised covariance matrix in long format for bear-case computation (Page 8)
  vc_long <- as.data.frame(as.table(ptf$var_cov * 12))
  names(vc_long) <- c("Asset1", "Asset2", "Cov_Ann")
  vc_long$Cov_Ann <- as.numeric(vc_long$Cov_Ann)
  write_parquet(vc_long, paths["var_cov"])

  unname(paths)
}

#' Save portfolio allocation and returns plots to PNG
#'
#' @param ptf_name    Portfolio name
#' @param ptf         Portfolio object (output of build_portfolio)
#' @param output_path Path to output folder (must contain a "gfx/" subfolder)
#' @return Vector of saved file paths
save_ptf_plots <- function(ptf_name, ptf, output_path) {
  gfx_dir <- paste0(output_path, "gfx/")
  if (!dir.exists(gfx_dir)) dir.create(gfx_dir, recursive = TRUE)

  paths <- c(
    quotes  = paste0(gfx_dir, ptf_name, "_quotes.png"),
    returns = paste0(gfx_dir, ptf_name, "_returns.png")
  )

  analysis <- ptf$ptf_output$Ptf_Analysis

  png(paths["quotes"], width = 800, height = 600)
  print(plot_quotes(analysis$Assets, analysis$Quotes))
  dev.off()

  png(paths["returns"], width = 800, height = 600)
  print(plot_returns(
    analysis$Assets,
    (analysis$Ret_Avg      + 1)^12 - 1,    # annualize for display
    (analysis$Ret_Weighted + 1)^12 - 1
  ))
  dev.off()

  unname(paths)
}

# --- Plot helpers -------------------------------------------------------------

plot_quotes <- function(assets, quotes) {
  df        <- data.frame(assets, quotes)
  df$assets <- factor(df$assets, levels = df$assets)

  ggplot(df, aes(x = "", y = quotes, fill = assets)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = scales::percent(quotes, accuracy = 0.1)),
              position = position_stack(vjust = 0.5), size = 4) +
    labs(title = "Portfolio Asset Allocation") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
}

plot_returns <- function(assets, ret_avg, ret_weighted) {
  data.frame(assets, ret_avg, ret_weighted) %>%
    pivot_longer(cols = c(ret_avg, ret_weighted),
                 names_to  = "Type",
                 values_to = "Value") %>%
    ggplot(aes(x = reorder(assets, -Value), y = Value, fill = Type)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = scales::percent(Value, accuracy = 0.01)),
              position = position_dodge(width = 0.9),
              hjust = -0.1, size = 3.5) +
    labs(title = "Avg returns vs Weighted returns",
         x = "Assets", y = "Return", fill = "Type") +
    coord_flip() +
    theme_minimal()
}