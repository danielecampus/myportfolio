# =============================================================================
# R/fun_sourcing.R
# Functions for downloading and preparing market data.
# Input:  assets.xlsx with columns Ticker / Index
# Output: data_ticker_df, data_prices, data_returns (parquet + summary xlsx)
# =============================================================================

#' Load ticker / index table from assets.xlsx
#'
#' Reads the "ticker" sheet. Supports optional columns:
#'   Role       — "proxy" marks a ticker used only for history extension (not investable)
#'   Extend_Via — Index label of the proxy asset to use for history extension of this asset
#'
#' @param input_path Path to the input folder
#' @return data.frame with columns Ticker, Index, Role, Extend_Via
load_tickers <- function(input_path) {
  df <- read_xlsx(
    paste0(input_path, "assets.xlsx"),
    sheet     = "ticker",
    col_names = TRUE,
    col_types = "text"
  )
  if (!"Role"       %in% names(df)) df$Role       <- ""
  if (!"Extend_Via" %in% names(df)) df$Extend_Via <- ""
  df$Role[is.na(df$Role)]             <- ""
  df$Extend_Via[is.na(df$Extend_Via)] <- ""
  df
}

#' Download adjusted prices from Yahoo Finance for all tickers
#'
#' @param ticker_df data.frame with columns Ticker and Index
#' @return Named list: each element is an xts series of adjusted prices,
#'         named by the human-readable Index label. Tickers that fail to
#'         download are skipped with a warning.
download_prices <- function(ticker_df) {
  results <- map2(ticker_df$Ticker, ticker_df$Index, function(tk, nm) {
    tryCatch({
      prices <- getSymbols(tk, src = "yahoo", auto.assign = FALSE)
      prices <- na.omit(prices)
      Ad(prices)
    }, error = function(e) {
      warning(sprintf("Skipping ticker '%s' (%s): %s", tk, nm, conditionMessage(e)))
      NULL
    })
  })
  names(results) <- ticker_df$Index
  Filter(Negate(is.null), results)
}

#' Build core and universe monthly price matrices with optional proxy-based history extension
#'
#' Pipeline:
#'   1. Downsample each daily xts to end-of-month prices (per-series, preserving full range)
#'   2. Extend short-history assets using OLS-calibrated proxy series (at monthly level)
#'   3. Core matrix  — inner join of assets with >= min_obs_months non-NA observations
#'   4. Universe matrix — outer join; excludes proxy-only tickers and assets with
#'      insufficient history that are not held in any portfolio
#'
#' @param tsPrices        Named list of xts daily adjusted-price series
#' @param min_obs_months  Minimum months of history to include in core (default 60 = 5y)
#' @param proxy_map       Named list mapping asset Index label -> proxy Index label,
#'                        e.g. list("ETC GOLD" = "Gold Spot"). Proxy series are downloaded
#'                        as regular tickers but their synthetic returns are prepended to
#'                        the short-history asset.
#' @param portfolio_assets Character vector of asset names held in any portfolio —
#'                        these are always kept in the universe regardless of history length
#' @param proxy_assets    Character vector of Index labels that are proxy-only (not
#'                        investable) — excluded from both core and universe
#' @return List: core (data.frame, no NAs) and universe (data.frame, NAs allowed)
build_price_matrix <- function(tsPrices,
                               min_obs_months   = 60L,
                               proxy_map        = list(),
                               portfolio_assets = character(0),
                               proxy_assets     = character(0)) {

  # ---- Phase 1: downsample each xts to end-of-month prices --------------------
  to_monthly_df <- function(x, nm) {
    price_col_raw <- colnames(as.data.frame(x))[1]
    x %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Dates_chr") %>%
      mutate(
        Dates       = as.Date(Dates_chr),
        year_group  = year(Dates),
        month_group = month(Dates)
      ) %>%
      group_by(year_group, month_group) %>%
      slice_tail() %>%
      ungroup() %>%
      # Normalise to true calendar month-end so inner_join/full_join align correctly
      # across assets whose last trading day of the month differs by 1-2 days.
      mutate(Dates = lubridate::ceiling_date(Dates, "month") - 1L) %>%
      select(Dates, all_of(price_col_raw)) %>%
      rename(!!nm := all_of(price_col_raw))
  }

  monthly_list <- lapply(names(tsPrices), function(nm) to_monthly_df(tsPrices[[nm]], nm))
  names(monthly_list) <- names(tsPrices)

  # ---- Phase 2: proxy-based history extension ----------------------------------
  # For each (asset, proxy) pair: OLS on log-returns over overlap period,
  # then prepend synthetic prices for the pre-asset period.
  # Synthetic history is used ONLY for covariance estimation and backtesting.
  # Expected-return mu is computed from the common (real) window in risk_portfolio().
  extend_with_proxy_monthly <- function(mlist, pmap) {
    for (asset_nm in names(pmap)) {
      proxy_nm <- pmap[[asset_nm]]
      if (!asset_nm %in% names(mlist) || !proxy_nm %in% names(mlist)) {
        warning(sprintf("Proxy extension skipped for '%s': asset or proxy '%s' not found",
                        asset_nm, proxy_nm))
        next
      }

      asset_df <- mlist[[asset_nm]]   # Dates | price_col
      proxy_df <- mlist[[proxy_nm]]

      a_prices <- asset_df[[2]]; a_dates <- asset_df$Dates
      p_prices <- proxy_df[[2]]; p_dates <- proxy_df$Dates

      # Log-returns (skip first NA from diff)
      a_ret <- diff(log(a_prices)); a_ret_dates <- a_dates[-1]
      p_ret <- diff(log(p_prices)); p_ret_dates <- p_dates[-1]

      # Overlap: dates where both have real data
      overlap <- intersect(as.character(a_ret_dates), as.character(p_ret_dates))
      if (length(overlap) < 24L) {
        warning(sprintf(
          "Proxy extension '%s' <- '%s': overlap only %d months (need >= 24), skipping",
          asset_nm, proxy_nm, length(overlap)
        ))
        next
      }

      a_ov <- a_ret[as.character(a_ret_dates) %in% overlap]
      p_ov <- p_ret[as.character(p_ret_dates) %in% overlap]
      fit  <- lm(a_ov ~ p_ov)
      alpha <- coef(fit)[1L]; beta <- coef(fit)[2L]

      # Pre-asset period: proxy dates before the asset's first price date
      pre_mask  <- p_ret_dates < min(a_dates)
      pre_dates <- p_ret_dates[pre_mask]
      pre_p_ret <- p_ret[pre_mask]
      if (length(pre_p_ret) == 0L) next

      synth_log_ret <- alpha + beta * pre_p_ret

      # Reconstruct synthetic prices backwards from the asset's first real price
      first_price <- a_prices[1L]
      synth_prices <- numeric(length(synth_log_ret))
      p_val <- first_price
      for (i in rev(seq_along(synth_log_ret))) {
        p_val          <- p_val * exp(-synth_log_ret[i])
        synth_prices[i] <- p_val
      }

      synth_df <- data.frame(Dates = pre_dates, Price = synth_prices,
                             stringsAsFactors = FALSE)
      colnames(synth_df)[2L] <- colnames(asset_df)[2L]

      mlist[[asset_nm]] <- bind_rows(synth_df, asset_df) %>% arrange(Dates)

      message(sprintf(
        "Extended '%s' with %d synthetic months via '%s' (alpha=%.4f, beta=%.3f, R2=%.3f)",
        asset_nm, length(pre_p_ret), proxy_nm,
        alpha, beta, summary(fit)$r.squared
      ))
    }
    mlist
  }

  if (length(proxy_map) > 0L) {
    monthly_list <- extend_with_proxy_monthly(monthly_list, proxy_map)
  }

  # ---- Phase 3: core matrix (inner join, long-history assets only) -------------
  is_core_eligible <- function(nm) {
    if (nm %in% proxy_assets) return(FALSE)
    n_obs <- sum(!is.na(monthly_list[[nm]][[2L]]))
    n_obs >= min_obs_months
  }
  core_names <- Filter(is_core_eligible, names(monthly_list))
  if (length(core_names) == 0L) {
    warning("No assets meet min_obs_months threshold — using all non-proxy assets as core.")
    core_names <- setdiff(names(monthly_list), proxy_assets)
  }
  dropped_from_core <- setdiff(setdiff(names(monthly_list), proxy_assets), core_names)
  if (length(dropped_from_core) > 0L) {
    message(sprintf("Universe-only assets (< %d months history): %s",
                    min_obs_months, paste(dropped_from_core, collapse = ", ")))
  }

  core <- Reduce(function(a, b) inner_join(a, b, by = "Dates"),
                 monthly_list[core_names]) %>%
    na.omit() %>%
    arrange(Dates)

  # ---- Phase 4: universe matrix (outer join, filtered) -------------------------
  is_universe_eligible <- function(nm) {
    if (nm %in% proxy_assets) return(FALSE)
    n_obs <- sum(!is.na(monthly_list[[nm]][[2L]]))
    n_obs >= min_obs_months || nm %in% portfolio_assets
  }
  univ_names <- Filter(is_universe_eligible, names(monthly_list))

  universe <- Reduce(function(a, b) full_join(a, b, by = "Dates"),
                     monthly_list[univ_names]) %>%
    arrange(Dates)

  list(core = core, universe = universe)
}

#' Compute simple monthly returns from a price matrix
#'
#' @param sheet_prices data.frame from build_price_matrix (core or universe)
#' @param drop_na      If TRUE (default), drop rows where ANY asset has NA.
#'                     Set FALSE for the universe matrix to preserve partial rows.
#' @return data.frame with Dates and asset returns, ordered descending
compute_returns <- function(sheet_prices, drop_na = TRUE) {
  asset_cols <- sheet_prices %>% select(-Dates)
  ret        <- asset_cols / lag(asset_cols) - 1

  result <- ret %>%
    mutate(Dates = sheet_prices$Dates) %>%
    relocate(Dates, .before = everything()) %>%
    arrange(desc(Dates))

  if (drop_na) na.omit(result) else result[-nrow(result), ]  # always drop last (all-NA lag row)
}

#' Persist all sourcing artifacts to disk
#'
#' @param ticker_df       Ticker / index table
#' @param prices          Core monthly price data.frame
#' @param returns         Core monthly returns data.frame
#' @param prices_universe Universe monthly price data.frame (all assets, NAs allowed)
#' @param returns_universe Universe monthly returns data.frame (NAs allowed)
#' @param input_path      Path to input folder
#' @return Character vector of saved file paths (used by targets `format = "file"`)
save_data <- function(ticker_df, prices, returns,
                      prices_universe, returns_universe,
                      input_path) {

  # Sort prices descending for consistency with returns and the rest of the codebase
  prices_desc          <- prices          %>% arrange(desc(Dates))
  prices_universe_desc <- prices_universe %>% arrange(desc(Dates))

  paths <- c(
    ticker            = paste0(input_path, "data_ticker_df.parquet"),
    prices            = paste0(input_path, "data_prices.parquet"),
    returns           = paste0(input_path, "data_returns.parquet"),
    prices_universe   = paste0(input_path, "data_prices_universe.parquet"),
    returns_universe  = paste0(input_path, "data_returns_universe.parquet"),
    xlsx              = paste0(input_path, "data_timeseries.xlsx")
  )

  write_parquet(ticker_df,          paths["ticker"])
  write_parquet(prices_desc,        paths["prices"])
  write_parquet(returns,            paths["returns"])
  write_parquet(prices_universe_desc, paths["prices_universe"])
  write_parquet(returns_universe,   paths["returns_universe"])

  # Excel workbook for manual inspection (core data only)
  wb <- createWorkbook()
  addWorksheet(wb, "Prices");  writeData(wb, "Prices",  prices_desc)
  addWorksheet(wb, "Returns"); writeData(wb, "Returns", returns)
  saveWorkbook(wb, paths["xlsx"], overwrite = TRUE)

  unname(paths)
}