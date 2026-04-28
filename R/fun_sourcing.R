# =============================================================================
# R/fun_sourcing.R
# Functions for downloading and preparing market data.
# Input:  assets.xlsx with columns Ticker / Index
# Output: data_ticker_df, data_prices, data_returns (parquet + summary xlsx)
# =============================================================================

#' Load ticker / index table from assets.xlsx
#'
#' @param input_path Path to the input folder
#' @return data.frame with columns Ticker and Index
load_tickers <- function(input_path) {
  read_xlsx(
    paste0(input_path, "assets.xlsx"),
    sheet     = "ticker",
    col_names = TRUE,
    col_types = "text"
  )
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

#' Align series to the shortest common history and downsample to monthly
#'
#' Asset series with fewer than min_obs daily observations are excluded from the
#' "core" matrix (used for portfolio building) but retained in the "universe"
#' matrix (used for universe optimization). This prevents short-history ETFs
#' from truncating the historical window for the core portfolios.
#'
#' @param tsPrices      Named list of xts adjusted-price series
#' @param min_obs       Minimum number of daily observations to include in core
#'                      (default 252 * 5 = ~5 years)
#' @return List with two elements:
#'   \item{core}{data.frame for portfolio analysis (only long-history assets)}
#'   \item{universe}{data.frame for universe optimization (all assets, NAs allowed)}
build_price_matrix <- function(tsPrices, min_obs = 252 * 5) {

  # Split into core (sufficient history) and extended (all assets)
  core_ts <- Filter(function(x) nrow(x) >= min_obs, tsPrices)
  if (length(core_ts) == 0) {
    warning("No assets meet the minimum history threshold — using all assets as core.")
    core_ts <- tsPrices
  }
  dropped <- setdiff(names(tsPrices), names(core_ts))
  if (length(dropped) > 0) {
    message(sprintf(
      "Universe-only assets (history < %d obs, not used in portfolio building): %s",
      min_obs, paste(dropped, collapse = ", ")
    ))
  }

  monthly_from_xts <- function(ts_list) {
    min_len <- min(map_int(ts_list, nrow))
    aligned <- map(ts_list, ~tail(.x, min_len))
    mat <- do.call(cbind, aligned) %>% na.omit()
    colnames(mat) <- names(ts_list)
    mat %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "Dates_chr") %>%
      mutate(
        Dates       = as.Date(Dates_chr),
        year_group  = year(Dates),
        month_group = month(Dates)
      ) %>%
      group_by(year_group, month_group) %>%
      slice_tail() %>%
      ungroup() %>%
      select(-year_group, -month_group, -Dates_chr) %>%
      relocate(Dates, .before = everything()) %>%
      arrange(Dates)
  }

  # Universe matrix: full outer join on dates so short-history assets show NAs
  # rather than truncating the common window
  universe_monthly <- function(ts_list) {
    monthly_list <- lapply(names(ts_list), function(nm) {
      x <- ts_list[[nm]]
      # After as.data.frame(), the price column keeps its xts name (e.g. "IWDA.L.Adjusted")
      price_col_raw <- colnames(as.data.frame(x))[1]
      df <- x %>%
        as.data.frame() %>%
        tibble::rownames_to_column(var = "Dates_chr") %>%
        mutate(
          Dates       = as.Date(Dates_chr),
          year_group  = year(Dates),
          month_group = month(Dates)
        ) %>%
        group_by(year_group, month_group) %>%
        slice_tail() %>%
        ungroup() %>%
        select(Dates, all_of(price_col_raw)) %>%
        rename(!!nm := all_of(price_col_raw))
      df
    })
    Reduce(function(a, b) full_join(a, b, by = "Dates"), monthly_list) %>%
      arrange(Dates)
  }

  list(
    core     = monthly_from_xts(core_ts),
    universe = universe_monthly(tsPrices)
  )
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