# =============================================================================
# 01_sourcing.R
# Standalone runner: downloads market data and writes parquet files to input/.
# =============================================================================

source("library.R")
source("R/fun_sourcing.R")

`%||%` <- function(x, y) if (!is.null(x)) x else y

cfg        <- yaml::read_yaml("config.yaml")
input_path <- cfg$paths$input

cat("== Sourcing pipeline ==\n")

cat("[1/5] Loading tickers...\n")
ticker_df <- load_tickers(input_path)
cat("      Loaded", nrow(ticker_df), "tickers\n")

cat("[2/5] Downloading prices from Yahoo Finance...\n")
ts_prices <- download_prices(ticker_df)

cat("[3/5] Building monthly price matrices (core + universe)...\n")
min_obs       <- 252 * (cfg$sourcing$min_history_years %||% 5)
price_mats    <- build_price_matrix(ts_prices, min_obs = min_obs)
prices        <- price_mats$core
prices_univ   <- price_mats$universe
cat("      Core date range:", as.character(min(prices$Dates)), "to",
    as.character(max(prices$Dates)), "(", nrow(prices), "months,",
    ncol(prices) - 1L, "assets)\n")
cat("      Universe:", ncol(prices_univ) - 1L, "assets (with NAs for shorter history)\n")

cat("[4/5] Computing monthly returns...\n")
returns      <- compute_returns(prices)
returns_univ <- compute_returns(prices_univ, drop_na = FALSE)

cat("[5/5] Saving artifacts...\n")
saved_files <- save_data(ticker_df, prices, returns, prices_univ, returns_univ, input_path)
cat("      Wrote:\n")
cat(paste0("        - ", saved_files), sep = "\n")

cat("\nSourcing complete.\n")