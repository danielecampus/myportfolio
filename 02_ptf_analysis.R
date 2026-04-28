# =============================================================================
# 02_ptf_analysis.R
# =============================================================================

source("library.R")
source("R/fun_ptf_analysis.R")

cfg         <- yaml::read_yaml("config.yaml")
input_path  <- cfg$paths$input
output_path <- cfg$paths$output

# Load data once for all portfolios
ticker_df <- read_parquet(paste0(input_path, "data_ticker_df.parquet"))
prices    <- read_parquet(paste0(input_path, "data_prices.parquet"))
returns   <- read_parquet(paste0(input_path, "data_returns.parquet"))

ptf_names <- names(cfg$portfolios)
cat("== Portfolio analysis pipeline ==\n")
cat("Portfolios to process:", paste(ptf_names, collapse = ", "), "\n\n")

# Loop over every portfolio in config
results <- list()
for (ptf_name in ptf_names) {
  cat("--- ", ptf_name, " ---\n")

  ptf <- build_portfolio(ptf_name, cfg, returns, prices, ticker_df)

  save_ptf_output(ptf_name, ptf, output_path)
  save_ptf_plots(ptf_name,  ptf, output_path)

  s <- ptf$ptf_output$Ptf_Summary
  cat(sprintf("  Annual return: %.2f%%  |  Vol (ann): %.2f%%  |  Sharpe: %.3f\n",
              s$Annual_Ret * 100, s$Std_Dev_A * 100, s$Sharpe_ratio))
  cat(sprintf("  VaR 95%%: %.2f%%  |  ES 95%%: %.2f%%\n\n",
              s$VaR * 100, s$ES * 100))

  results[[ptf_name]] <- ptf
}

cat("Portfolio analysis complete.\n")