# =============================================================================
# 03_montecarlo_sim.R
# =============================================================================

source("library.R")
source("R/fun_ptf_analysis.R")   # build_portfolio
source("R/fun_simulation.R")     # run_montecarlo, save_*

cfg         <- yaml::read_yaml("config.yaml")
input_path  <- cfg$paths$input
output_path <- cfg$paths$output

# Load data once
ticker_df <- read_parquet(paste0(input_path, "data_ticker_df.parquet"))
prices    <- read_parquet(paste0(input_path, "data_prices.parquet"))
returns   <- read_parquet(paste0(input_path, "data_returns.parquet"))

ptf_names <- names(cfg$portfolios)
cat("== Monte Carlo simulation pipeline ==\n")
cat("n_sim:", cfg$simulation$n_sim,
    "| seed:", cfg$simulation$seed,
    "| default n_months:", cfg$simulation$n_months, "\n\n")

for (ptf_name in ptf_names) {
  cat("--- ", ptf_name, " ---\n")

  # Build portfolio (needed to provide returns slice and weights)
  ptf <- build_portfolio(ptf_name, cfg, returns, prices, ticker_df)

  # Run simulation (n_months from config: portfolio override > global default)
  mc <- run_montecarlo(ptf, cfg)

  save_mc_output(ptf_name, mc, output_path)
  save_mc_plot(ptf_name,   mc, output_path)

  s <- mc$forecast_summary
  cat(sprintf("  Horizon: %d months  |  E[R] (ann): %.2f%%\n",
              s$Horizon_period, s$Expected_Return * 100))
  cat(sprintf("  Expected value: %.0f  (Q5: %.0f / Q95: %.0f)\n",
              s$Expected_Value, s$Q_5, s$Q_95))
  cat(sprintf("  VaR 95%% (cumulative): %.2f%%  |  ES 95%%: %.2f%%\n\n",
              s$VaR * 100, s$ES * 100))
}

cat("Monte Carlo simulation complete.\n")