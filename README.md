# myportfolio

Personal multi-asset portfolio analysis, optimization, and reporting framework built in R. Manages multiple portfolios through a single YAML configuration, runs a reproducible pipeline via [`{targets}`](https://docs.ropensci.org/targets/), and generates quarterly PDF reports with risk metrics, Monte Carlo projections, Black-Litterman macro-tilted allocations, and walk-forward backtesting.

Published at [danielecampus.eu](https://danielecampus.eu).

-----

## What it does

The pipeline takes a set of ETF-based portfolios defined in `config.yaml`, downloads market data from Yahoo Finance, and produces a full analytical suite for each portfolio:

- **Risk analysis** — annualized return, volatility, Sharpe ratio, VaR, ES, risk decomposition per asset (marginal and total risk contribution).
- **Monte Carlo simulation** — 10,000-path bootstrap resampling of historical monthly return vectors. Preserves empirical correlations and fat tails without distributional assumptions.
- **Goal-based optimization** — given a target terminal value, monthly contributions, and time horizon, computes the required annual return and finds the minimum-variance allocation on the efficient frontier that achieves it.
- **Macro-tilted optimization (Black-Litterman)** — fetches macro indicators from FRED (yield curve 10Y-2Y, CPI, Fed Funds Rate, HY spread, breakeven inflation, VIX), generates rule-based views with calibrated confidence levels, and produces posterior expected returns. Max-Sharpe optimization on the BL posterior gives macro-conditioned weights.
- **Universe optimization** — expands the search beyond current portfolio holdings to the full ETF universe in `assets.xlsx`, subject to concentration and cardinality constraints.
- **Walk-forward backtesting** — compares Current, Max-Sharpe, Min-Variance, Equal-Weight, 60/40 Benchmark, and MSCI World Buy-and-Hold strategies with annual rebalancing and 36-month rolling lookback. Reports annualized return, Sharpe, max drawdown, Calmar ratio, and hit rate.
- **PDF report generation** — multi-page quarterly report for each portfolio: executive summary, allocation and risk decomposition, correlation matrix, macro context, allocation comparison, backtest with drawdown, Monte Carlo projection, goal projection with calibrated scenarios, methodology and disclaimer.

## Project structure

```
myportfolio/
├── config.yaml              # All parameters: portfolios, macro rules, backtest, goals
├── _targets.R               # Pipeline DAG definition (targets package)
├── library.R                # Package loading
│
├── 00_run_pipeline.R        # Entry point: tar_make()
├── 01_sourcing.R            # Standalone: download and prepare market data
├── 02_ptf_analysis.R        # Standalone: portfolio risk analysis
├── 03_simulation.R          # Standalone: Monte Carlo simulation
├── 04_goal_optimization.R   # Standalone: goal-based optimization
├── 05_macro_optimization.R  # Standalone: Black-Litterman macro-tilted allocation
├── 06_backtesting.R         # Standalone: walk-forward strategy comparison
├── 07_universe_optimization.R # Standalone: full-universe optimization
├── 08_report.R              # Standalone: PDF report generation
├── FIRE.R                   # Financial independence / early retirement calculator
│
├── R/
│   ├── fun_sourcing.R       # Data download, alignment, monthly returns
│   ├── fun_ptf_analysis.R   # Risk metrics, risk decomposition, plots
│   ├── fun_simulation.R     # Historical bootstrap Monte Carlo
│   ├── fun_optimization.R   # Markowitz, Black-Litterman, FRED views, goal solver
│   ├── fun_backtesting.R    # Walk-forward engine, strategy factories, metrics
│   └── fun_report.R         # PDF page generators
│
├── input/                   # Market data (parquet) and asset definitions (xlsx)
├── output/                  # Analysis results, plots, parquet summaries
├── reports/                 # Generated PDF reports (per portfolio, per period)
├── doc/                     # Reference papers
└── archive/                 # Previous code versions (excluded from pipeline)
```

## Requirements

- **R** >= 4.4.0
- **FRED API key** (free) from [fred.stlouisfed.org](https://fredaccount.stlouisfed.org/apikey) — required for macro indicators. Set as `FRED_API_KEY` in your `.Renviron` file.

### R packages

```r
install.packages(c(
  "targets", "tarchetypes", "yaml",
  "tidyverse", "arrow", "readxl", "openxlsx", "writexl",
  "yfR", "rvest", "tidyquant", "quantmod",
  "nloptr", "quadprog", "PerformanceAnalytics",
  "fredr", "xts", "zoo",
  "corrplot", "gridExtra", "scales",
  "languageserver", "httpgd"
))
```

## Quick start

```r
# 1. Set working directory to the project root
setwd("path/to/myportfolio")

# 2. Run the full pipeline (sources data, analyses, simulations, optimization, backtest)
source("00_run_pipeline.R")

# 3. Inspect results
targets::tar_read(ptf_chiara)$ptf_output$Ptf_Summary
targets::tar_read(mc_chiara)$forecast_summary
targets::tar_read(backtest_chiara)$metrics

# 4. Visualize the pipeline DAG
targets::tar_visnetwork()

# 5. Generate PDF reports
source("08_report.R")
```

Alternatively, run individual stages for development or debugging:

```r
source("01_sourcing.R")           # refresh market data
source("02_ptf_analysis.R")       # risk analysis only
source("03_simulation.R")         # Monte Carlo only
source("05_macro_optimization.R") # Black-Litterman only
source("06_backtesting.R")        # backtest only
```

## Configuration

All portfolios, parameters, and optimization rules are defined in `config.yaml`. Adding a new portfolio requires zero code changes — only a new entry in the YAML file.

### Adding a portfolio

```yaml
portfolios:
  new_portfolio:
    horizon:       20             # investment horizon (years)
    initial_value: 30000          # current portfolio value
    assets:
      - "MSCI World"
      - "EU Gov bonds 7-10y"
      - "ETC GOLD"
    quotes:
      - 0.60
      - 0.25
      - 0.15
    goal:                         # optional — omit to skip goal optimization
      PV: 30000
      FV: 300000
      t:  20
      CF: 500
```

### Macro rules calibration

The `macro_rules` section controls Black-Litterman view generation. Three views are generated from FRED data:

1. **Equity vs Bonds** — driven by the 10Y-2Y Treasury yield curve spread. Thresholds define recession (inverted curve) and expansion (steep curve) regimes.
1. **Gold** — driven by CPI year-over-year. Higher inflation triggers a positive gold view.
1. **Cash** — anchored to the current Fed Funds Rate with high structural confidence.

Each view has a **Q value** (expected return or spread) and a **confidence** (0 to 1, capped at 0.85). These are calibrated from conditional historical backtests and should be reviewed quarterly.

## Methodology

|Component   |Method                                            |Reference               |
|------------|--------------------------------------------------|------------------------|
|Returns     |Simple monthly returns (P_t / P_{t-1} - 1)        |—                       |
|Covariance  |Sample covariance on full history                 |—                       |
|VaR / ES    |Historical simulation, 95% confidence             |PerformanceAnalytics    |
|Simulation  |Historical bootstrap (row-vector resampling)      |—                       |
|Optimization|Markowitz mean-variance, long-only (quadprog)     |Markowitz (1952)        |
|Macro views |Black-Litterman with rule-based FRED indicators   |Black & Litterman (1992)|
|Backtesting |Walk-forward, rolling lookback, no look-ahead bias|—                       |
|Goal solver |Numerical root-finding on FV-of-annuity equation  |—                       |

## Output examples

Each pipeline run produces per portfolio:

|File                                |Description                                              |
|------------------------------------|---------------------------------------------------------|
|`{name}_summary.parquet`            |Aggregate risk metrics (return, vol, Sharpe, VaR, ES)    |
|`{name}_analysis.parquet`           |Per-asset risk decomposition                             |
|`{name}_mc_summary.parquet`         |Monte Carlo expected value, VaR, ES over horizon         |
|`{name}_mc_forecast.parquet`        |Time series: historical + simulated paths (mean, P5, P95)|
|`{name}_macro_weights.parquet`      |BL macro-tilted optimal weights                          |
|`{name}_macro_views.txt`            |Active macro views narrative                             |
|`{name}_goal_weights.parquet`       |Goal-based optimal weights                               |
|`{name}_bt_metrics.parquet`         |Backtest performance comparison                          |
|`reports/{name}/{period}_{name}.pdf`|Quarterly PDF report                                     |

## Disclaimer

**This project does not constitute financial advice.** It is a personal analytical tool built for educational and research purposes. The models, outputs, reports, and any content generated by this project should not be interpreted as recommendations to buy, sell, or hold any financial instrument.

Key limitations to be aware of:

- **Past performance is not indicative of future results.** All returns, risk metrics, and simulations are based on historical data and carry no guarantee of future outcomes.
- **Model risk is real.** Mean-variance optimization is sensitive to estimation error in expected returns and covariances. Black-Litterman mitigates but does not eliminate this. Bootstrap simulation preserves historical tail behaviour but cannot predict unprecedented events.
- **Data limitations.** Market data is sourced from Yahoo Finance and may contain errors, survivorship bias, or gaps. Macro indicators from FRED may have publication lags. ETFs with short histories produce less reliable estimates.
- **No tax, transaction cost, or liquidity modelling.** The analysis assumes frictionless trading and does not account for bid-ask spreads, fund fees beyond those embedded in ETF prices, capital gains taxes, or liquidity constraints.
- **Drawdown figures are computed on monthly returns.** Actual peak-to-trough drawdowns on a daily basis can be 20-40% worse than the monthly figures reported.

Always consult a qualified financial professional before making investment decisions.

## License

Copyright © 2025 Daniele Campus. All Rights Reserved.

This repository is public for reference purposes only. No part of the code, data, or content may be used, reproduced, or distributed without explicit written permission. See <LICENSE.md> for details.
