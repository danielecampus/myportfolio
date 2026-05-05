# =============================================================================
# _targets.R
# Pipeline definition for the {targets} package.
# Run with `tar_make()` (or via 00_run_pipeline.R).
# =============================================================================

library(targets)
library(tarchetypes)

tar_option_set(
  packages = c(
    "tidyverse", "purrr", "readr", "readxl", "openxlsx", "writexl",
    "yfR", "rvest", "tidyquant", "quantmod",
    "nloptr", "quadprog", "arrow",
    "PerformanceAnalytics",
    "yaml", "xts", "zoo", "jsonlite",
    "lubridate"
  ),
  format = "rds"
)

tar_source("R/")

cfg       <- yaml::read_yaml("config.yaml")
ptf_names <- names(cfg$portfolios)


list(

  # --- Config tracking ------------------------------------------------------
  tar_target(config_file, "config.yaml", format = "file"),
  tar_target(config,      yaml::read_yaml(config_file)),

  # --- 01. Sourcing ---------------------------------------------------------
  tar_target(tickers,        load_tickers(config$paths$input)),
  tar_target(ts_prices,      download_prices(tickers)),
  tar_target(
    proxy_map,
    {
      pr <- tickers[nchar(trimws(tickers$Extend_Via)) > 0 &
                    trimws(tickers$Role) != "proxy", , drop = FALSE]
      if (nrow(pr) > 0) setNames(as.list(pr$Extend_Via), pr$Index) else list()
    }
  ),
  tar_target(
    proxy_assets,
    tickers$Index[trimws(tickers$Role) == "proxy"]
  ),
  tar_target(
    portfolio_assets,
    unique(unlist(lapply(config$portfolios, `[[`, "assets")))
  ),
  tar_target(
    price_matrices,
    build_price_matrix(
      ts_prices,
      min_obs_months   = 12L * (config$sourcing$min_history_years %||% 5L),
      proxy_map        = proxy_map,
      portfolio_assets = portfolio_assets,
      proxy_assets     = proxy_assets
    )
  ),
  tar_target(prices,          price_matrices$core),
  tar_target(prices_universe, price_matrices$universe),
  tar_target(returns,         compute_returns(prices)),
  tar_target(returns_universe, compute_returns(prices_universe, drop_na = FALSE)),
  tar_target(
    sourcing_files,
    save_data(tickers, prices, returns, prices_universe, returns_universe,
              config$paths$input),
    format = "file"
  ),

  # --- Per-portfolio analysis, MC, optimization, backtest -------------------
  tar_map(
    values = list(ptf_name = ptf_names),
    names  = ptf_name,

    # 02. Portfolio analysis
    tar_target(
      ptf,
      build_portfolio(ptf_name, config, returns, prices, tickers)
    ),
    tar_target(
      ptf_files,
      save_ptf_output(ptf_name, ptf, config$paths$output),
      format = "file"
    ),
    tar_target(
      ptf_plots,
      save_ptf_plots(ptf_name, ptf, config$paths$output),
      format = "file"
    ),

    # 03. Monte Carlo
    tar_target(
      mc,
      run_montecarlo(ptf, config)
    ),
    tar_target(
      mc_files,
      save_mc_output(ptf_name, mc, config$paths$output),
      format = "file"
    ),
    tar_target(
      mc_plot,
      save_mc_plot(ptf_name, mc, config$paths$output),
      format = "file"
    ),

    # 04. Goal-based optimization (skipped if no goal in config)
    tar_target(
      goal_opt,
      if (!is.null(config$portfolios[[ptf_name]]$goal)) {
        optimize_for_goal(ptf, config$portfolios[[ptf_name]]$goal,
                          risk_free = config$global$risk_free)
      } else NULL
    ),
    tar_target(
      goal_files,
      save_goal_output(ptf_name, goal_opt, config$paths$output),
      format = "file"
    ),

    # 05. Macro-tilted optimization (Black-Litterman from FRED rules)
    tar_target(
      macro_opt,
      optimize_macro(ptf, config)
    ),
    tar_target(
      macro_files,
      save_macro_output(ptf_name, macro_opt, config$paths$output),
      format = "file"
    ),

    # 06. Backtesting comparison across strategies
    tar_target(
      backtest,
      {
        rets_mat <- ptf$returns %>%
          arrange(Dates) %>% select(-Dates) %>% as.matrix()
        rownames(rets_mat) <- as.character(
          ptf$returns %>% arrange(Dates) %>% pull(Dates)
        )

        # Build benchmark weights inline (equity/bond classification)
        is_equity <- grepl(
          paste0("MSCI World|World Momentum|World Quality|World Low Volatility|",
                 "World Value|Europe RAFI|Health Care|Consumer Staples|Small Cap|",
                 "Emerging|EM IMI|Europe 600|STOXX|World Small|",
                 "China|All Country"),
          ptf$assets, ignore.case = TRUE
        )
        is_bond <- grepl(
          "Gov bonds|Corp.*bonds|High-Yield|Inflation-Linked|TIPS|linker",
          ptf$assets, ignore.case = TRUE
        )

        ew_w  <- rep(1 / length(ptf$assets), length(ptf$assets))
        bench <- rep(0, length(ptf$assets))
        if (any(is_equity)) bench[is_equity] <- 0.60 / sum(is_equity)
        if (any(is_bond))   bench[is_bond]   <- 0.40 / sum(is_bond)
        if (sum(bench) > 0) bench <- bench / sum(bench) else bench <- ew_w

        strategies <- list(
          Current         = strategy_fixed_weights(ptf$quotes),
          Max_Sharpe      = strategy_max_sharpe(risk_free = config$global$risk_free),
          Min_Variance    = strategy_min_variance(),
          Equal_Weight    = strategy_fixed_weights(ew_w),
          Benchmark_60_40 = strategy_fixed_weights(bench)
        )

        # Add MSCI World buy-and-hold as market benchmark (portfolio-specific)
        msci_idx <- which(ptf$assets == "MSCI World")
        if (length(msci_idx) == 1L) {
          w_msci <- rep(0, length(ptf$assets)); w_msci[msci_idx] <- 1
          strategies[["MSCI_World_BnH"]] <- strategy_fixed_weights(w_msci)
        }

        run_strategy_comparison(
          returns_matrix  = rets_mat,
          strategies      = strategies,
          lookback        = config[["backtest"]][["lookback"]],
          rebalance_freq  = config[["backtest"]][["rebalance_freq"]],
          risk_free       = config$global$risk_free
        )
      }
    ),
    tar_target(
      backtest_files,
      save_backtest_output(ptf_name, backtest, config$paths$output),
      format = "file"
    ),

    # 07. Universe optimization — Markowitz (bounded) over full asset universe
    tar_target(
      universe_opt,
      optimize_universe(ptf, returns_universe, config,
                        exclude_assets = config$portfolios[[ptf_name]]$exclude_from_universe %||% character(0))
    ),
    tar_target(
      universe_opt_files,
      save_universe_opt_output(ptf_name, universe_opt, "universe", config$paths$output),
      format = "file"
    ),

    # 07b. Macro-tilted universe optimization (Black-Litterman on full universe)
    tar_target(
      macro_universe_opt,
      optimize_macro_universe(ptf, returns_universe, config,
                              exclude_assets = config$portfolios[[ptf_name]]$exclude_from_universe %||% character(0))
    ),
    tar_target(
      macro_universe_files,
      save_universe_opt_output(ptf_name, macro_universe_opt, "macro_universe",
                               config$paths$output),
      format = "file"
    )
  )
)