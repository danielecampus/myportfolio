# =============================================================================
# 07_universe_optimization.R
# Standalone runner: for each portfolio, optimizes over the FULL asset universe
# (not just portfolio assets). Two approaches:
#   A) optimize_universe()       — pure Markowitz (min-var & max-Sharpe) bounded
#   B) optimize_macro_universe() — Black-Litterman tilted + bounded max-Sharpe
#
# Assets not currently in the portfolio are allowed up to max_new_weight (config).
# Current portfolio weights can deviate by ± current_slack.
# This produces concrete rebalancing suggestions, including adding new assets
# when their historical characteristics or macro context justify it.
# =============================================================================

source("library.R")
source("R/fun_ptf_analysis.R")
source("R/fun_optimization.R")

cfg         <- yaml::read_yaml("config.yaml")
input_path  <- cfg$paths$input
output_path <- cfg$paths$output

ticker_df        <- arrow::read_parquet(paste0(input_path, "data_ticker_df.parquet"))
prices           <- arrow::read_parquet(paste0(input_path, "data_prices.parquet"))
returns          <- arrow::read_parquet(paste0(input_path, "data_returns.parquet"))
returns_universe <- arrow::read_parquet(paste0(input_path, "data_returns_universe.parquet"))

ptf_names <- names(cfg$portfolios)

cat("== Universe Optimization ==\n")
cat(sprintf("Universe assets: %d  |  Core assets: %d\n\n",
            ncol(returns_universe) - 1L,
            ncol(returns) - 1L))

print_weights <- function(w, orig_w = NULL, min_w = 0.01) {
  w_sig <- sort(w[w >= min_w], decreasing = TRUE)
  for (nm in names(w_sig)) {
    pct    <- w_sig[nm] * 100
    marker <- if (!nm %in% names(orig_w)) " [NEW]" else ""
    delta  <- if (nm %in% names(orig_w))
      sprintf(" (was %.1f%%)", orig_w[nm] * 100) else ""
    cat(sprintf("    %-35s %5.1f%%%s%s\n", nm, pct, delta, marker))
  }
}

for (ptf_name in ptf_names) {

  cat(paste0("--- ", ptf_name, " ---\n"))
  ptf <- build_portfolio(ptf_name, cfg, returns, prices, ticker_df)
  orig_w <- ptf$quotes
  names(orig_w) <- ptf$assets

  exclude_assets <- cfg$portfolios[[ptf_name]]$exclude_from_universe %||% character(0)

  # ---- A) Pure Markowitz on universe ----------------------------------------
  cat("  [A] Markowitz universe optimization\n")
  univ_opt <- tryCatch(
    optimize_universe(ptf, returns_universe, cfg, exclude_assets = exclude_assets),
    error = function(e) {
      cat(sprintf("    ERROR: %s\n", e$message)); NULL
    }
  )

  if (!is.null(univ_opt)) {
    cat(sprintf("    Universe size: %d assets  |  Common history: %d months\n",
                length(univ_opt$universe), univ_opt$n_months))

    for (sol_name in c("min_variance", "max_sharpe")) {
      sol <- univ_opt[[sol_name]]
      if (is.null(sol)) next
      cat(sprintf("\n    %s:\n", sol$label))
      cat(sprintf("      E[R]: %.2f%%  |  Vol: %.2f%%  |  Sharpe: %.2f\n",
                  sol$expected_return * 100, sol$volatility * 100, sol$sharpe))
      if (length(sol$new_assets_added) > 0) {
        cat(sprintf("      New assets suggested: %s\n",
                    paste(sprintf("%s (%.1f%%)", sol$new_assets_added,
                                  sol$new_weights * 100), collapse = ", ")))
      } else {
        cat("      No new assets added (current allocation already efficient)\n")
      }
      cat("      Weights:\n")
      print_weights(sol$weights, orig_w, min_w = cfg$universe_optimization$min_weight_report)
    }

    # Save
    save_universe_opt_output(ptf_name, univ_opt, "universe", output_path)
    cat("\n    Saved: output/", ptf_name, "_universe_weights.parquet\n", sep = "")
  }

  # ---- B) Macro-tilted universe optimization (BL) ---------------------------
  cat("\n  [B] Macro-tilted (Black-Litterman) universe optimization\n")
  macro_univ_opt <- tryCatch(
    optimize_macro_universe(ptf, returns_universe, cfg, exclude_assets = exclude_assets),
    error = function(e) {
      cat(sprintf("    ERROR: %s\n", e$message)); NULL
    }
  )

  if (!is.null(macro_univ_opt)) {
    # Print active macro views
    if (is.null(macro_univ_opt$views$P) || length(macro_univ_opt$views$narrative) == 0) {
      cat("    Active views: none (all indicators neutral or unavailable)\n")
    } else {
      cat("    Active macro views:\n")
      for (n in macro_univ_opt$views$narrative) cat("      -", n, "\n")
    }

    cat(sprintf("\n    E[R]: %.2f%%  |  Vol: %.2f%%  |  Sharpe: %.2f\n",
                macro_univ_opt$expected_return * 100,
                macro_univ_opt$volatility * 100,
                macro_univ_opt$sharpe))

    if (length(macro_univ_opt$new_assets_added) > 0) {
      cat(sprintf("    New assets suggested: %s\n",
                  paste(sprintf("%s (%.1f%%)", macro_univ_opt$new_assets_added,
                                macro_univ_opt$new_weights * 100), collapse = ", ")))
    } else {
      cat("    No new assets added\n")
    }

    cat("    Macro-tilted weights:\n")
    print_weights(macro_univ_opt$weights, orig_w,
                  min_w = cfg$universe_optimization$min_weight_report)

    # Save
    save_universe_opt_output(ptf_name, macro_univ_opt, "macro_universe", output_path)
    cat("\n    Saved: output/", ptf_name, "_macro_universe_weights.parquet\n", sep = "")
  }

  cat("\n")
}

cat("Universe optimization complete.\n")
