# =============================================================================
# 08_report.R
# Standalone runner: generates PDF reports for each portfolio and period type.
# Run AFTER all other scripts (01–07) have produced their parquet outputs.
#
# Output structure:
#   reports/{portfolio}/{period_label}_{portfolio}.pdf
#
# Period types generated:
#   quarterly  — most recently completed quarter (e.g. 2026-Q1 when run in April)
#   ytd        — year-to-date through end of last completed quarter (e.g. 2026-YTD)
#   semiannual — last completed half-year (e.g. 2025-H2 or 2026-H1)
#   annual     — last completed full year (e.g. 2025)
# =============================================================================

source("library.R")
source("R/fun_ptf_analysis.R")
source("R/fun_report.R")

cfg         <- yaml::read_yaml("config.yaml")
output_path <- cfg$paths$output
ptf_names   <- names(cfg$portfolios)
periods     <- c("quarterly", "ytd", "semiannual", "annual")

cat("== Report Generation ==\n")
cat(sprintf("Portfolios: %s\n", paste(ptf_names, collapse = ", ")))
cat(sprintf("Periods:    %s\n\n", paste(periods, collapse = ", ")))

for (ptf_name in ptf_names) {
  cat(sprintf("--- Portfolio: %s ---\n", ptf_name))
  for (period in periods) {
    pinfo <- get_period_info(period)
    cat(sprintf("    [%s] %s ... ", period, pinfo$label))
    pdf_path <- tryCatch(
      generate_portfolio_report(
        ptf_name     = ptf_name,
        cfg          = cfg,
        output_path  = output_path,
        reports_path = "reports/",
        period       = period
      ),
      error = function(e) {
        cat(sprintf("ERROR: %s\n", e$message))
        NULL
      }
    )
    if (!is.null(pdf_path)) cat(sprintf("OK  →  %s\n", pdf_path))
  }
  cat("\n")
}

cat("Report generation complete.\n")
