source("library.R")
source("R/fun_optimization.R")
cfg <- yaml::read_yaml("config.yaml")

ind <- fetch_macro_indicators({
  k <- cfg$fred_api_key %||% ""
  if (nchar(trimws(k)) == 0) Sys.getenv("FRED_API_KEY") else k
})
cat("Yield curve 10Y-2Y:", round(ind$yield_curve_10y2y$latest, 2), "\n")
cat("CPI YoY:", round(compute_yoy(ind$cpi_yoy$series), 2), "%\n")
cat("Fed Funds Rate:", round(ind$fed_funds$latest, 2), "%\n")
cat("HY Spread:", round(ind$hy_spread$latest, 2), "%\n")
cat("5Y Breakeven:", round(ind$breakeven_5y$latest, 2), "%\n")
cat("VIX:", round(ind$vix$latest, 2), "\n")

# Vedi quali views si attivano con i parametri correnti
views <- generate_macro_views(ind, c("MSCI World","EU Gov bonds 7-10y","ETC GOLD","EU Overnight"), cfg$macro_rules)
print(views$narrative)
