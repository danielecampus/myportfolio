# =============================================================================
# R/fun_report.R
# Quarterly PDF report generation for each portfolio.
# Reads saved parquet files from output/ — does NOT re-run optimizations.
# =============================================================================


# --- Colour palettes (pastel) -------------------------------------------------

# Scenario comparison colours
REPORT_COLOURS <- c(
  "Current"      = "#5D7FA8",   # muted cornflower blue
  "Max-Sharpe"   = "#6ABBB0",   # soft teal
  "Macro-tilted" = "#78B87A",   # sage green
  "Min-Variance" = "#B07FBA"    # soft mauve
)

STRATEGY_COLOURS <- c(
  "Current"          = "#5D7FA8",  # muted cornflower blue
  "Max_Sharpe"       = "#6ABBB0",  # soft teal
  "Min_Variance"     = "#B07FBA",  # soft mauve
  "Equal_Weight"     = "#E0A070",  # warm amber
  "Benchmark_60_40"  = "#9EAAB2",  # blue-grey
  "MSCI_World_BnH"   = "#D4904A"   # warm orange (equity benchmark)
)

# Asset class palette: warm = equity, cool = bonds, earth = commodity, neutral = cash
ASSET_CLASS_COLOURS <- list(
  Equity    = c("#F4A9A9", "#F5C4A0", "#F8DFA0", "#F9B890", "#F5D0C0", "#EDB89A"),
  Bond      = c("#9DC4D8", "#85B0CC", "#B8D4E5", "#7AAFC0", "#C5DDE8"),
  Commodity = c("#A8CC98", "#C2D8A8", "#8DB88D"),
  Cash      = c("#CBCCCC", "#B8BCBF", "#D5D8D8"),
  Other     = c("#C8AACC", "#B8A0C8", "#D8BDE0")
)

#' Classify an asset into a broad class using its name
classify_asset_simple <- function(name) {
  n <- tolower(name)
  if (grepl("msci|world|stoxx|europe|emerg|china|all country|small cap|momentum|quality|low vol|value|health|consumer|rafi|equity|stock", n))
    "Equity"
  else if (grepl("bond|corporate|high.yield|tips|inflation|linker|treasury|aggregate|credit|duration|gov|note|fixed", n))
    "Bond"
  else if (grepl("commodit|gold|silver|oil|metal|agri|natural resource", n))
    "Commodity"
  else if (grepl("cash|money market|liquidity|overnight|estr|short.term", n))
    "Cash"
  else
    "Other"
}

#' Assign pastel colours to a vector of asset names based on their asset class
get_asset_colours <- function(asset_names) {
  classes   <- vapply(asset_names, classify_asset_simple, character(1L))
  cls_count <- setNames(integer(length(ASSET_CLASS_COLOURS)), names(ASSET_CLASS_COLOURS))
  colours   <- character(length(asset_names))
  for (i in seq_along(asset_names)) {
    cls          <- classes[[i]]
    palette      <- ASSET_CLASS_COLOURS[[cls]]
    if (is.null(palette)) palette <- ASSET_CLASS_COLOURS$Other
    idx          <- (cls_count[[cls]] %% length(palette)) + 1L
    cls_count[cls] <- cls_count[[cls]] + 1L
    colours[i]   <- palette[idx]
  }
  setNames(colours, asset_names)
}


# --- Period info helper -------------------------------------------------------

#' Return metadata for a report period: label, period dates, chart window
#'
#' Logic: always refers to the most recently *completed* period.
#' e.g. running in April 2026 → quarterly = Q1 2026, annual = 2025, H = H2 2025.
#'
#' @param period One of "quarterly", "ytd", "semiannual", "annual"
#' @return Named list: label, period_start, period_end, chart_start, chart_end, chart_months
get_period_info <- function(period = "quarterly") {
  today           <- Sys.Date()
  q_start_current <- lubridate::floor_date(today, "quarter")
  last_q_end      <- q_start_current - 1L          # last day of completed quarter
  last_q_year     <- lubridate::year(last_q_end)
  last_q_num      <- lubridate::quarter(last_q_end)

  switch(period,

    quarterly = list(
      label        = paste0(last_q_year, "-Q", last_q_num),
      period_start = lubridate::floor_date(last_q_end, "quarter"),
      period_end   = last_q_end,
      chart_start  = NULL,   # NULL → use chart_months from today
      chart_end    = NULL,
      chart_months = 36L     # last 12 quarters for context
    ),

    ytd = {
      ytd_start <- as.Date(paste0(last_q_year, "-01-01"))
      list(
        label        = paste0(last_q_year, "-YTD"),
        period_start = ytd_start,
        period_end   = last_q_end,
        chart_start  = ytd_start,
        chart_end    = last_q_end,
        chart_months = NULL
      )
    },

    semiannual = {
      # last completed half-year: H1 ends Jun 30, H2 ends Dec 31
      if (last_q_num <= 2L) {
        h_year  <- last_q_year - 1L
        h_start <- as.Date(paste0(h_year, "-07-01"))
        h_end   <- as.Date(paste0(h_year, "-12-31"))
        h_label <- paste0(h_year, "-H2")
      } else {
        h_year  <- last_q_year
        h_start <- as.Date(paste0(h_year, "-01-01"))
        h_end   <- as.Date(paste0(h_year, "-06-30"))
        h_label <- paste0(h_year, "-H1")
      }
      list(
        label        = h_label,
        period_start = h_start,
        period_end   = h_end,
        chart_start  = h_start,
        chart_end    = h_end,
        chart_months = NULL
      )
    },

    annual = {
      # last fully completed calendar year
      ann_year  <- if (last_q_num == 4L) last_q_year else last_q_year - 1L
      ann_start <- as.Date(paste0(ann_year, "-01-01"))
      ann_end   <- as.Date(paste0(ann_year, "-12-31"))
      list(
        label        = as.character(ann_year),
        period_start = ann_start,
        period_end   = ann_end,
        chart_start  = ann_start,
        chart_end    = ann_end,
        chart_months = NULL
      )
    },

    stop("period must be one of: quarterly, ytd, semiannual, annual")
  )
}

#' Compute the return of a strategy over a date window from bt_paths
compute_period_return <- function(bt_paths, start_date, end_date, strategy = "Current") {
  if (is.null(bt_paths) || is.null(start_date) || is.null(end_date)) return(NA_real_)
  sub <- bt_paths[bt_paths$Strategy == strategy &
                  bt_paths$Date >= start_date &
                  bt_paths$Date <= end_date, ]
  if (nrow(sub) < 2L) return(NA_real_)
  sub <- sub[order(sub$Date), ]
  sub$Cum_Value[nrow(sub)] / sub$Cum_Value[1L] - 1
}


# --- Page 1: Summary table + macro narrative ----------------------------------

#' Build a ggplot summary page with key portfolio metrics and macro views
#'
#' @param ptf_name        Portfolio name
#' @param ptf_summary     data.frame from {ptf}_summary.parquet
#' @param bt_metrics      data.frame from {ptf}_bt_metrics.parquet
#' @param macro_views_txt Character vector from {ptf}_macro_views.txt
#' @param period_label    e.g. "2026-Q1"
#' @param period_return   Numeric return over the period (NA if unavailable)
plot_summary_page <- function(ptf_name, ptf_summary, bt_metrics,
                              macro_views_txt, period_label, period_return = NA) {

  # Helper: extract one metric from bt_metrics for a given strategy
  .bt_val <- function(strat, col) {
    if (is.null(bt_metrics)) return(NA_real_)
    row <- bt_metrics[bt_metrics$Strategy == strat, ]
    if (nrow(row) == 0L) return(NA_real_)
    row[[col]][1L]
  }

  max_dd      <- .bt_val("Current", "Max_Drawdown")
  calmar_ptf  <- .bt_val("Current",          "Calmar")
  calmar_6040 <- .bt_val("Benchmark_60_40",  "Calmar")
  calmar_msci <- .bt_val("MSCI_World_BnH",   "Calmar")

  # Calmar comparison string — show only available benchmarks
  calmar_str <- paste0("Ptf: ", if (!is.na(calmar_ptf)) sprintf("%.2f", calmar_ptf) else "N/A")
  if (!is.na(calmar_6040))
    calmar_str <- paste0(calmar_str, "  |  60/40: ",     sprintf("%.2f", calmar_6040))
  if (!is.na(calmar_msci))
    calmar_str <- paste0(calmar_str, "  |  MSCI World: ", sprintf("%.2f", calmar_msci))

  # 7 main metric rows — Calmar rendered separately below with benchmark context
  metrics <- data.frame(
    Label = c("Rendimento Periodo", "Rendimento Annualizzato", "Volatilita' (ann.)",
              "Sharpe Ratio", "VaR 95%", "ES 95%",
              "Max Drawdown (BT, mensile)"),
    Value = c(
      if (!is.na(period_return)) sprintf("%.2f%%", period_return * 100) else "N/A",
      sprintf("%.2f%%", ptf_summary$Annual_Ret   * 100),
      sprintf("%.2f%%", ptf_summary$Std_Dev_A     * 100),
      sprintf("%.2f",   ptf_summary$Sharpe_ratio),
      sprintf("%.2f%%", ptf_summary$VaR           * 100),
      sprintf("%.2f%%", ptf_summary$ES            * 100),
      if (!is.na(max_dd)) sprintf("%.2f%%", max_dd * 100) else "N/A"
    ),
    stringsAsFactors = FALSE
  )

  header <- paste0(toupper(ptf_name), " — Report  |  ", period_label)

  narrative_text <- if (length(macro_views_txt) == 0 ||
                        all(trimws(macro_views_txt) %in% c("", "(no active macro views)"))) {
    "Segnali macro: nessun segnale attivo (tutti gli indicatori neutrali o non disponibili)."
  } else {
    paste("Segnali macro:", paste(macro_views_txt, collapse = " | "))
  }

  calmar_y <- 1 - 8 * 0.083   # row-8 position for the benchmark Calmar line

  metrics$row <- seq_len(nrow(metrics))
  ggplot(metrics) +
    annotate("text", x = 0.5, y = 1.05, label = header,
             size = 6, fontface = "bold", hjust = 0.5) +
    annotate("text", x = 0.5, y = 0.97,
             label = "Metriche di Performance del Portafoglio",
             size = 4.5, colour = "grey40", hjust = 0.5) +
    geom_text(aes(x = 0.2, y = 1 - row * 0.083, label = Label),
              hjust = 0, size = 4, colour = "grey20") +
    geom_text(aes(x = 0.75, y = 1 - row * 0.083, label = Value),
              hjust = 0, size = 4, fontface = "bold", colour = "#2C3E50") +
    # Calmar row with benchmark comparison (smaller font to fit the longer string)
    annotate("text", x = 0.20, y = calmar_y,
             label = "Calmar Ratio (BT, mensile)",
             hjust = 0, size = 4, colour = "grey20") +
    annotate("text", x = 0.75, y = calmar_y,
             label = calmar_str,
             hjust = 0, size = 3.2, fontface = "bold", colour = "#2C3E50") +
    # Macro views
    annotate("text", x = 0.5, y = 0.16,
             label = strwrap(narrative_text, width = 90),
             size = 3.2, colour = "grey30", hjust = 0.5, vjust = 1) +
    # Disclaimer
    annotate("text", x = 0.5, y = 0.03,
             label = paste0(
               "Rendimento annualizzato e rischio calcolati sull'intera serie storica disponibile. ",
               "Drawdown e Calmar su ritorni mensili: il drawdown giornaliero reale puo' essere superiore del 20-40%. ",
               "VaR/ES al 95% su distribuzione storica mensile."
             ),
             size = 2.8, colour = "grey50", hjust = 0.5, fontface = "italic") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1.1)) +
    theme_void() +
    theme(plot.margin = margin(20, 20, 20, 20))
}


# --- Page 2: Current allocation pie ------------------------------------------

#' Pie chart of current portfolio allocation with asset-class-based pastel colours
plot_current_allocation <- function(assets, weights, ptf_name) {
  df        <- data.frame(assets = assets, weights = weights)
  df$assets <- factor(df$assets, levels = df$assets)

  asset_colours <- get_asset_colours(assets)

  ggplot(df, aes(x = "", y = weights, fill = assets)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = asset_colours) +
    geom_text(aes(label = scales::percent(weights, accuracy = 0.1)),
              position = position_stack(vjust = 0.5), size = 3.5) +
    labs(title   = paste0("Allocazione Attuale — ", toupper(ptf_name)),
         caption = paste0("Pesi correnti configurati nel portafoglio. ",
                          "Colori: rossi/arancio = equity  |  blu = obbligazioni  |  ",
                          "verde = commodity  |  grigio = cash."),
         fill    = "Strumento") +
    theme_void() +
    theme(plot.title   = element_text(hjust = 0.5, face = "bold", size = 13),
          plot.caption = element_text(colour = "grey50", size = 8, hjust = 0.5),
          legend.title = element_text(size = 9),
          legend.text  = element_text(size = 8))
}


# --- Page 3: Allocation comparison bar chart ----------------------------------

#' Grouped horizontal bar: current vs max-sharpe vs macro-tilted weights
#'
#' @param current_w    Named numeric vector of current weights
#' @param univ_w       data.frame with columns Asset, Weight, Method (from universe parquet)
#' @param macro_w      data.frame with columns Asset, Weight (from macro_universe parquet)
#' @param min_report   Minimum weight to display
plot_allocation_comparison <- function(current_w, univ_w, macro_w, min_report = 0.02) {

  cur_df <- data.frame(
    Asset    = names(current_w),
    Weight   = as.numeric(current_w),
    Scenario = "Current",
    stringsAsFactors = FALSE
  )

  ms_df <- if (!is.null(univ_w) && "Method" %in% names(univ_w)) {
    tmp <- univ_w[univ_w$Method == "Max_Sharpe", c("Asset", "Weight")]
    if (nrow(tmp) > 0) { tmp$Scenario <- "Max-Sharpe"; tmp } else NULL
  } else NULL

  mac_df <- if (!is.null(macro_w) && nrow(macro_w) > 0) {
    data.frame(Asset = macro_w$Asset, Weight = macro_w$Weight,
               Scenario = "Macro-tilted", stringsAsFactors = FALSE)
  } else NULL

  combined <- do.call(rbind, Filter(Negate(is.null), list(cur_df, ms_df, mac_df)))
  if (is.null(combined) || nrow(combined) == 0) return(NULL)

  visible_assets <- unique(combined$Asset[combined$Weight >= min_report])
  combined <- combined[combined$Asset %in% visible_assets, ]
  combined$Scenario <- factor(combined$Scenario,
                              levels = c("Current", "Max-Sharpe", "Macro-tilted"))

  # Order: current assets first (by weight desc), then new assets (by max suggested weight desc)
  cur_visible <- intersect(names(current_w), visible_assets)
  new_visible <- setdiff(visible_assets, names(current_w))
  cur_order   <- names(sort(current_w[cur_visible], decreasing = TRUE))
  if (length(new_visible) > 0) {
    new_max_w <- sapply(new_visible,
                        function(a) max(combined$Weight[combined$Asset == a], na.rm = TRUE))
    new_order <- new_visible[order(new_max_w, decreasing = TRUE)]
  } else {
    new_order <- character(0)
  }
  asset_order    <- c(cur_order, new_order)
  combined$Asset <- factor(combined$Asset, levels = rev(asset_order))

  ggplot(combined, aes(x = Asset, y = Weight, fill = Scenario)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.65) +
    geom_text(aes(label = scales::percent(Weight, accuracy = 0.1)),
              position = position_dodge(width = 0.75),
              hjust = -0.1, size = 3) +
    scale_fill_manual(values = REPORT_COLOURS) +
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.15))) +
    coord_flip() +
    labs(title    = "Confronto Allocazioni: Attuale vs Suggerita",
         subtitle = paste0(
           "Max-Sharpe: massimizza il rendimento per unita' di rischio (Sharpe). ",
           "Macro-tilted: Black-Litterman con viste da indicatori FRED (curva tassi, CPI, VIX). ",
           "I pesi rispettano i limiti di concentrazione e il numero massimo di strumenti."
         ),
         x = NULL, y = "Peso", fill = "Scenario") +
    theme_minimal(base_size = 11) +
    theme(plot.title      = element_text(face = "bold", size = 13),
          plot.subtitle   = element_text(colour = "grey40", size = 8),
          legend.position = "bottom")
}


# --- Page 4: Backtest — configurable window -----------------------------------

#' Plot backtest cumulative-value paths for a given date window
#'
#' @param bt_paths   data.frame with Date, Strategy, Cum_Value columns
#' @param start_date Date: start of window (NULL → use n_months from today)
#' @param end_date   Date: end of window (NULL → today)
#' @param n_months   Integer: fallback window in months when start_date is NULL
#' @param title      Plot title string
plot_backtest_recent <- function(bt_paths, start_date = NULL, end_date = NULL,
                                 n_months = 36L, title = "Backtest") {
  if (!is.null(start_date) && !is.null(end_date)) {
    bt_recent <- bt_paths[bt_paths$Date >= start_date & bt_paths$Date <= end_date, ]
  } else {
    cutoff    <- Sys.Date() - as.integer(round(n_months * 30.5))
    bt_recent <- bt_paths[bt_paths$Date >= cutoff, ]
  }
  if (nrow(bt_recent) == 0) bt_recent <- bt_paths

  bt_recent <- bt_recent %>%
    group_by(Strategy) %>%
    mutate(Cum_Value = Cum_Value / first(Cum_Value) * 100) %>%
    ungroup()

  ggplot(bt_recent, aes(x = Date, y = Cum_Value, colour = Strategy)) +
    geom_line(data = bt_recent[bt_recent$Strategy != "Current", ], linewidth = 0.7) +
    geom_line(data = bt_recent[bt_recent$Strategy == "Current", ], linewidth = 1.8) +
    scale_colour_manual(values = STRATEGY_COLOURS, breaks = names(STRATEGY_COLOURS)) +
    labs(title    = title,
         subtitle = paste0(
           "Backtest walk-forward: ogni strategia e' ribilanciata periodicamente usando solo ",
           "i dati disponibili a quel momento (no look-ahead bias). ",
           "Il portafoglio Corrente e' evidenziato con la linea piu' spessa."
         ),
         x      = NULL,
         y      = "Valore (base 100 inizio finestra)",
         colour = "Strategia") +
    theme_minimal(base_size = 11) +
    theme(plot.title      = element_text(face = "bold", size = 13),
          plot.subtitle   = element_text(colour = "grey40", size = 8),
          legend.position = "bottom")
}


# --- Page 5: Monte Carlo forecast --------------------------------------------

#' Rebuild the MC fan-chart from saved forecast_ts parquet
plot_mc_from_parquet <- function(mc_ts, ptf_name) {
  hist_df     <- mc_ts[is.na(mc_ts$P5), ]
  forecast_df <- mc_ts[!is.na(mc_ts$P5), ]

  ggplot() +
    geom_line(data = hist_df,
              aes(x = Dates, y = Mean), colour = "#5D7FA8", linewidth = 0.8) +
    geom_ribbon(data = forecast_df,
                aes(x = Dates, ymin = P5, ymax = P95),
                fill = "#6ABBB0", alpha = 0.25) +
    geom_line(data = forecast_df,
              aes(x = Dates, y = Mean),
              colour = "#6ABBB0", linewidth = 0.9, linetype = "dashed") +
    labs(title    = paste0("Proiezione Monte Carlo 12 Mesi — ", toupper(ptf_name)),
         subtitle = paste0(
           "Simulazione bootstrap storico (1000 scenari). ",
           "La banda rappresenta il 5° e 95° percentile; ",
           "la linea tratteggiata e' il valore atteso mediano."
         ),
         x = NULL, y = "Valore portafoglio (base 100)") +
    theme_minimal(base_size = 11) +
    theme(plot.title    = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(colour = "grey40", size = 10))
}


# --- Page 0: Goal-based projection scenarios (if portfolio has a goal) ----------

#' Project portfolio value for 3 CF scenarios over the goal horizon
#'
#' Bull:     CF increases by `step` euros/month each year
#' Standard: CF stays constant
#' Bear:     CF decreases by `step` euros/month each year, floored at `bear_floor`
project_scenarios <- function(pv, t_years, cf_base, ann_ret,
                               step = 50, bear_floor = 200) {
  r_m <- (1 + ann_ret)^(1 / 12) - 1
  n   <- as.integer(round(t_years * 12))

  v_bull <- v_std <- v_bear <- numeric(n + 1L)
  v_bull[1L] <- v_std[1L] <- v_bear[1L] <- pv

  for (m in seq_len(n)) {
    yr             <- floor((m - 1L) / 12)
    cf_bull        <- cf_base + yr * step
    cf_bear        <- max(cf_base - yr * step, bear_floor)
    v_bull[m + 1L] <- v_bull[m] * (1 + r_m) + cf_bull
    v_std[m + 1L]  <- v_std[m]  * (1 + r_m) + cf_base
    v_bear[m + 1L] <- v_bear[m] * (1 + r_m) + cf_bear
  }

  data.frame(Year = (0:n) / 12, Bull = v_bull, Standard = v_std, Bear = v_bear)
}

#' Compute monthly CF required to reach FV with a constant annual expected return
compute_required_cf <- function(pv, fv, t_years, ann_ret) {
  r_m <- (1 + ann_ret)^(1 / 12) - 1
  n   <- as.integer(round(t_years * 12))
  if (abs(r_m) < 1e-10) return((fv - pv) / n)
  fv_growth      <- pv * (1 + r_m)^n
  annuity_factor <- ((1 + r_m)^n - 1) / r_m
  (fv - fv_growth) / annuity_factor
}

#' Goal scenario line-chart: Bull / Standard / Bear paths vs FV target
plot_goal_scenarios <- function(ptf_name, goal_cfg, ptf_summary, period_label,
                                step = 50, bear_floor = 200) {
  pv      <- goal_cfg$PV
  fv      <- goal_cfg$FV
  t_years <- goal_cfg$t
  cf_base <- goal_cfg$CF
  ann_ret <- ptf_summary$Annual_Ret

  paths  <- project_scenarios(pv, t_years, cf_base, ann_ret, step, bear_floor)
  req_cf <- compute_required_cf(pv, fv, t_years, ann_ret)

  long          <- tidyr::pivot_longer(paths, cols = c("Bull", "Standard", "Bear"),
                                       names_to = "Scenario", values_to = "Value")
  long$Scenario <- factor(long$Scenario, levels = c("Bull", "Standard", "Bear"))

  fmt <- function(x) format(round(x), big.mark = ".", scientific = FALSE)

  tv_bull <- fmt(tail(paths$Bull,     1L))
  tv_std  <- fmt(tail(paths$Standard, 1L))
  tv_bear <- fmt(tail(paths$Bear,     1L))

  subtitle <- sprintf(
    paste0(
      "Obiettivo: €%s  |  Valore attuale: €%s  |  Orizzonte: %d anni  |  ",
      "CF attuale: €%d/mese  |  Rendimento atteso: %.1f%%\n",
      "CF necessario per raggiungere l'obiettivo: €%.0f/mese  |  ",
      "Bull: +€%d/anno  |  Bear: min €%d/mese\n",
      "Valore finale atteso  ·  Bull: €%s  ·  Standard: €%s  ·  Bear: €%s"
    ),
    fmt(fv), fmt(pv), t_years, cf_base, ann_ret * 100,
    req_cf, step, bear_floor,
    tv_bull, tv_std, tv_bear
  )

  scen_colours <- c("Bull" = "#78B87A", "Standard" = "#5D7FA8", "Bear" = "#E08080")

  ggplot(long, aes(x = Year, y = Value / 1e3, colour = Scenario)) +
    geom_hline(yintercept = fv / 1e3, linetype = "dashed",
               colour = "grey50", linewidth = 0.7) +
    annotate("text", x = 0, y = fv / 1e3 * 1.03,
             label = paste0("Obiettivo: €", fmt(fv)),
             hjust = 0, size = 3.2, colour = "grey50") +
    geom_line(linewidth = 1.2) +
    scale_colour_manual(values = scen_colours) +
    scale_y_continuous(
      labels = function(x) paste0("€", format(round(x), big.mark = ".", scientific = FALSE), "k")
    ) +
    scale_x_continuous(
      breaks = seq(0, t_years, by = max(1L, as.integer(t_years / 7L)))
    ) +
    labs(
      title    = paste0("Proiezione verso Obiettivo — ", toupper(ptf_name),
                        " | ", period_label),
      subtitle = subtitle,
      x        = "Anni",
      y        = "Valore (€ migliaia)",
      colour   = "Scenario"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title      = element_text(face = "bold", size = 13),
      plot.subtitle   = element_text(colour = "grey40", size = 8.5),
      legend.position = "bottom"
    )
}


# --- Page macro: Macro indicators dashboard + view confidence -----------------

#' Full-page macro dashboard: indicators table (left) + BL views & confidence (right)
#'
#' @param ptf_name       Portfolio name
#' @param indicators_df  data.frame(Indicator, Label, Value, Date, Unit) from parquet
#' @param cfg_rules      cfg$macro_rules list
#' @param period_label   e.g. "2026-Q1"
plot_macro_page <- function(ptf_name, indicators_df, cfg_rules, period_label) {

  # --- Helpers ----------------------------------------------------------------
  get_ind <- function(id) {
    r <- indicators_df[indicators_df$Indicator == id, ]
    if (nrow(r) == 0L) list(value = NA_real_, date = NA_character_)
    else list(value = r$Value[1L], date = r$Date[1L])
  }
  fmt_date <- function(d) {
    if (is.na(d) || identical(d, "NA")) return("")
    tryCatch(format(as.Date(d), "%b %d"), error = function(e) d)
  }

  yc_i  <- get_ind("Yield_Curve_10Y2Y")
  cpi_i <- get_ind("CPI_YoY")
  ff_i  <- get_ind("Fed_Funds")
  hy_i  <- get_ind("HY_Spread")
  be_i  <- get_ind("Breakeven_5Y")
  vix_i <- get_ind("VIX")

  yc_v  <- yc_i$value;  cpi_v <- cpi_i$value; ff_v <- ff_i$value
  hy_v  <- hy_i$value;  be_v  <- be_i$value;  vix_v <- vix_i$value

  # --- Classify regimes -------------------------------------------------------
  yc_regime <- if (is.na(yc_v)) {
    list(badge = "N/D",                  col = "grey85",   active = FALSE,
         view  = "Dati non disponibili")
  } else if (yc_v < cfg_rules$yield_curve$inversion_strong) {
    list(badge = "Forte inversione",      col = "#E08080",  active = TRUE,
         view  = sprintf("ATTIVA — Bond outperform equity di +%.0f%% (conf %.0f%%)",
                         cfg_rules$yield_curve$Q_recession * 100,
                         cfg_rules$yield_curve$conf_recession * 100))
  } else if (yc_v < cfg_rules$yield_curve$flat) {
    list(badge = "Piatta/invertita",      col = "#F4C09A",  active = FALSE,
         view  = "Non attiva — inversione lieve, segnale ambiguo")
  } else if (yc_v > cfg_rules$yield_curve$steep) {
    list(badge = "Ripida (espansione)",   col = "#A8CC98",  active = TRUE,
         view  = sprintf("ATTIVA — Equity outperform bond di +%.0f%% (conf %.0f%%)",
                         cfg_rules$yield_curve$Q_expansion * 100,
                         cfg_rules$yield_curve$conf_expansion * 100))
  } else {
    list(badge = "Normale (neutrale)",    col = "#D4E8D4",  active = FALSE,
         view  = "Non attiva — curva nella zona neutra")
  }

  cpi_regime <- if (is.na(cpi_v)) {
    list(badge = "N/D",                  col = "grey85",   active = FALSE,
         view  = "Dati non disponibili")
  } else if (cpi_v > cfg_rules$inflation$high) {
    list(badge = "Alta inflazione",       col = "#E08080",  active = TRUE,
         view  = sprintf("ATTIVA — Gold expected +%.0f%% (conf %.0f%%)",
                         cfg_rules$inflation$Q_high * 100,
                         cfg_rules$inflation$conf_high * 100))
  } else if (cpi_v > cfg_rules$inflation$moderate) {
    list(badge = "Inflaz. moderata",      col = "#F4C09A",  active = TRUE,
         view  = sprintf("ATTIVA — Gold expected +%.0f%% (conf %.0f%%)",
                         cfg_rules$inflation$Q_moderate * 100,
                         cfg_rules$inflation$conf_moderate * 100))
  } else {
    list(badge = "Inflaz. bassa",         col = "#A8CC98",  active = FALSE,
         view  = "Non attiva — inflazione sotto soglia moderata")
  }

  ff_regime <- if (is.na(ff_v)) {
    list(badge = "N/D",                  col = "grey85",   active = FALSE,
         view  = "Non attiva — dati Fed Funds non disponibili")
  } else {
    list(badge = "Disponibile",           col = "#D4E8D4",  active = TRUE,
         view  = sprintf("ATTIVA — Cash expected %.2f%% (conf %.0f%%)",
                         ff_v, cfg_rules$cash$confidence * 100))
  }

  # Text colour: dim inactive views
  vc <- function(active) if (active) "#2C3E50" else "grey60"

  # --- Indicator table rows --------------------------------------------------
  ind_rows <- data.frame(
    y      = c(0.80, 0.72, 0.64, 0.56, 0.48, 0.40),
    label  = c("Curva Tassi (10Y-2Y)", "CPI (Inflaz. YoY)", "Fed Funds Rate",
               "HY Spread (OAS)", "Breakeven 5Y", "VIX"),
    value  = c(
      if (is.na(yc_v))  "N/D" else sprintf("%+.2f%%", yc_v),
      if (is.na(cpi_v)) "N/D" else sprintf("%.2f%%",  cpi_v),
      if (is.na(ff_v))  "N/D" else sprintf("%.2f%%",  ff_v),
      if (is.na(hy_v))  "N/D" else sprintf("%.2f%%",  hy_v),
      if (is.na(be_v))  "N/D" else sprintf("%.2f%%",  be_v),
      if (is.na(vix_v)) "N/D" else sprintf("%.1f",    vix_v)
    ),
    date   = c(fmt_date(yc_i$date), fmt_date(cpi_i$date), fmt_date(ff_i$date),
               fmt_date(hy_i$date), fmt_date(be_i$date),  fmt_date(vix_i$date)),
    badge  = c(yc_regime$badge, cpi_regime$badge, ff_regime$badge, "—", "—", "—"),
    bcolor = c(yc_regime$col,   cpi_regime$col,   ff_regime$col,
               "grey91", "grey91", "grey91"),
    stringsAsFactors = FALSE
  )

  # --- Build canvas -----------------------------------------------------------
  gg <- ggplot() +
    # Main title
    annotate("text", x = 0.5, y = 1.06,
             label = paste0("MACRO INDICATORI & VISTE — ", toupper(ptf_name),
                            " | ", period_label),
             size = 6, fontface = "bold", hjust = 0.5, colour = "#1A2530") +

    # Vertical separator
    annotate("segment", x = 0.50, xend = 0.50, y = 0.04, yend = 0.93,
             colour = "grey72", linewidth = 0.5) +

    # ===== LEFT COLUMN: indicators table ======================================
    annotate("text", x = 0.01, y = 0.91,
             label = "INDICATORI MACROECONOMICI (FRED)",
             size = 3.9, fontface = "bold", colour = "#2C3E50", hjust = 0) +
    annotate("segment", x = 0.01, xend = 0.49, y = 0.88, yend = 0.88,
             colour = "grey70", linewidth = 0.4) +

    # ===== RIGHT COLUMN: views ================================================
    annotate("text", x = 0.52, y = 0.91,
             label = "VISTE ATTIVE & LIVELLI DI CONFIDENZA",
             size = 3.9, fontface = "bold", colour = "#2C3E50", hjust = 0) +
    annotate("segment", x = 0.52, xend = 0.99, y = 0.88, yend = 0.88,
             colour = "grey70", linewidth = 0.4)

  # --- Left: indicator rows --------------------------------------------------
  for (i in seq_len(nrow(ind_rows))) {
    yy <- ind_rows$y[i]
    gg <- gg +
      annotate("rect",
               xmin = 0.01, xmax = 0.205, ymin = yy - 0.026, ymax = yy + 0.026,
               fill = ind_rows$bcolor[i], alpha = 0.75) +
      annotate("text", x = 0.108, y = yy,
               label = ind_rows$badge[i], size = 2.6, hjust = 0.5, colour = "grey20") +
      annotate("text", x = 0.215, y = yy,
               label = ind_rows$label[i], size = 3.4, hjust = 0, colour = "grey25") +
      annotate("text", x = 0.385, y = yy,
               label = ind_rows$value[i], size = 3.8, hjust = 0.5,
               fontface = "bold", colour = "#2C3E50") +
      annotate("text", x = 0.475, y = yy,
               label = ind_rows$date[i], size = 2.8, hjust = 1, colour = "grey55")
  }

  # Left: thresholds & BL parameters legend
  gg <- gg +
    annotate("text", x = 0.01, y = 0.29,
             label = "Soglie di configurazione",
             size = 3.4, fontface = "bold", colour = "#2C3E50", hjust = 0) +
    annotate("text", x = 0.01, y = 0.24,
             label = sprintf(
               "Curva tassi:   < %.2f%%  forte inversione   < %.2f%%  piatta   > %.2f%%  espansione",
               cfg_rules$yield_curve$inversion_strong,
               cfg_rules$yield_curve$flat,
               cfg_rules$yield_curve$steep),
             size = 2.9, hjust = 0, colour = "grey38") +
    annotate("text", x = 0.01, y = 0.20,
             label = sprintf(
               "CPI YoY:       > %.1f%%  alta inflazione   > %.1f%%  moderata",
               cfg_rules$inflation$high, cfg_rules$inflation$moderate),
             size = 2.9, hjust = 0, colour = "grey38") +
    annotate("text", x = 0.01, y = 0.16,
             label = "Cash:          Fed Funds rate (FRED), vista attiva quando il dato è disponibile",
             size = 2.9, hjust = 0, colour = "grey38") +
    annotate("segment", x = 0.01, xend = 0.49, y = 0.12, yend = 0.12,
             colour = "grey82", linewidth = 0.3) +
    annotate("text", x = 0.01, y = 0.09,
             label = sprintf(
               "Black-Litterman:   tau = %.3f (incertezza prior, tipico 0.025-0.05)   lambda = %.1f (avversione al rischio)",
               cfg_rules$tau, cfg_rules$lambda),
             size = 2.9, hjust = 0, colour = "grey45") +
    annotate("text", x = 0.01, y = 0.05,
             label = paste0(
               "tau alto -> piu' peso alle viste personali vs equilibrio di mercato.  ",
               "lambda = standard accademico (Black-Litterman 1992, He-Litterman 1999)."),
             size = 2.7, hjust = 0, colour = "grey55", fontface = "italic")

  # --- Right: three view blocks ----------------------------------------------
  # View 1 — Yield curve
  gg <- gg +
    annotate("text", x = 0.52, y = 0.84,
             label = sprintf("1.  EQUITY vs BOND  —  Curva Tassi 10Y-2Y  (%s)",
                             if (is.na(yc_v)) "N/D" else sprintf("%+.2f%%", yc_v)),
             size = 3.5, fontface = "bold", hjust = 0, colour = vc(yc_regime$active)) +
    annotate("text", x = 0.54, y = 0.79,
             label = yc_regime$view,
             size = 3.2, hjust = 0, colour = vc(yc_regime$active)) +
    annotate("text", x = 0.54, y = 0.74,
             label = sprintf(
               "Conf. recessione  %.0f%%  —  segnale storico forte (~90%% dei cicli passati),",
               cfg_rules$yield_curve$conf_recession * 100),
             size = 2.85, hjust = 0, colour = "grey42") +
    annotate("text", x = 0.54, y = 0.70,
             label = paste0(
               "ma il lead time è variabile (6–24 mesi); rischio falso positivo (2023)."),
             size = 2.85, hjust = 0, colour = "grey42") +
    annotate("text", x = 0.54, y = 0.66,
             label = sprintf(
               "Conf. espansione  %.0f%%  —  curva ripida in contesto geopolitico può riflettere",
               cfg_rules$yield_curve$conf_expansion * 100),
             size = 2.85, hjust = 0, colour = "grey42") +
    annotate("text", x = 0.54, y = 0.62,
             label = "inflazione da offerta, non crescita reale; confidenza ridotta rispetto al caso normale.",
             size = 2.85, hjust = 0, colour = "grey42") +
    annotate("segment", x = 0.52, xend = 0.99, y = 0.58, yend = 0.58,
             colour = "grey87", linewidth = 0.3)

  # View 2 — Inflation / Gold
  gg <- gg +
    annotate("text", x = 0.52, y = 0.55,
             label = sprintf("2.  ORO  —  CPI YoY  (%s)",
                             if (is.na(cpi_v)) "N/D" else sprintf("%.2f%%", cpi_v)),
             size = 3.5, fontface = "bold", hjust = 0, colour = vc(cpi_regime$active)) +
    annotate("text", x = 0.54, y = 0.50,
             label = cpi_regime$view,
             size = 3.2, hjust = 0, colour = vc(cpi_regime$active)) +
    annotate("text", x = 0.54, y = 0.45,
             label = sprintf(
               "Conf. alta (>%.0f%%)  %.0f%%  —  inflazione elevata supporta l'oro storicamente,",
               cfg_rules$inflation$high, cfg_rules$inflation$conf_high * 100),
             size = 2.85, hjust = 0, colour = "grey42") +
    annotate("text", x = 0.54, y = 0.41,
             label = "ma il rischio stagflazione e l'incertezza geopolitica moderano la vista.",
             size = 2.85, hjust = 0, colour = "grey42") +
    annotate("text", x = 0.54, y = 0.37,
             label = sprintf(
               "Conf. moderata (>%.0f%%)  %.0f%%  —  effetto oro meno certo; normalizzazione verso",
               cfg_rules$inflation$moderate, cfg_rules$inflation$conf_moderate * 100),
             size = 2.85, hjust = 0, colour = "grey42") +
    annotate("text", x = 0.54, y = 0.33,
             label = "target BCE/Fed (~2%) possibile; confidenza contenuta per non sovrappesare l'oro.",
             size = 2.85, hjust = 0, colour = "grey42") +
    annotate("segment", x = 0.52, xend = 0.99, y = 0.29, yend = 0.29,
             colour = "grey87", linewidth = 0.3)

  # View 3 — Cash / Fed Funds
  gg <- gg +
    annotate("text", x = 0.52, y = 0.26,
             label = sprintf("3.  CASH  —  Fed Funds Rate  (%s)",
                             if (is.na(ff_v)) "N/D" else sprintf("%.2f%%", ff_v)),
             size = 3.5, fontface = "bold", hjust = 0, colour = vc(ff_regime$active)) +
    annotate("text", x = 0.54, y = 0.21,
             label = ff_regime$view,
             size = 3.2, hjust = 0, colour = vc(ff_regime$active)) +
    annotate("text", x = 0.54, y = 0.16,
             label = sprintf(
               "Conf.  %.0f%%  —  il tasso è pubblicato dalla Fed e noto quasi in real-time.",
               cfg_rules$cash$confidence * 100),
             size = 2.85, hjust = 0, colour = "grey42") +
    annotate("text", x = 0.54, y = 0.12,
             label = paste0(
               "Rappresenta il rendimento atteso dei money market / overnight nel portafoglio. ",
               "Alta confidenza strutturale"),
             size = 2.85, hjust = 0, colour = "grey42") +
    annotate("text", x = 0.54, y = 0.08,
             label = "perché il tasso è uno dei pochi dati macro disponibili senza ritardo significativo.",
             size = 2.85, hjust = 0, colour = "grey42")

  gg +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1.1)) +
    theme_void() +
    theme(plot.margin = margin(15, 15, 15, 15))
}


# --- Master report function ---------------------------------------------------

#' Generate a 5-page PDF report for one portfolio and one time period
#'
#' Reads all data from already-saved parquet files in output_path.
#' Output file: reports_path/{ptf_name}/{period_label}_{ptf_name}.pdf
#'
#' @param ptf_name      Portfolio name (e.g. "chiara")
#' @param cfg           Config list (from yaml::read_yaml)
#' @param output_path   Path where parquet outputs live (e.g. "output/")
#' @param reports_path  Root folder for reports (e.g. "reports/")
#' @param period        One of "quarterly", "ytd", "semiannual", "annual"
generate_portfolio_report <- function(ptf_name, cfg,
                                      output_path  = "output/",
                                      reports_path = "reports/",
                                      period       = "quarterly") {

  pinfo        <- get_period_info(period)
  period_label <- pinfo$label

  report_dir <- paste0(reports_path, ptf_name, "/")
  if (!dir.exists(report_dir)) dir.create(report_dir, recursive = TRUE)
  pdf_path <- paste0(report_dir, period_label, "_", ptf_name, ".pdf")

  # ---- Load data from parquets -----------------------------------------------
  ptf_summary <- tryCatch(
    arrow::read_parquet(paste0(output_path, ptf_name, "_summary.parquet")),
    error = function(e) NULL
  )
  bt_paths <- tryCatch(
    arrow::read_parquet(paste0(output_path, ptf_name, "_bt_paths.parquet")),
    error = function(e) NULL
  )
  bt_metrics <- tryCatch(
    arrow::read_parquet(paste0(output_path, ptf_name, "_bt_metrics.parquet")),
    error = function(e) NULL
  )
  univ_w <- tryCatch(
    arrow::read_parquet(paste0(output_path, ptf_name, "_universe_weights.parquet")),
    error = function(e) NULL
  )
  macro_w <- tryCatch(
    arrow::read_parquet(paste0(output_path, ptf_name, "_macro_universe_weights.parquet")),
    error = function(e) NULL
  )
  mc_ts <- tryCatch(
    arrow::read_parquet(paste0(output_path, ptf_name, "_mc_forecast.parquet")) %>%
      arrange(Dates),
    error = function(e) NULL
  )
  macro_views_txt <- tryCatch(
    readLines(paste0(output_path, ptf_name, "_macro_views.txt"), warn = FALSE),
    error = function(e) character(0)
  )
  macro_indicators <- tryCatch(
    arrow::read_parquet(paste0(output_path, ptf_name, "_macro_indicators.parquet")),
    error = function(e) NULL
  )

  ptf_cfg   <- cfg$portfolios[[ptf_name]]
  current_w <- setNames(as.numeric(ptf_cfg$quotes), ptf_cfg$assets)

  # Period return from bt_paths filtered to the period window
  period_return <- compute_period_return(bt_paths, pinfo$period_start, pinfo$period_end)

  # ---- Build plots -----------------------------------------------------------

  # Page 0 — Goal projection scenarios (only if the portfolio has a goal block)
  p0 <- if (!is.null(ptf_cfg$goal) && !is.null(ptf_summary)) {
    tryCatch(
      plot_goal_scenarios(ptf_name, ptf_cfg$goal, ptf_summary, period_label),
      error = function(e) {
        warning(sprintf("Goal page failed for %s: %s", ptf_name, e$message))
        NULL
      }
    )
  } else NULL

  # Page 1 — Summary + macro narrative
  p1 <- if (!is.null(ptf_summary)) {
    plot_summary_page(ptf_name, ptf_summary, bt_metrics,
                      macro_views_txt, period_label, period_return)
  } else {
    ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Summary data unavailable") + theme_void()
  }

  # Page 1b — Macro indicators & view confidence (only if indicator data exists)
  p_macro <- if (!is.null(macro_indicators)) {
    tryCatch(
      plot_macro_page(ptf_name, macro_indicators, cfg$macro_rules, period_label),
      error = function(e) {
        warning(sprintf("Macro page failed for %s: %s", ptf_name, e$message))
        NULL
      }
    )
  } else NULL

  # Page 2 — Current allocation pie
  p2 <- plot_current_allocation(names(current_w), as.numeric(current_w), ptf_name)

  # Page 3 — Allocation comparison
  p3 <- plot_allocation_comparison(
    current_w  = current_w,
    univ_w     = univ_w,
    macro_w    = macro_w,
    min_report = cfg$universe_optimization$min_weight_report %||% 0.02
  )
  if (is.null(p3)) {
    p3 <- ggplot() +
      annotate("text", x = 0.5, y = 0.5, label = "Universe optimization data unavailable") +
      theme_void()
  }

  # Page 4 — Backtest window per period type
  # Quarterly: show last 36 months for context; YTD/semi/annual: show the specific period
  p4 <- if (!is.null(bt_paths)) {
    plot_backtest_recent(
      bt_paths   = bt_paths,
      start_date = pinfo$chart_start,
      end_date   = pinfo$chart_end,
      n_months   = pinfo$chart_months %||% 36L,
      title      = paste0("Backtest | ", toupper(ptf_name), " — ", period_label)
    )
  } else {
    ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Backtest data unavailable") + theme_void()
  }

  # Page 5 — Monte Carlo 12-month forecast
  p5 <- if (!is.null(mc_ts)) {
    plot_mc_from_parquet(mc_ts, ptf_name)
  } else {
    ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Monte Carlo data unavailable") + theme_void()
  }

  # ---- Write PDF -------------------------------------------------------------
  pdf(pdf_path, width = 11, height = 7.5, paper = "a4r")
  if (!is.null(p0))     print(p0)
  print(p1)
  if (!is.null(p_macro)) print(p_macro)
  print(p2)
  print(p3)
  print(p4)
  print(p5)
  dev.off()

  invisible(pdf_path)
}
