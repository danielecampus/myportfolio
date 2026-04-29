# =============================================================================
# R/fun_report.R
# 9-page quarterly PDF report for each portfolio.
# Reads saved parquet files from output/ — does NOT re-run optimizations.
#
# Page 1 — Executive Summary
# Page 2 — Allocation & Risk Decomposition
# Page 3 — Correlation Matrix
# Page 4 — Macro Context & Black-Litterman Views
# Page 5 — Allocation Comparison (current vs optimized scenarios)
# Page 6 — Backtest & Drawdown
# Page 7 — Monte Carlo Projection
# Page 8 — Goal Projection (calibrated Bear / Base / Bull)
# Page 9 — Methodology & Disclaimer
# =============================================================================

if (!exists("%||%")) `%||%` <- function(x, y) if (!is.null(x)) x else y

fmt_eur <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("N/A")
  x <- as.numeric(x)
  if (abs(x) >= 1e6)      sprintf("%.2fM", x / 1e6)
  else if (abs(x) >= 1e3) sprintf("%.0fk", x / 1e3)
  else                    sprintf("%.0f", x)
}


# --- Colour palettes ----------------------------------------------------------

REPORT_COLOURS <- c(
  "Current"      = "#5D7FA8",
  "Max-Sharpe"   = "#6ABBB0",
  "Macro-tilted" = "#78B87A",
  "Min-Variance" = "#B07FBA"
)

STRATEGY_COLOURS <- c(
  "Current"          = "#5D7FA8",
  "Max_Sharpe"       = "#6ABBB0",
  "Min_Variance"     = "#B07FBA",
  "Equal_Weight"     = "#E0A070",
  "Benchmark_60_40"  = "#9EAAB2",
  "MSCI_World_BnH"   = "#D4904A"
)

ASSET_CLASS_COLOURS <- list(
  Equity    = c("#F4A9A9", "#F5C4A0", "#F8DFA0", "#F9B890", "#F5D0C0", "#EDB89A"),
  Bond      = c("#9DC4D8", "#85B0CC", "#B8D4E5", "#7AAFC0", "#C5DDE8"),
  Commodity = c("#A8CC98", "#C2D8A8", "#8DB88D"),
  Cash      = c("#CBCCCC", "#B8BCBF", "#D5D8D8"),
  Other     = c("#C8AACC", "#B8A0C8", "#D8BDE0")
)

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

get_asset_colours <- function(asset_names) {
  classes   <- vapply(asset_names, classify_asset_simple, character(1L))
  cls_count <- setNames(integer(length(ASSET_CLASS_COLOURS)), names(ASSET_CLASS_COLOURS))
  colours   <- character(length(asset_names))
  for (i in seq_along(asset_names)) {
    cls        <- classes[[i]]
    palette    <- ASSET_CLASS_COLOURS[[cls]]
    if (is.null(palette)) palette <- ASSET_CLASS_COLOURS$Other
    idx        <- (cls_count[[cls]] %% length(palette)) + 1L
    cls_count[cls] <- cls_count[[cls]] + 1L
    colours[i] <- palette[idx]
  }
  setNames(colours, asset_names)
}


# --- Period info + helpers ----------------------------------------------------

get_period_info <- function(period = "quarterly") {
  today           <- Sys.Date()
  q_start_current <- lubridate::floor_date(today, "quarter")
  last_q_end      <- q_start_current - 1L
  last_q_year     <- lubridate::year(last_q_end)
  last_q_num      <- lubridate::quarter(last_q_end)

  switch(period,
    quarterly = list(
      label        = paste0(last_q_year, "-Q", last_q_num),
      period_start = lubridate::floor_date(last_q_end, "quarter"),
      period_end   = last_q_end,
      chart_start  = NULL, chart_end = NULL, chart_months = 36L
    ),
    ytd = {
      s <- as.Date(paste0(last_q_year, "-01-01"))
      list(label = paste0(last_q_year, "-YTD"), period_start = s, period_end = last_q_end,
           chart_start = s, chart_end = last_q_end, chart_months = NULL)
    },
    semiannual = {
      if (last_q_num <= 2L) {
        hy <- last_q_year - 1L
        list(label = paste0(hy, "-H2"), period_start = as.Date(paste0(hy, "-07-01")),
             period_end = as.Date(paste0(hy, "-12-31")),
             chart_start = as.Date(paste0(hy, "-07-01")),
             chart_end   = as.Date(paste0(hy, "-12-31")), chart_months = NULL)
      } else {
        list(label = paste0(last_q_year, "-H1"),
             period_start = as.Date(paste0(last_q_year, "-01-01")),
             period_end   = as.Date(paste0(last_q_year, "-06-30")),
             chart_start  = as.Date(paste0(last_q_year, "-01-01")),
             chart_end    = as.Date(paste0(last_q_year, "-06-30")), chart_months = NULL)
      }
    },
    annual = {
      yr <- if (last_q_num == 4L) last_q_year else last_q_year - 1L
      list(label = as.character(yr),
           period_start = as.Date(paste0(yr, "-01-01")),
           period_end   = as.Date(paste0(yr, "-12-31")),
           chart_start  = as.Date(paste0(yr, "-01-01")),
           chart_end    = as.Date(paste0(yr, "-12-31")), chart_months = NULL)
    },
    stop("period must be: quarterly, ytd, semiannual, annual")
  )
}

compute_period_return <- function(bt_paths, start_date, end_date, strategy = "Current") {
  if (is.null(bt_paths) || is.null(start_date) || is.null(end_date)) return(NA_real_)
  sub <- bt_paths[bt_paths$Strategy == strategy &
                  bt_paths$Date >= start_date & bt_paths$Date <= end_date, ]
  if (nrow(sub) < 2L) return(NA_real_)
  sub <- sub[order(sub$Date), ]
  sub$Cum_Value[nrow(sub)] / sub$Cum_Value[1L] - 1
}

.footer <- function(ptf_name) {
  paste0(toupper(ptf_name), " — Report | ", lubridate::year(Sys.Date()),
         "  |  danielecampus.eu")
}

.print_page <- function(x) {
  if (inherits(x, "ggplot")) {
    print(x)
  } else {
    grid::grid.newpage()
    grid::grid.draw(x)
  }
}

.minvar_weights <- function(Sigma) {
  n    <- nrow(Sigma)
  Dmat <- 2 * Sigma
  dvec <- rep(0, n)
  Amat <- cbind(rep(1, n), diag(n))
  bvec <- c(1, rep(0, n))
  tryCatch({
    sol <- quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq = 1)
    w   <- sol$solution; w[w < 1e-8] <- 0; w / sum(w)
  }, error = function(e) rep(1 / n, n))
}

.long_to_matrix <- function(df, value_col) {
  assets <- unique(df$Asset1)
  n      <- length(assets)
  M      <- matrix(0, n, n, dimnames = list(assets, assets))
  for (i in seq_len(nrow(df))) M[df$Asset1[i], df$Asset2[i]] <- df[[value_col]][i]
  M
}

.placeholder <- function(msg) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = msg,
             size = 4.5, colour = "grey50", hjust = 0.5) +
    theme_void()
}

.bt_metric <- function(bt_metrics, strat, col) {
  if (is.null(bt_metrics)) return(NA_real_)
  row <- bt_metrics[bt_metrics$Strategy == strat, ]
  if (nrow(row) == 0L) return(NA_real_)
  row[[col]][1L]
}


# =============================================================================
# TITLE PAGE
# =============================================================================

plot_title_page <- function(ptf_name, cfg_ptf, period_label) {
  goal_line <- if (!is.null(cfg_ptf$goal)) {
    sprintf("Obiettivo: EUR %s  |  Valore attuale (PV): EUR %s  |  Orizzonte: %d anni  |  CF mensile: EUR %s/mese",
            fmt_eur(cfg_ptf$goal$FV), fmt_eur(cfg_ptf$goal$PV),
            cfg_ptf$goal$t, fmt_eur(cfg_ptf$goal$CF))
  } else "Portfolio senza obiettivo specifico"

  ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0.78, ymax = 1,
             fill = "#1A2530", alpha = 1) +
    annotate("text", x = 0.5, y = 0.91,
             label = toupper(ptf_name),
             size = 22, fontface = "bold", hjust = 0.5, colour = "white") +
    annotate("text", x = 0.5, y = 0.82,
             label = "Portfolio Report",
             size = 9, hjust = 0.5, colour = "#9EAAB2") +
    annotate("text", x = 0.5, y = 0.68,
             label = period_label,
             size = 14, hjust = 0.5, colour = "#2C3E50", fontface = "bold") +
    annotate("segment", x = 0.25, xend = 0.75, y = 0.60, yend = 0.60,
             colour = "grey75", linewidth = 0.6) +
    annotate("text", x = 0.5, y = 0.52,
             label = goal_line,
             size = 4.5, hjust = 0.5, colour = "grey40") +
    annotate("text", x = 0.5, y = 0.12,
             label = paste0("Generato il ", format(Sys.Date(), "%d %B %Y"),
                            "  |  danielecampus.eu"),
             size = 4, hjust = 0.5, colour = "grey55", fontface = "italic") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    theme_void() +
    theme(plot.margin = margin(20, 20, 20, 20))
}


# =============================================================================
# PAGE 1 — Executive Summary
# =============================================================================

plot_executive_summary <- function(ptf_name, cfg_ptf, ptf_summary, ptf_analysis,
                                   bt_metrics, macro_views_txt, period_label) {

  header <- paste0("Executive Summary  |  ", period_label)

  # Pie — legend labels include the %
  assets        <- ptf_analysis$Assets
  weights       <- ptf_analysis$Quotes
  asset_colours <- get_asset_colours(assets)
  leg_labels    <- paste0(assets, "  ", scales::percent(weights, accuracy = 0.1))
  leg_colours   <- setNames(asset_colours, leg_labels)
  pie_df        <- data.frame(
    fill_label = factor(leg_labels, levels = leg_labels),
    weights    = weights
  )

  p_pie <- ggplot(pie_df, aes(x = "", y = weights, fill = fill_label)) +
    geom_bar(stat = "identity", width = 1, colour = "white", linewidth = 0.3) +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = leg_colours) +
    labs(title = "Allocazione attuale", fill = NULL) +
    theme_void() +
    theme(plot.title      = element_text(hjust = 0.5, face = "bold", size = 10),
          legend.text     = element_text(size = 7),
          legend.key.size = unit(0.35, "cm"),
          plot.margin     = margin(5, 5, 5, 5))

  # Metrics — portfolio vs MSCI World benchmark
  max_dd <- .bt_metric(bt_metrics, "Current",        "Max_Drawdown")
  calmar <- .bt_metric(bt_metrics, "Current",        "Calmar")
  msci   <- if (!is.null(bt_metrics)) {
    r <- bt_metrics[bt_metrics$Strategy == "MSCI_World_BnH", ]
    if (nrow(r) > 0) r else NULL
  } else NULL

  .fp2 <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) "—" else sprintf("%.2f%%", as.numeric(x) * 100)
  .fn2 <- function(x) if (is.null(x) || length(x) == 0 || is.na(x)) "—" else sprintf("%.2f",   as.numeric(x))

  metrics <- data.frame(
    Label = c("Rendimento annualizzato", "Volatilita' (ann.)", "Sharpe Ratio",
              "VaR 95% (mensile)", "ES 95% (mensile)",
              "Max Drawdown", "Calmar Ratio"),
    Portfolio = c(
      sprintf("%.2f%%", ptf_summary$Annual_Ret  * 100),
      sprintf("%.2f%%", ptf_summary$Std_Dev_A   * 100),
      sprintf("%.2f",   ptf_summary$Sharpe_ratio),
      sprintf("%.2f%%", ptf_summary$VaR          * 100),
      sprintf("%.2f%%", ptf_summary$ES            * 100),
      if (!is.na(max_dd)) sprintf("%.2f%%", max_dd * 100) else "N/A",
      if (!is.na(calmar)) sprintf("%.2f",   calmar)       else "N/A"
    ),
    MSCI = c(
      .fp2(if (!is.null(msci)) msci$Ann_Return     else NULL),
      .fp2(if (!is.null(msci)) msci$Ann_Volatility else NULL),
      .fn2(if (!is.null(msci)) msci$Sharpe         else NULL),
      "—", "—",
      .fp2(if (!is.null(msci)) msci$Max_Drawdown   else NULL),
      .fn2(if (!is.null(msci)) msci$Calmar         else NULL)
    ),
    stringsAsFactors = FALSE
  )
  metrics$row <- seq_len(nrow(metrics))

  p_metrics <- ggplot(metrics) +
    annotate("text", x = 0.5, y = 1.10,
             label = "Metriche di Performance",
             size = 3.9, fontface = "bold", hjust = 0.5, colour = "#1A2530") +
    annotate("text", x = 0.63, y = 1.04,
             label = "Portafoglio", size = 3.0, fontface = "bold",
             hjust = 0.5, colour = "#5D7FA8") +
    annotate("text", x = 0.90, y = 1.04,
             label = "MSCI World", size = 3.0, fontface = "bold",
             hjust = 0.5, colour = "#D4904A") +
    annotate("segment", x = 0.02, xend = 0.98, y = 0.99, yend = 0.99,
             colour = "grey75", linewidth = 0.4) +
    geom_text(aes(y = 1 - row * 0.122, label = Label),
              x = 0.03, hjust = 0, size = 3.3, colour = "grey25") +
    geom_text(aes(y = 1 - row * 0.122, label = Portfolio),
              x = 0.63, hjust = 0.5, size = 3.3, fontface = "bold", colour = "#2C3E50") +
    geom_text(aes(y = 1 - row * 0.122, label = MSCI),
              x = 0.90, hjust = 0.5, size = 3.3, colour = "#D4904A") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0.05, 1.14)) +
    theme_void() +
    theme(plot.margin = margin(5, 10, 5, 5))

  # Macro signals
  macro_line <- if (length(macro_views_txt) > 0 &&
                    !all(trimws(macro_views_txt) %in% c("", "(no active macro views)"))) {
    paste0("Segnali macro attivi: ", paste(macro_views_txt, collapse = "  |  "))
  } else {
    "Segnali macro: nessun segnale attivo (tutti gli indicatori in zona neutra)."
  }

  next_rebal <- format(
    lubridate::floor_date(Sys.Date(), "quarter") + months(3), "%B %Y"
  )
  goal_line <- if (!is.null(cfg_ptf$goal)) {
    sprintf("Obiettivo: EUR %s  |  PV: EUR %s  |  Orizzonte: %d anni  |  CF: EUR %s/mese",
            fmt_eur(cfg_ptf$goal$FV), fmt_eur(cfg_ptf$goal$PV),
            cfg_ptf$goal$t, fmt_eur(cfg_ptf$goal$CF))
  } else ""

  title_grob  <- grid::textGrob(header,
                  gp = grid::gpar(fontsize = 14, fontface = "bold", col = "#1A2530"))
  goal_grob   <- grid::textGrob(goal_line,
                  gp = grid::gpar(fontsize = 9, col = "grey40"))
  macro_grob  <- grid::textGrob(
                  paste(strwrap(macro_line, width = 130), collapse = "\n"),
                  gp = grid::gpar(fontsize = 8.5, col = "#2C4A6A"))
  footer_grob <- grid::textGrob(
                  paste0("Prossimo ribilanciamento: ", next_rebal,
                         "   |   ", .footer(ptf_name),
                         "   |   Generato il ", format(Sys.Date(), "%d/%m/%Y")),
                  gp = grid::gpar(fontsize = 7.5, col = "grey55", fontface = "italic"))

  gridExtra::arrangeGrob(
    title_grob,
    goal_grob,
    gridExtra::arrangeGrob(ggplotGrob(p_pie), ggplotGrob(p_metrics),
                           ncol = 2, widths = c(0.47, 0.53)),
    macro_grob,
    footer_grob,
    nrow    = 5,
    heights = unit(c(0.09, 0.06, 0.68, 0.10, 0.07), "null")
  )
}


# =============================================================================
# PAGE 2 — Allocation & Risk Decomposition
# =============================================================================

plot_risk_decomposition <- function(ptf_name, ptf_analysis, period_label) {

  assets        <- ptf_analysis$Assets
  weights       <- ptf_analysis$Quotes
  rc_total      <- ptf_analysis$Total_RC / sum(ptf_analysis$Total_RC)
  asset_colours <- get_asset_colours(assets)

  # Pie — legend labels with %
  leg_labels  <- paste0(assets, "  ", scales::percent(weights, accuracy = 0.1))
  leg_colours <- setNames(asset_colours, leg_labels)
  pie_df      <- data.frame(
    fill_label = factor(leg_labels, levels = leg_labels),
    weights    = weights
  )

  p_pie <- ggplot(pie_df, aes(x = "", y = weights, fill = fill_label)) +
    geom_bar(stat = "identity", width = 1, colour = "white", linewidth = 0.4) +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = leg_colours) +
    labs(title = "Allocazione attuale", fill = NULL) +
    theme_void() +
    theme(plot.title      = element_text(hjust = 0.5, face = "bold", size = 11),
          legend.text     = element_text(size = 7),
          legend.key.size = unit(0.35, "cm"),
          plot.margin     = margin(5, 5, 5, 10))

  bar_df <- data.frame(
    Asset   = rep(assets, 2),
    Measure = rep(c("Peso nel portafoglio", "Contributo al rischio"), each = length(assets)),
    Value   = c(weights, rc_total),
    stringsAsFactors = FALSE
  )
  bar_df$Asset   <- factor(bar_df$Asset,   levels = rev(assets))
  bar_df$Measure <- factor(bar_df$Measure,
                           levels = c("Peso nel portafoglio", "Contributo al rischio"))

  p_bar <- ggplot(bar_df, aes(x = Asset, y = Value, fill = Measure)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.7), width = 0.62) +
    geom_text(aes(label = scales::percent(Value, accuracy = 0.1)),
              position = position_dodge(width = 0.7), hjust = -0.1, size = 2.9) +
    scale_fill_manual(values = c("Peso nel portafoglio"   = "#5D7FA8",
                                 "Contributo al rischio"  = "#E08080")) +
    scale_y_continuous(labels = scales::percent,
                       expand = expansion(mult = c(0, 0.22))) +
    coord_flip() +
    labs(title    = "Peso vs Contributo al Rischio",
         subtitle = paste0(
           "Contributo al rischio > peso = concentratore di rischio. ",
           "Calcolato come w_i * (Sigma*w)_i / sigma_ptf (marginal RC). ",
           "Colori: blu = peso, rosso = rischio."
         ),
         x = NULL, y = "Percentuale", fill = NULL,
         caption = .footer(ptf_name)) +
    theme_minimal(base_size = 10) +
    theme(plot.title      = element_text(face = "bold", size = 11),
          plot.subtitle   = element_text(colour = "grey40", size = 7.5),
          plot.caption    = element_text(colour = "grey55", size = 7, hjust = 0.5),
          legend.position = "bottom",
          plot.margin     = margin(5, 10, 5, 5))

  gridExtra::arrangeGrob(
    ggplotGrob(p_pie),
    ggplotGrob(p_bar),
    ncol   = 2,
    widths = c(0.42, 0.58),
    top    = grid::textGrob(
      paste0("ALLOCAZIONE & RISK DECOMPOSITION  |  ", period_label),
      gp = grid::gpar(fontsize = 13, fontface = "bold", col = "#1A2530")
    )
  )
}


# =============================================================================
# PAGE 3 — Correlation Matrix
# =============================================================================

plot_correlation_heatmap <- function(corr_df, ptf_name, period_label) {

  if (is.null(corr_df) || nrow(corr_df) == 0) {
    return(.placeholder(
      "Correlation matrix not available.\nRun tar_make() to regenerate pipeline outputs."
    ))
  }

  assets <- unique(corr_df$Asset1)
  corr_df$Asset1       <- factor(corr_df$Asset1, levels = assets)
  corr_df$Asset2       <- factor(corr_df$Asset2, levels = rev(assets))
  corr_df$Correlation  <- as.numeric(corr_df$Correlation)

  ggplot(corr_df, aes(x = Asset1, y = Asset2, fill = Correlation)) +
    geom_tile(colour = "white", linewidth = 0.6) +
    geom_text(aes(label = sprintf("%.2f", Correlation)), size = 3.1, colour = "grey20") +
    scale_fill_gradient2(low = "#3B6EA5", mid = "white", high = "#C0392B",
                         midpoint = 0, limits = c(-1, 1), name = "Correlazione") +
    labs(
      title    = paste0("Matrice di Correlazione  |  ", period_label),
      subtitle = paste0(
        "Correlazioni su rendimenti mensili, intera serie storica disponibile. ",
        "Blu = correlazione negativa (diversificazione); Rosso = correlazione positiva (concentrazione)."
      ),
      x = NULL, y = NULL,
      caption = .footer(ptf_name)
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title      = element_text(face = "bold", size = 13),
          plot.subtitle   = element_text(colour = "grey40", size = 8.5),
          plot.caption    = element_text(colour = "grey55", size = 7, hjust = 0.5),
          axis.text.x     = element_text(angle = 30, hjust = 1, size = 9),
          axis.text.y     = element_text(size = 9),
          legend.position = "right",
          panel.grid      = element_blank())
}


# =============================================================================
# PAGE 4 — Macro Context & Black-Litterman Views
# =============================================================================

plot_macro_page <- function(ptf_name, indicators_df, cfg_rules, period_label) {

  get_ind  <- function(id) {
    r <- indicators_df[indicators_df$Indicator == id, ]
    if (nrow(r) == 0L) list(value = NA_real_, date = NA_character_)
    else list(value = r$Value[1L], date = r$Date[1L])
  }
  fmt_date <- function(d) {
    if (is.null(d) || is.na(d) || identical(d, "NA")) return("")
    tryCatch(format(as.Date(d), "%b %d"), error = function(e) as.character(d))
  }

  yc_i  <- get_ind("Yield_Curve_10Y2Y"); cpi_i <- get_ind("CPI_YoY")
  ff_i  <- get_ind("Fed_Funds");         hy_i  <- get_ind("HY_Spread")
  be_i  <- get_ind("Breakeven_5Y");      vix_i <- get_ind("VIX")
  yc_v  <- yc_i$value; cpi_v <- cpi_i$value; ff_v <- ff_i$value
  hy_v  <- hy_i$value; be_v  <- be_i$value;  vix_v <- vix_i$value

  yc_regime <- if (is.na(yc_v)) {
    list(badge = "N/A", col = "grey85", active = FALSE, view = "Dati non disponibili")
  } else if (yc_v < cfg_rules$yield_curve$inversion_strong) {
    list(badge = "Forte inversione", col = "#E08080", active = TRUE,
         view = sprintf("ATTIVA — Bond outperform equity di +%.0f%% (conf %.0f%%)",
                        cfg_rules$yield_curve$Q_recession * 100,
                        cfg_rules$yield_curve$conf_recession * 100))
  } else if (yc_v < cfg_rules$yield_curve$flat) {
    list(badge = "Piatta/invertita", col = "#F4C09A", active = FALSE,
         view = "Non attiva — inversione lieve, segnale ambiguo")
  } else if (yc_v > cfg_rules$yield_curve$steep) {
    list(badge = "Ripida (espansione)", col = "#A8CC98", active = TRUE,
         view = sprintf("ATTIVA — Equity outperform bond di +%.0f%% (conf %.0f%%)",
                        cfg_rules$yield_curve$Q_expansion * 100,
                        cfg_rules$yield_curve$conf_expansion * 100))
  } else {
    list(badge = "Normale (neutrale)", col = "#D4E8D4", active = FALSE,
         view = "Non attiva — curva nella zona neutra")
  }

  cpi_regime <- if (is.na(cpi_v)) {
    list(badge = "N/A", col = "grey85", active = FALSE, view = "Dati non disponibili")
  } else if (cpi_v > cfg_rules$inflation$high) {
    list(badge = "Alta inflazione", col = "#E08080", active = TRUE,
         view = sprintf("ATTIVA — Gold expected +%.0f%% (conf %.0f%%)",
                        cfg_rules$inflation$Q_high * 100,
                        cfg_rules$inflation$conf_high * 100))
  } else if (cpi_v > cfg_rules$inflation$moderate) {
    list(badge = "Inflaz. moderata", col = "#F4C09A", active = TRUE,
         view = sprintf("ATTIVA — Gold expected +%.0f%% (conf %.0f%%)",
                        cfg_rules$inflation$Q_moderate * 100,
                        cfg_rules$inflation$conf_moderate * 100))
  } else {
    list(badge = "Inflaz. bassa", col = "#A8CC98", active = FALSE,
         view = "Non attiva — inflazione sotto soglia moderata")
  }

  ff_regime <- if (is.na(ff_v)) {
    list(badge = "N/A", col = "grey85", active = FALSE,
         view = "Non attiva — dati Fed Funds non disponibili")
  } else {
    list(badge = "Disponibile", col = "#D4E8D4", active = TRUE,
         view = sprintf("ATTIVA — Cash expected %.2f%% (conf %.0f%%)",
                        ff_v, cfg_rules$cash$confidence * 100))
  }

  vc <- function(active) if (active) "#2C3E50" else "grey60"

  ind_rows <- data.frame(
    y     = c(0.77, 0.69, 0.61, 0.53, 0.45, 0.37),
    label = c("Curva Tassi (10Y-2Y)", "CPI (Inflaz. YoY)", "Fed Funds Rate",
              "HY Spread (OAS)", "Breakeven 5Y", "VIX"),
    value = c(
      if (is.na(yc_v))  "N/A" else sprintf("%+.2f%%", yc_v),
      if (is.na(cpi_v)) "N/A" else sprintf("%.2f%%",  cpi_v),
      if (is.na(ff_v))  "N/A" else sprintf("%.2f%%",  ff_v),
      if (is.na(hy_v))  "N/A" else sprintf("%.2f%%",  hy_v),
      if (is.na(be_v))  "N/A" else sprintf("%.2f%%",  be_v),
      if (is.na(vix_v)) "N/A" else sprintf("%.1f",    vix_v)
    ),
    date  = c(fmt_date(yc_i$date), fmt_date(cpi_i$date), fmt_date(ff_i$date),
              fmt_date(hy_i$date), fmt_date(be_i$date),  fmt_date(vix_i$date)),
    badge = c(yc_regime$badge, cpi_regime$badge, ff_regime$badge, "N/A", "N/A", "N/A"),
    bcol  = c(yc_regime$col,   cpi_regime$col,   ff_regime$col,
              "grey91", "grey91", "grey91"),
    stringsAsFactors = FALSE
  )

  report_date <- format(Sys.Date(), "%d %B %Y")

  gg <- ggplot() +
    annotate("text", x = 0.5, y = 1.10,
             label = paste0("MACRO INDICATORI & VISTE  |  ", period_label),
             size = 5.5, fontface = "bold", hjust = 0.5, colour = "#1A2530") +
    annotate("text", x = 0.5, y = 1.04,
             label = paste0("Dati al: ", report_date,
                            "  |  Fonte: FRED (Federal Reserve Bank of St. Louis)"),
             size = 3.0, colour = "grey45", hjust = 0.5) +
    annotate("segment", x = 0.50, xend = 0.50, y = 0.04, yend = 0.92,
             colour = "grey72", linewidth = 0.5) +
    annotate("text", x = 0.01, y = 0.90,
             label = "INDICATORI MACROECONOMICI (FRED)",
             size = 3.6, fontface = "bold", colour = "#2C3E50", hjust = 0) +
    annotate("segment", x = 0.01, xend = 0.49, y = 0.87, yend = 0.87,
             colour = "grey70", linewidth = 0.4) +
    annotate("text", x = 0.52, y = 0.90,
             label = "VISTE ATTIVE & LIVELLI DI CONFIDENZA",
             size = 3.6, fontface = "bold", colour = "#2C3E50", hjust = 0) +
    annotate("segment", x = 0.52, xend = 0.99, y = 0.87, yend = 0.87,
             colour = "grey70", linewidth = 0.4)

  for (i in seq_len(nrow(ind_rows))) {
    yy <- ind_rows$y[i]
    gg <- gg +
      annotate("rect", xmin = 0.01, xmax = 0.190, ymin = yy - 0.025, ymax = yy + 0.025,
               fill = ind_rows$bcol[i], alpha = 0.75) +
      annotate("text", x = 0.100, y = yy, label = ind_rows$badge[i],
               size = 2.4, hjust = 0.5, colour = "grey20") +
      annotate("text", x = 0.200, y = yy, label = ind_rows$label[i],
               size = 3.1, hjust = 0, colour = "grey25") +
      annotate("text", x = 0.370, y = yy, label = ind_rows$value[i],
               size = 3.5, hjust = 0.5, fontface = "bold", colour = "#2C3E50") +
      annotate("text", x = 0.475, y = yy, label = ind_rows$date[i],
               size = 2.6, hjust = 1, colour = "grey55")
  }

  gg <- gg +
    annotate("text", x = 0.01, y = 0.27, label = "Soglie di configurazione",
             size = 3.2, fontface = "bold", colour = "#2C3E50", hjust = 0) +
    annotate("text", x = 0.01, y = 0.22,
             label = sprintf(
               "Curva tassi: < %.2f%% forte inversione  |  < %.2f%% piatta  |  > %.2f%% espansione",
               cfg_rules$yield_curve$inversion_strong,
               cfg_rules$yield_curve$flat,
               cfg_rules$yield_curve$steep),
             size = 2.7, hjust = 0, colour = "grey38") +
    annotate("text", x = 0.01, y = 0.18,
             label = sprintf("CPI YoY: > %.1f%% alta inflazione  |  > %.1f%% moderata",
                             cfg_rules$inflation$high, cfg_rules$inflation$moderate),
             size = 2.7, hjust = 0, colour = "grey38") +
    annotate("text", x = 0.01, y = 0.14,
             label = "Cash: Fed Funds Rate (FRED), vista attiva quando il dato e' disponibile",
             size = 2.7, hjust = 0, colour = "grey38") +
    annotate("segment", x = 0.01, xend = 0.49, y = 0.10, yend = 0.10,
             colour = "grey82", linewidth = 0.3) +
    annotate("text", x = 0.01, y = 0.07,
             label = sprintf("Black-Litterman: tau = %.3f  |  lambda = %.1f (He-Litterman 1999)",
                             cfg_rules$tau, cfg_rules$lambda),
             size = 2.7, hjust = 0, colour = "grey45") +
    annotate("text", x = 0.01, y = 0.03,
             label = paste0("tau alto -> piu' peso alle viste vs equilibrio. ",
                            "lambda = avversione al rischio standard accademica."),
             size = 2.6, hjust = 0, colour = "grey55", fontface = "italic")

  gg <- gg +
    annotate("text", x = 0.52, y = 0.83,
             label = sprintf("1.  EQUITY vs BOND  —  Curva Tassi 10Y-2Y  (%s)",
                             if (is.na(yc_v)) "N/A" else sprintf("%+.2f%%", yc_v)),
             size = 3.3, fontface = "bold", hjust = 0, colour = vc(yc_regime$active)) +
    annotate("text", x = 0.54, y = 0.78, label = yc_regime$view,
             size = 3.0, hjust = 0, colour = vc(yc_regime$active)) +
    annotate("text", x = 0.54, y = 0.73,
             label = sprintf("Conf. recessione %.0f%%  —  segnale storico forte (~90%% cicli passati),",
                             cfg_rules$yield_curve$conf_recession * 100),
             size = 2.7, hjust = 0, colour = "grey42") +
    annotate("text", x = 0.54, y = 0.69,
             label = "lead time variabile (6-24 mesi); rischio falso positivo (es. 2023).",
             size = 2.7, hjust = 0, colour = "grey42") +
    annotate("text", x = 0.54, y = 0.65,
             label = sprintf("Conf. espansione %.0f%%  —  curva ripida puo' riflettere inflaz. da offerta.",
                             cfg_rules$yield_curve$conf_expansion * 100),
             size = 2.7, hjust = 0, colour = "grey42") +
    annotate("segment", x = 0.52, xend = 0.99, y = 0.61, yend = 0.61,
             colour = "grey87", linewidth = 0.3) +
    annotate("text", x = 0.52, y = 0.58,
             label = sprintf("2.  ORO  —  CPI YoY  (%s)",
                             if (is.na(cpi_v)) "N/A" else sprintf("%.2f%%", cpi_v)),
             size = 3.3, fontface = "bold", hjust = 0, colour = vc(cpi_regime$active)) +
    annotate("text", x = 0.54, y = 0.53, label = cpi_regime$view,
             size = 3.0, hjust = 0, colour = vc(cpi_regime$active)) +
    annotate("text", x = 0.54, y = 0.48,
             label = sprintf("Conf. alta (>%.0f%%)  %.0f%%  —  inflazione elevata supporta l'oro storicamente,",
                             cfg_rules$inflation$high, cfg_rules$inflation$conf_high * 100),
             size = 2.7, hjust = 0, colour = "grey42") +
    annotate("text", x = 0.54, y = 0.44,
             label = "rischio stagflazione e incertezza geopolitica moderano la vista.",
             size = 2.7, hjust = 0, colour = "grey42") +
    annotate("text", x = 0.54, y = 0.40,
             label = sprintf("Conf. moderata (>%.0f%%)  %.0f%%  —  normalizzazione verso target BCE/Fed possibile.",
                             cfg_rules$inflation$moderate, cfg_rules$inflation$conf_moderate * 100),
             size = 2.7, hjust = 0, colour = "grey42") +
    annotate("segment", x = 0.52, xend = 0.99, y = 0.36, yend = 0.36,
             colour = "grey87", linewidth = 0.3) +
    annotate("text", x = 0.52, y = 0.33,
             label = sprintf("3.  CASH  —  Fed Funds Rate  (%s)",
                             if (is.na(ff_v)) "N/A" else sprintf("%.2f%%", ff_v)),
             size = 3.3, fontface = "bold", hjust = 0, colour = vc(ff_regime$active)) +
    annotate("text", x = 0.54, y = 0.28, label = ff_regime$view,
             size = 3.0, hjust = 0, colour = vc(ff_regime$active)) +
    annotate("text", x = 0.54, y = 0.23,
             label = sprintf("Conf. %.0f%%  —  tasso pubblicato dalla Fed, disponibile quasi in real-time.",
                             cfg_rules$cash$confidence * 100),
             size = 2.7, hjust = 0, colour = "grey42") +
    annotate("text", x = 0.54, y = 0.19,
             label = "Rappresenta il rendimento atteso dei money market / overnight nel portafoglio.",
             size = 2.7, hjust = 0, colour = "grey42") +
    annotate("text", x = 0.54, y = 0.15,
             label = "Alta confidenza strutturale: il tasso e' uno dei pochi dati macro senza ritardo.",
             size = 2.7, hjust = 0, colour = "grey42")

  gg +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1.14)) +
    theme_void() +
    theme(plot.margin = margin(12, 12, 12, 12))
}


# =============================================================================
# PAGE 5 — Allocation Comparison + Scenario Metrics Table
# =============================================================================

.build_comparison_bar <- function(current_w, univ_w, macro_w, min_report) {
  cur_df <- data.frame(Asset = names(current_w), Weight = as.numeric(current_w),
                       Scenario = "Current", stringsAsFactors = FALSE)
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

  vis <- unique(combined$Asset[combined$Weight >= min_report])
  combined <- combined[combined$Asset %in% vis, ]
  combined$Scenario <- factor(combined$Scenario, levels = c("Current", "Max-Sharpe", "Macro-tilted"))

  cur_vis  <- intersect(names(current_w), vis)
  new_vis  <- setdiff(vis, names(current_w))
  cur_ord  <- names(sort(current_w[cur_vis], decreasing = TRUE))
  new_ord  <- if (length(new_vis) > 0) {
    nm <- sapply(new_vis, function(a) max(combined$Weight[combined$Asset == a]))
    new_vis[order(nm, decreasing = TRUE)]
  } else character(0)
  combined$Asset <- factor(combined$Asset, levels = rev(c(cur_ord, new_ord)))

  ggplot(combined, aes(x = Asset, y = Weight, fill = Scenario)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.75), width = 0.65) +
    geom_text(aes(label = scales::percent(Weight, accuracy = 0.1)),
              position = position_dodge(width = 0.75), hjust = -0.1, size = 2.8) +
    scale_fill_manual(values = REPORT_COLOURS) +
    scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.18))) +
    coord_flip() +
    labs(title    = "Confronto Allocazioni: Attuale vs Suggerita",
         subtitle = paste0(
           "Max-Sharpe: Markowitz bounded sull'universo completo. ",
           "Macro-tilted: BL con viste da FRED. ",
           "Pesi rispettano i limiti di concentrazione e numero massimo asset."
         ),
         x = NULL, y = "Peso", fill = "Scenario") +
    theme_minimal(base_size = 10) +
    theme(plot.title      = element_text(face = "bold", size = 11),
          plot.subtitle   = element_text(colour = "grey40", size = 7.5),
          legend.position = "bottom",
          plot.margin     = margin(5, 10, 5, 5))
}

plot_allocation_comparison_full <- function(ptf_name, current_w, univ_w, macro_w,
                                             ptf_summary, univ_summary, macro_summary,
                                             min_report = 0.02, period_label) {

  p_bar <- .build_comparison_bar(current_w, univ_w, macro_w, min_report)
  if (is.null(p_bar)) {
    return(ggplotGrob(.placeholder("Universe optimization data unavailable.")))
  }

  .fp <- function(x) if (is.null(x) || length(x) == 0 || is.na(x[1])) "N/A" else sprintf("%.2f%%", as.numeric(x[1]) * 100)
  .fn <- function(x) if (is.null(x) || length(x) == 0 || is.na(x[1])) "N/A" else sprintf("%.2f",   as.numeric(x[1]))

  ms_row  <- if (!is.null(univ_summary) && "Method" %in% names(univ_summary))
    univ_summary[univ_summary$Method == "Max_Sharpe",  ] else NULL
  mv_row  <- if (!is.null(univ_summary) && "Method" %in% names(univ_summary))
    univ_summary[univ_summary$Method == "Min_Variance", ] else NULL

  tbl <- data.frame(
    Scenario        = c("Attuale (storico)", "Max-Sharpe (universo)", "Macro-tilted (BL)"),
    `Rend. atteso`  = c(.fp(ptf_summary$Annual_Ret),
                        .fp(if (!is.null(ms_row) && nrow(ms_row) > 0) ms_row$Expected_Return else NULL),
                        .fp(if (!is.null(macro_summary)) macro_summary$Expected_Return else NULL)),
    `Volatilita'`   = c(.fp(ptf_summary$Std_Dev_A),
                        .fp(if (!is.null(ms_row) && nrow(ms_row) > 0) ms_row$Volatility else NULL),
                        .fp(if (!is.null(macro_summary)) macro_summary$Volatility else NULL)),
    Sharpe          = c(.fn(ptf_summary$Sharpe_ratio),
                        .fn(if (!is.null(ms_row) && nrow(ms_row) > 0) ms_row$Sharpe else NULL),
                        .fn(if (!is.null(macro_summary)) macro_summary$Sharpe else NULL)),
    check.names = FALSE, stringsAsFactors = FALSE
  )

  tt <- gridExtra::ttheme_minimal(base_size = 9)
  n_rows <- nrow(tbl); n_cols <- ncol(tbl)
  tt$core$bg_params$fill    <- rep(c("white", "#F0F4F8"), length.out = n_rows * n_cols)
  tt$core$fg_params$cex     <- 0.82
  tt$colhead$fg_params      <- list(cex = 0.82, fontface = "bold")
  tt$colhead$bg_params$fill <- rep("#E0E8F0", n_cols)
  t_grob <- gridExtra::tableGrob(tbl, rows = NULL, theme = tt)
  note_grob  <- grid::textGrob(
    paste0("Max-Sharpe ottimizza su rendimenti storici (bounded Markowitz). ",
           "Macro-tilted incorpora le viste BL correnti.  |  ", .footer(ptf_name)),
    gp = grid::gpar(fontsize = 7.5, col = "grey50", fontface = "italic")
  )
  title_grob <- grid::textGrob(
    paste0("CONFRONTO ALLOCAZIONI  |  ", period_label),
    gp = grid::gpar(fontsize = 13, fontface = "bold", col = "#1A2530")
  )

  gridExtra::arrangeGrob(
    title_grob,
    ggplotGrob(p_bar),
    t_grob,
    note_grob,
    nrow    = 4,
    heights = unit(c(0.07, 0.60, 0.24, 0.09), "null")
  )
}

# Keep old name for any direct callers
plot_allocation_comparison <- function(current_w, univ_w, macro_w, min_report = 0.02) {
  .build_comparison_bar(current_w, univ_w, macro_w, min_report)
}


# =============================================================================
# PAGE 6 — Backtest & Drawdown (two-panel)
# =============================================================================

plot_backtest_with_drawdown <- function(bt_paths, ptf_name, period_label,
                                         start_date = NULL, end_date = NULL,
                                         n_months = 36L) {
  if (is.null(bt_paths)) {
    return(ggplotGrob(.placeholder("Backtest data unavailable.")))
  }

  if (!is.null(start_date) && !is.null(end_date)) {
    bt_w <- bt_paths[bt_paths$Date >= start_date & bt_paths$Date <= end_date, ]
  } else {
    cutoff <- Sys.Date() - as.integer(round(n_months * 30.5))
    bt_w   <- bt_paths[bt_paths$Date >= cutoff, ]
  }
  if (nrow(bt_w) == 0) bt_w <- bt_paths

  bt_w <- bt_w %>%
    dplyr::group_by(Strategy) %>%
    dplyr::mutate(Cum_Value = Cum_Value / dplyr::first(Cum_Value) * 100) %>%
    dplyr::ungroup()

  p_upper <- ggplot(bt_w, aes(x = Date, y = Cum_Value, colour = Strategy)) +
    geom_line(data = bt_w[bt_w$Strategy != "Current", ], linewidth = 0.7, alpha = 0.8) +
    geom_line(data = bt_w[bt_w$Strategy == "Current", ], linewidth = 1.8) +
    scale_colour_manual(values = STRATEGY_COLOURS, breaks = names(STRATEGY_COLOURS)) +
    labs(title    = paste0("Backtest Walk-Forward  |  ", period_label),
         subtitle = paste0(
           "Ogni strategia e' ribilanciata usando solo i dati disponibili a quel momento ",
           "(no look-ahead bias). Portafoglio Corrente = linea spessa."
         ),
         x = NULL, y = "Valore (base 100 inizio finestra)", colour = "Strategia") +
    theme_minimal(base_size = 10) +
    theme(plot.title      = element_text(face = "bold", size = 12),
          plot.subtitle   = element_text(colour = "grey40", size = 7.5),
          legend.position = "bottom",
          legend.text     = element_text(size = 8),
          axis.text.x     = element_blank(),
          axis.ticks.x    = element_blank(),
          plot.margin     = margin(5, 5, 0, 5))

  cur_path <- bt_w[bt_w$Strategy == "Current", ]
  cur_path <- cur_path[order(cur_path$Date), ]

  p_lower <- if (nrow(cur_path) > 0) {
    cur_path$Peak     <- cummax(cur_path$Cum_Value)
    cur_path$Drawdown <- cur_path$Cum_Value / cur_path$Peak - 1
    ggplot(cur_path, aes(x = Date)) +
      geom_ribbon(aes(ymin = Drawdown), ymax = 0, fill = "#E08080", alpha = 0.55) +
      geom_line(aes(y = Drawdown), colour = "#C0392B", linewidth = 0.6) +
      geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.4) +
      scale_y_continuous(labels = scales::percent,
                         expand = expansion(mult = c(0.05, 0.02))) +
      labs(x = NULL, y = "Drawdown (%)",
           caption = paste0(
             "Drawdown calcolato su rendimenti mensili walk-forward. ",
             "Il drawdown giornaliero reale puo' essere superiore del 20-40%.  |  ",
             .footer(ptf_name)
           )) +
      theme_minimal(base_size = 10) +
      theme(plot.caption = element_text(colour = "grey55", size = 7, hjust = 0.5),
            plot.margin  = margin(0, 5, 5, 5))
  } else {
    .placeholder("Drawdown data unavailable.")
  }

  gridExtra::arrangeGrob(
    ggplotGrob(p_upper),
    ggplotGrob(p_lower),
    nrow    = 2,
    heights = unit(c(0.65, 0.35), "null")
  )
}

# Keep old name for any direct callers
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
    dplyr::group_by(Strategy) %>%
    dplyr::mutate(Cum_Value = Cum_Value / dplyr::first(Cum_Value) * 100) %>%
    dplyr::ungroup()
  ggplot(bt_recent, aes(x = Date, y = Cum_Value, colour = Strategy)) +
    geom_line(linewidth = 0.8) +
    scale_colour_manual(values = STRATEGY_COLOURS, breaks = names(STRATEGY_COLOURS)) +
    labs(title = title, x = NULL, y = "Valore (base 100)", colour = "Strategia") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 13), legend.position = "bottom")
}


# =============================================================================
# PAGE 7 — Monte Carlo Projection (enhanced)
# =============================================================================

plot_mc_from_parquet <- function(mc_ts, mc_summary = NULL, ptf_name) {
  if (is.null(mc_ts)) return(.placeholder("Monte Carlo data unavailable."))

  hist_df     <- mc_ts[is.na(mc_ts$P5), ]
  forecast_df <- mc_ts[!is.na(mc_ts$P5), ]

  n_sim_str <- if (!is.null(mc_summary) && !is.null(mc_summary$Horizon_period))
    paste0("N simulazioni: 10.000  |  Orizzonte: ", mc_summary$Horizon_period[1],
           " mesi  |  Metodo: bootstrap storico (righe intere, preserva correlazioni)")
  else "Metodo: bootstrap storico (ricampionamento righe intere)"

  exp_ret_str <- if (!is.null(mc_summary) && !is.na(mc_summary$Expected_Return[1]))
    sprintf("  |  E[R] ann.: %.2f%%  |  VaR %dm: %.2f%%",
            mc_summary$Expected_Return[1] * 100,
            as.integer(mc_summary$Horizon_period[1]),
            mc_summary$VaR[1] * 100)
  else ""

  ggplot() +
    geom_line(data = hist_df, aes(x = Dates, y = Mean),
              colour = "#5D7FA8", linewidth = 0.8) +
    geom_ribbon(data = forecast_df, aes(x = Dates, ymin = P5, ymax = P95),
                fill = "#6ABBB0", alpha = 0.25) +
    geom_line(data = forecast_df, aes(x = Dates, y = Mean),
              colour = "#6ABBB0", linewidth = 0.9, linetype = "dashed") +
    labs(
      title    = "Proiezione Monte Carlo",
      subtitle = paste0(n_sim_str, exp_ret_str,
                        "\nBanda: 5deg-95deg percentile. ",
                        "Linea storica: valore cumulato portafoglio (base 100)."),
      x = NULL, y = "Valore portafoglio (base 100)",
      caption = paste0(
        "Scenario ottimistico basato su bootstrap storico — risente del bias del campione recente.  |  ",
        .footer(ptf_name)
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title    = element_text(face = "bold", size = 13),
          plot.subtitle = element_text(colour = "grey40", size = 8.5),
          plot.caption  = element_text(colour = "grey55", size = 7, hjust = 0.5))
}


# =============================================================================
# PAGE 8 — Goal Projection (calibrated: Bear / Base / Bull)
# =============================================================================

plot_goal_calibrated <- function(ptf_name, goal_cfg, ptf_summary, mc_summary,
                                  macro_summary, var_cov_long, ptf_analysis,
                                  period_label, risk_free = 0.03) {

  pv      <- goal_cfg$PV
  fv      <- goal_cfg$FV
  t_years <- goal_cfg$t
  cf_base <- goal_cfg$CF
  fmt_e   <- fmt_eur  # use global helper (k/M notation, no scientific)

  # Bear: min-variance frontier return
  r_bear <- tryCatch({
    if (!is.null(var_cov_long) && nrow(var_cov_long) > 0 && !is.null(ptf_analysis)) {
      Sigma_ann <- .long_to_matrix(var_cov_long, "Cov_Ann")
      mu_ann    <- setNames((1 + ptf_analysis$Ret_Avg)^12 - 1, ptf_analysis$Assets)
      common    <- intersect(rownames(Sigma_ann), names(mu_ann))
      w_mvp     <- .minvar_weights(Sigma_ann[common, common])
      sum(w_mvp * mu_ann[common])
    } else ptf_summary$Annual_Ret * 0.6
  }, error = function(e) ptf_summary$Annual_Ret * 0.6)

  # Base: BL posterior expected return
  r_base <- if (!is.null(macro_summary) && !is.na(macro_summary$Expected_Return[1]))
    as.numeric(macro_summary$Expected_Return[1])
  else as.numeric(ptf_summary$Annual_Ret)

  # Bull: MC bootstrap expected return
  r_bull <- if (!is.null(mc_summary) && !is.na(mc_summary$Expected_Return[1]))
    as.numeric(mc_summary$Expected_Return[1])
  else as.numeric(ptf_summary$Annual_Ret) * 1.3

  # Project wealth paths
  project_path <- function(r_ann) {
    r_m  <- (1 + r_ann)^(1 / 12) - 1
    n    <- as.integer(round(t_years * 12))
    v    <- numeric(n + 1L); v[1] <- pv
    for (m in seq_len(n)) v[m + 1L] <- v[m] * (1 + r_m) + cf_base
    v
  }

  n_tot  <- as.integer(round(t_years * 12))
  years  <- (0:n_tot) / 12
  v_bear <- project_path(r_bear)
  v_base <- project_path(r_base)
  v_bull <- project_path(r_bull)

  paths_df <- data.frame(
    Year     = rep(years, 3),
    Value    = c(v_bear, v_base, v_bull),
    Scenario = factor(
      rep(c("Bear (min-variance)", "Base (BL posterior)", "Bull (MC bootstrap)"),
          each = length(years)),
      levels = c("Bear (min-variance)", "Base (BL posterior)", "Bull (MC bootstrap)")
    )
  )

  req_cf <- tryCatch({
    r_m <- (1 + r_base)^(1 / 12) - 1; n <- n_tot
    (fv - pv * (1 + r_m)^n) / (((1 + r_m)^n - 1) / r_m)
  }, error = function(e) NA_real_)

  tv_bear <- tail(v_bear, 1); tv_base <- tail(v_base, 1); tv_bull <- tail(v_bull, 1)

  subtitle <- paste0(
    sprintf("Bear: min-variance frontier (%.1f%%)  |  ", r_bear * 100),
    sprintf("Base: BL posterior (%.1f%%)  |  ", r_base * 100),
    sprintf("Bull: MC bootstrap (%.1f%%)\n", r_bull * 100),
    sprintf("Valori finali  —  Bear: EUR %s  |  Base: EUR %s  |  Bull: EUR %s\n",
            fmt_e(tv_bear), fmt_e(tv_base), fmt_e(tv_bull)),
    sprintf("CF attuale: EUR %d/mese  |  CF necessario (tasso base): %s",
            cf_base,
            if (is.na(req_cf)) "N/A" else sprintf("EUR %.0f/mese", req_cf))
  )

  scen_cols <- c("Bear (min-variance)"  = "#E08080",
                 "Base (BL posterior)"  = "#5D7FA8",
                 "Bull (MC bootstrap)"  = "#78B87A")

  ggplot(paths_df, aes(x = Year, y = Value / 1e3, colour = Scenario)) +
    geom_hline(yintercept = fv / 1e3, linetype = "dashed",
               colour = "grey50", linewidth = 0.7) +
    annotate("text", x = 0, y = fv / 1e3 * 1.04,
             label = paste0("Obiettivo: EUR ", fmt_e(fv)),
             hjust = 0, size = 3.2, colour = "grey50") +
    geom_line(linewidth = 1.3) +
    scale_colour_manual(values = scen_cols) +
    scale_y_continuous(
      labels = function(x) {
        v <- x * 1e3
        ifelse(abs(v) >= 1e6, sprintf("EUR %.2fM", v / 1e6),
               ifelse(abs(v) >= 1e3, sprintf("EUR %.0fk", v / 1e3),
                      sprintf("EUR %.0f", v)))
      }
    ) +
    scale_x_continuous(
      breaks = seq(0, t_years, by = max(1L, as.integer(t_years / 7L)))
    ) +
    labs(
      title    = paste0("Proiezione verso Obiettivo  |  ", period_label),
      subtitle = subtitle,
      x = "Anni", y = "Valore (EUR migliaia)", colour = "Scenario",
      caption  = paste0(
        "Bear: min-variance storica (frontiera efficiente). ",
        "Base: BL posterior con macro corrente — best estimate dato il regime macro. ",
        "Bull: MC bootstrap storico — risente del bias del campione recente.  |  ",
        .footer(ptf_name)
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title      = element_text(face = "bold", size = 13),
          plot.subtitle   = element_text(colour = "grey40", size = 8.5),
          plot.caption    = element_text(colour = "grey55", size = 7, hjust = 0.5),
          legend.position = "bottom")
}


# =============================================================================
# PAGE 9 — Methodology & Disclaimer
# =============================================================================

plot_methodology_page <- function(cfg, period_label) {

  lb  <- cfg$backtest$lookback       %||% 36
  rf  <- cfg$backtest$rebalance_freq %||% 12
  ns  <- cfg$simulation$n_sim        %||% 10000

  ggplot() +
    annotate("text", x = 0.5, y = 1.08,
             label = paste0("METODOLOGIA & DISCLAIMER | ", period_label),
             size = 5.5, fontface = "bold", hjust = 0.5, colour = "#1A2530") +
    annotate("segment", x = 0.02, xend = 0.98, y = 1.03, yend = 1.03,
             colour = "grey70", linewidth = 0.5) +
    annotate("text", x = 0.03, y = 0.99, label = "METODOLOGIA",
             size = 4, fontface = "bold", hjust = 0, colour = "#2C3E50") +
    annotate("text", x = 0.05, y = 0.94,
             label = "Rendimenti e volatilita': mensili semplici (P_t / P_{t-1} - 1), poi annualizzati (x12 per var, x sqrt(12) per vol).",
             size = 2.9, hjust = 0, colour = "grey25") +
    annotate("text", x = 0.05, y = 0.90,
             label = "Matrice di covarianza: covarianza campionaria su serie storica completa, annualizzata.",
             size = 2.9, hjust = 0, colour = "grey25") +
    annotate("text", x = 0.05, y = 0.86,
             label = "VaR e ES: metodo storico al 95% su ritorni mensili portafoglio (PerformanceAnalytics).",
             size = 2.9, hjust = 0, colour = "grey25") +
    annotate("text", x = 0.05, y = 0.82,
             label = paste0("Simulazione Monte Carlo: bootstrap storico, righe intere (preserva correlazioni e fat tails). N sim: ",
                            format(ns, big.mark = ".")),
             size = 2.9, hjust = 0, colour = "grey25") +
    annotate("text", x = 0.05, y = 0.78,
             label = "Ottimizzazione: Markowitz mean-variance, bounded weights, long-only (quadprog QP).",
             size = 2.9, hjust = 0, colour = "grey25") +
    annotate("text", x = 0.05, y = 0.74,
             label = "Black-Litterman: viste da FRED (yield curve 10Y-2Y, CPI YoY, Fed Funds Rate).",
             size = 2.9, hjust = 0, colour = "grey25") +
    annotate("text", x = 0.05, y = 0.70,
             label = paste0("Backtest walk-forward: lookback ", lb, " mesi, ribilanciamento ogni ",
                            rf, " mesi, no look-ahead bias."),
             size = 2.9, hjust = 0, colour = "grey25") +
    annotate("text", x = 0.05, y = 0.66,
             label = "Risk contribution: w_i * (Sigma*w)_i / sigma_ptf (marginal risk contribution).",
             size = 2.9, hjust = 0, colour = "grey25") +
    annotate("text", x = 0.05, y = 0.62,
             label = "Calmar Ratio: rendimento annualizzato / |max drawdown| (rendimenti mensili walk-forward).",
             size = 2.9, hjust = 0, colour = "grey25") +
    annotate("segment", x = 0.02, xend = 0.98, y = 0.58, yend = 0.58,
             colour = "grey82", linewidth = 0.4) +
    annotate("text", x = 0.03, y = 0.54, label = "DISCLAIMER",
             size = 4, fontface = "bold", hjust = 0, colour = "#2C3E50") +
    annotate("text", x = 0.05, y = 0.49,
             label = "Questo report e' generato automaticamente a scopo informativo e NON costituisce consulenza finanziaria.",
             size = 2.9, hjust = 0, colour = "grey25") +
    annotate("text", x = 0.05, y = 0.45,
             label = "I rendimenti passati non sono indicativi di risultati futuri.",
             size = 2.9, hjust = 0, colour = "grey25") +
    annotate("text", x = 0.05, y = 0.41,
             label = "Dati macro: FRED (Federal Reserve Bank of St. Louis) — possibili ritardi di pubblicazione.",
             size = 2.9, hjust = 0, colour = "grey25") +
    annotate("text", x = 0.05, y = 0.37,
             label = "Prezzi ETF: Yahoo Finance tramite yfR (R). Dati storici potrebbero contenere errori o gap.",
             size = 2.9, hjust = 0, colour = "grey25") +
    annotate("text", x = 0.05, y = 0.33,
             label = "I parametri di ottimizzazione si basano su rendimenti storici e possono cambiare significativamente nel tempo.",
             size = 2.9, hjust = 0, colour = "grey25") +
    annotate("text", x = 0.05, y = 0.29,
             label = "Il modello Black-Litterman usa viste soggettive (macro rules) che introducono incertezza aggiuntiva.",
             size = 2.9, hjust = 0, colour = "grey25") +
    annotate("segment", x = 0.02, xend = 0.98, y = 0.24, yend = 0.24,
             colour = "grey82", linewidth = 0.4) +
    annotate("text", x = 0.5, y = 0.18,
             label = paste0("Pipeline: R {targets}  |  Ottimizzazione: quadprog  |  ",
                            "Dati: yfR, FRED  |  Grafici: ggplot2, gridExtra"),
             size = 2.9, hjust = 0.5, colour = "grey45") +
    annotate("text", x = 0.5, y = 0.12,
             label = paste0("Generato il ", format(Sys.Date(), "%d %B %Y"),
                            "  |  danielecampus.eu"),
             size = 3.2, hjust = 0.5, fontface = "bold", colour = "grey40") +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1.12)) +
    theme_void() +
    theme(plot.margin = margin(15, 15, 15, 15))
}


# =============================================================================
# MASTER REPORT FUNCTION
# =============================================================================

generate_portfolio_report <- function(ptf_name, cfg,
                                      output_path  = "output/",
                                      reports_path = "reports/",
                                      period       = "quarterly") {

  pinfo        <- get_period_info(period)
  period_label <- pinfo$label
  risk_free    <- cfg$global$risk_free %||% 0.03

  report_dir <- paste0(reports_path, ptf_name, "/")
  if (!dir.exists(report_dir)) dir.create(report_dir, recursive = TRUE)
  pdf_path <- paste0(report_dir, period_label, "_", ptf_name, ".pdf")

  # Load all parquet outputs
  .load <- function(f) tryCatch(arrow::read_parquet(f), error = function(e) NULL)

  ptf_summary      <- .load(paste0(output_path, ptf_name, "_summary.parquet"))
  ptf_analysis     <- .load(paste0(output_path, ptf_name, "_analysis.parquet"))
  bt_paths         <- .load(paste0(output_path, ptf_name, "_bt_paths.parquet"))
  bt_metrics       <- .load(paste0(output_path, ptf_name, "_bt_metrics.parquet"))
  univ_w           <- .load(paste0(output_path, ptf_name, "_universe_weights.parquet"))
  univ_summary     <- .load(paste0(output_path, ptf_name, "_universe_summary.parquet"))
  macro_w          <- .load(paste0(output_path, ptf_name, "_macro_universe_weights.parquet"))
  macro_summary    <- .load(paste0(output_path, ptf_name, "_macro_summary.parquet"))
  mc_ts            <- tryCatch(
    arrow::read_parquet(paste0(output_path, ptf_name, "_mc_forecast.parquet")) %>%
      arrange(Dates),
    error = function(e) NULL
  )
  mc_summary       <- .load(paste0(output_path, ptf_name, "_mc_summary.parquet"))
  macro_indicators <- .load(paste0(output_path, ptf_name, "_macro_indicators.parquet"))
  macro_views_txt  <- tryCatch(
    readLines(paste0(output_path, ptf_name, "_macro_views.txt"), warn = FALSE),
    error = function(e) character(0)
  )
  corr_df      <- .load(paste0(output_path, ptf_name, "_corr_matrix.parquet"))
  var_cov_long <- .load(paste0(output_path, ptf_name, "_var_cov_ann.parquet"))

  ptf_cfg       <- cfg$portfolios[[ptf_name]]
  current_w     <- setNames(as.numeric(ptf_cfg$quotes), ptf_cfg$assets)
  min_report    <- cfg$universe_optimization$min_weight_report %||% 0.02
  period_return <- compute_period_return(bt_paths, pinfo$period_start, pinfo$period_end)

  # Build and print each page inside the PDF device context.
  # This ensures ggplotGrob() (called inside arrangeGrob) has an active device
  # for font metrics — required in ggplot2 >= 3.5.
  safe <- function(expr, fallback) {
    tryCatch(expr, error = function(e) {
      message(sprintf("[Report][%s] Pagina fallita: %s", ptf_name, conditionMessage(e)))
      fallback
    })
  }

  pdf(pdf_path, width = 11, height = 8.5, paper = "a4r")
  on.exit(if (!is.null(dev.list())) dev.off(), add = TRUE)

  # Title page
  print(safe(plot_title_page(ptf_name, ptf_cfg, period_label),
             .placeholder(paste0("REPORT — ", toupper(ptf_name), " | ", period_label))))

  # Page 1 — Executive Summary
  p1 <- if (!is.null(ptf_summary) && !is.null(ptf_analysis)) {
    safe(plot_executive_summary(ptf_name, ptf_cfg, ptf_summary, ptf_analysis,
                                bt_metrics, macro_views_txt, period_label),
         ggplotGrob(.placeholder("Executive summary unavailable")))
  } else ggplotGrob(.placeholder("Summary/analysis data unavailable"))
  .print_page(p1)

  # Page 2 — Risk Decomposition
  p2 <- if (!is.null(ptf_analysis)) {
    safe(plot_risk_decomposition(ptf_name, ptf_analysis, period_label),
         ggplotGrob(.placeholder("Risk decomposition unavailable")))
  } else ggplotGrob(.placeholder("Analysis data unavailable"))
  .print_page(p2)

  # Page 3 — Correlation Heatmap
  p3 <- safe(plot_correlation_heatmap(corr_df, ptf_name, period_label),
             .placeholder("Correlation matrix unavailable — run tar_make()"))
  print(p3)

  # Page 4 — Macro Page
  p4 <- if (!is.null(macro_indicators)) {
    safe(plot_macro_page(ptf_name, macro_indicators, cfg$macro_rules, period_label),
         .placeholder("Macro indicators page failed"))
  } else .placeholder("Macro indicators unavailable — run tar_make()")
  print(p4)

  # Page 5 — Allocation Comparison
  p5 <- if (!is.null(ptf_summary)) {
    safe(plot_allocation_comparison_full(ptf_name, current_w, univ_w, macro_w,
                                         ptf_summary, univ_summary, macro_summary,
                                         min_report, period_label),
         ggplotGrob(.placeholder("Allocation comparison unavailable")))
  } else ggplotGrob(.placeholder("Summary data unavailable"))
  .print_page(p5)

  # Page 6 — Backtest & Drawdown
  p6 <- safe(plot_backtest_with_drawdown(bt_paths, ptf_name, period_label,
                                          pinfo$chart_start, pinfo$chart_end,
                                          pinfo$chart_months %||% 36L),
             ggplotGrob(.placeholder("Backtest unavailable")))
  .print_page(p6)

  # Page 7 — Monte Carlo
  p7 <- safe(plot_mc_from_parquet(mc_ts, mc_summary, ptf_name),
             .placeholder("Monte Carlo data unavailable"))
  print(p7)

  # Page 8 — Goal Projection (only if goal is defined)
  if (!is.null(ptf_cfg$goal) && !is.null(ptf_summary)) {
    p8 <- safe(plot_goal_calibrated(ptf_name, ptf_cfg$goal, ptf_summary, mc_summary,
                                     macro_summary, var_cov_long, ptf_analysis,
                                     period_label, risk_free),
               .placeholder("Goal projection unavailable"))
    print(p8)
  }

  # Page 9 — Methodology
  p9 <- safe(plot_methodology_page(cfg, period_label),
             .placeholder("Methodology page unavailable"))
  print(p9)

  dev.off()
  on.exit(NULL)  # cancel the safety on.exit since we closed cleanly

  invisible(pdf_path)
}
