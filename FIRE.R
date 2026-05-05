# =============================================================================
# FIRE.R — Standalone FIRE Planning Analysis
# Run: source("FIRE.R")  or  Rscript FIRE.R
# Output: output/FIRE_analysis.pdf (5 pages, A4 landscape)
# =============================================================================

source("library.R")

# ── User parameters ───────────────────────────────────────────────────────────
cash_out_nominal <- 50000        # target annual withdrawal (today's EUR)
swr_rates        <- c(0.035, 0.04)
PV               <- 5000         # current wealth (EUR)
CF_annual        <- 10000        # annual savings (real, constant)
t_accum          <- 35           # years of accumulation
t_retire         <- 30           # years of retirement (decumulation horizon)
inflation        <- 0.025        # annual inflation assumption
r_real_base      <- 0.05         # base real return for deterministic scenarios
n_sim            <- 5000
seed             <- 42
# ─────────────────────────────────────────────────────────────────────────────

set.seed(seed)

# ── Helpers ───────────────────────────────────────────────────────────────────

# FV of a constant-CF savings plan at real rate r over t years (annual compounding)
fv_accum <- function(r, pv, cf_annual, t_years) {
  pv * (1 + r)^t_years + cf_annual * ((1 + r)^t_years - 1) / r
}

# Required real return to reach FIRE target given PV, CF, t
solve_r_real <- function(fire_target, pv, cf_annual, t_years,
                         lower = -0.02, upper = 0.30) {
  f <- function(r) fv_accum(r, pv, cf_annual, t_years) - fire_target
  if (f(lower) > 0) return(lower)
  if (f(upper) < 0) return(NA_real_)
  tryCatch(uniroot(f, c(lower, upper))$root, error = function(e) NA_real_)
}

fmt_eur <- function(x, digits = 0)
  formatC(round(x, digits), format = "f", digits = digits, big.mark = ".", decimal.mark = ",")

# ── 1. Required returns & break-even ─────────────────────────────────────────

fire_numbers <- cash_out_nominal / swr_rates   # in today's EUR (real)

req_real <- sapply(fire_numbers, solve_r_real,
                   pv = PV, cf_annual = CF_annual, t_years = t_accum)
req_nom  <- (1 + req_real) * (1 + inflation) - 1

cat("\n══════════════════════════════════════════════════════\n")
cat("  FIRE PLANNING ANALYSIS\n")
cat("══════════════════════════════════════════════════════\n\n")
cat(sprintf("  Reddito annuo target: EUR %s\n", fmt_eur(cash_out_nominal)))
cat(sprintf("  Patrimonio attuale:   EUR %s\n", fmt_eur(PV)))
cat(sprintf("  Risparmio annuo:      EUR %s\n", fmt_eur(CF_annual)))
cat(sprintf("  Orizzonte accum.:     %d anni\n\n", t_accum))

cat("  SWR   | FIRE target (EUR) | r reale richiesto | r nominale richiesto\n")
cat("  ------+-------------------+-------------------+---------------------\n")
for (i in seq_along(swr_rates)) {
  cat(sprintf("  %.1f%%  |  %s         |   %.2f%%            |   %.2f%%\n",
              swr_rates[i] * 100,
              fmt_eur(fire_numbers[i]),
              req_real[i] * 100,
              req_nom[i] * 100))
}

fire_target_4 <- fire_numbers[which.min(abs(swr_rates - 0.04))]
shortfall     <- fire_target_4 - PV
yrs_zero      <- shortfall / CF_annual
# Exact: (PV + CF/r)*(1+r)^t = fire_target + CF/r  →  t = log((fire+CF/r)/(PV+CF/r)) / log(1+r)
yrs_base      <- log((fire_target_4 + CF_annual / r_real_base) /
                     (PV + CF_annual / r_real_base)) / log(1 + r_real_base)

cat(sprintf("\n  Break-even (SWR 4%%):\n"))
cat(sprintf("    FIRE number:   EUR %s\n", fmt_eur(fire_target_4)))
cat(sprintf("    Shortfall:     EUR %s\n", fmt_eur(shortfall)))
cat(sprintf("    Anni a r=0%%:   %.1f anni\n", yrs_zero))
cat(sprintf("    Anni a r=%.0f%%:  %.1f anni (appross.)\n",
            r_real_base * 100, yrs_base))
cat("\n══════════════════════════════════════════════════════\n\n")

# ── 2. Sensitivity heatmap data ───────────────────────────────────────────────

t_vec  <- seq(10, 45, by = 5)
cf_vec <- seq(5000, 50000, by = 5000)

heat_grid <- expand.grid(t = t_vec, CF = cf_vec)
heat_grid$r_real <- mapply(function(t, cf)
  solve_r_real(fire_target_4, PV, cf, t),
  heat_grid$t, heat_grid$CF)

heat_grid$r_pct    <- round(heat_grid$r_real * 100, 1)
heat_grid$label    <- ifelse(is.na(heat_grid$r_pct), "NA",
                             paste0(format(heat_grid$r_pct, nsmall = 1), "%"))
heat_grid$achievable <- !is.na(heat_grid$r_real) & heat_grid$r_real <= r_real_base

p_heatmap <- ggplot(heat_grid, aes(x = factor(t), y = factor(CF / 1000))) +
  geom_tile(aes(fill = r_real), color = "white") +
  geom_text(aes(label = label,
                color = ifelse(achievable, "white", "grey20")),
            size = 3.2, fontface = "bold") +
  scale_fill_gradient2(
    low = "#78B87A", mid = "#F5C842", high = "#E08080",
    midpoint = r_real_base,
    name = "r reale",
    labels = scales::percent_format(accuracy = 0.1),
    na.value = "grey80"
  ) +
  scale_color_identity() +
  scale_x_discrete(name = "Anni di accumulo") +
  scale_y_discrete(name = "Risparmio annuo (k EUR)") +
  labs(
    title    = sprintf("Heatmap: tasso reale richiesto per FIRE @ SWR 4%% (target EUR %s)",
                       fmt_eur(fire_target_4)),
    subtitle = sprintf("Verde = raggiungibile con r ≤ %.0f%% (ipotesi base) | PV = EUR %s",
                       r_real_base * 100, fmt_eur(PV))
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10),
        legend.position = "right")

# ── 3. Deterministic accumulation + decumulation ──────────────────────────────

r_scenarios <- c(
  "Pessimista"  = r_real_base - 0.02,
  "Base"        = r_real_base,
  "Ottimista"   = r_real_base + 0.02
)
scen_colors <- c("Pessimista" = "#E08080", "Base" = "#5D7FA8", "Ottimista" = "#78B87A")

n_accum_m <- t_accum  * 12L
n_retire_m <- t_retire * 12L
n_total_m  <- n_accum_m + n_retire_m

det_paths <- do.call(rbind, lapply(names(r_scenarios), function(nm) {
  r <- r_scenarios[nm]
  r_m <- (1 + r)^(1/12) - 1
  v   <- numeric(n_total_m + 1L)
  v[1] <- PV

  # Accumulation
  for (m in seq_len(n_accum_m))
    v[m + 1] <- v[m] * (1 + r_m) + CF_annual / 12

  # Decumulation
  for (m in seq(n_accum_m + 1L, n_total_m))
    v[m + 1] <- max(0, v[m] * (1 + r_m) - cash_out_nominal / 12)

  data.frame(
    Month    = 0:n_total_m,
    Year     = (0:n_total_m) / 12,
    Value    = v,
    Scenario = nm
  )
}))

fire_line_4 <- fire_target_4

p_det <- ggplot(det_paths, aes(x = Year, y = Value / 1e3, color = Scenario)) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = t_accum, linetype = "dashed", color = "grey40", linewidth = 0.7) +
  geom_hline(yintercept = fire_line_4 / 1e3, linetype = "dotted",
             color = "#E08080", linewidth = 0.8) +
  annotate("text", x = t_accum + 0.5, y = max(det_paths$Value) * 0.95 / 1e3,
           label = "Pensionamento", hjust = 0, size = 3.5, color = "grey40") +
  annotate("text", x = 1, y = fire_line_4 / 1e3 * 1.03,
           label = sprintf("FIRE target @ 4%%: EUR %sk", round(fire_line_4 / 1e3)),
           hjust = 0, size = 3.2, color = "#E08080") +
  scale_color_manual(values = scen_colors, name = "Scenario") +
  scale_y_continuous(labels = function(x) paste0("€", x, "k")) +
  labs(
    title    = "Proiezione deterministica: accumulo + decumulo",
    subtitle = sprintf("Ritorni reali: Pessimista %.0f%% | Base %.0f%% | Ottimista %.0f%%  |  CF annuo: EUR %s  |  Prelievo: EUR %s/anno",
                       (r_real_base - 0.02) * 100, r_real_base * 100, (r_real_base + 0.02) * 100,
                       fmt_eur(CF_annual), fmt_eur(cash_out_nominal)),
    x = "Anno", y = "Patrimonio (EUR k)"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 9),
        legend.position = "bottom")

# ── 4. Monte Carlo bootstrap ──────────────────────────────────────────────────

returns_parquet <- "input/data_returns.parquet"
if (file.exists(returns_parquet)) {
  ret_df  <- read_parquet(returns_parquet)
  ret_mat <- ret_df %>% select(-Dates) %>% as.matrix()
  ret_mat <- ret_mat[complete.cases(ret_mat), , drop = FALSE]
  # equal-weight portfolio monthly return
  ptf_ret <- rowMeans(ret_mat)
  message(sprintf("MC bootstrap: %d historical monthly returns loaded (%d assets)",
                  length(ptf_ret), ncol(ret_mat)))
  use_bootstrap <- TRUE
} else {
  warning("data_returns.parquet not found — using parametric normal fallback")
  r_m_base <- (1 + r_real_base)^(1/12) - 1
  ptf_ret  <- NULL
  use_bootstrap <- FALSE
}

# Draw n_sim × n_total_m return matrix
n_obs <- if (use_bootstrap) length(ptf_ret) else 0L
if (use_bootstrap) {
  idx_mc  <- matrix(sample(n_obs, size = n_sim * n_total_m, replace = TRUE),
                    nrow = n_sim, ncol = n_total_m)
  ret_mc  <- matrix(ptf_ret[idx_mc], nrow = n_sim, ncol = n_total_m)
} else {
  r_m_base <- (1 + r_real_base)^(1/12) - 1
  ret_mc   <- matrix(rnorm(n_sim * n_total_m,
                           mean = r_m_base,
                           sd   = 0.04 / sqrt(12)),
                     nrow = n_sim, ncol = n_total_m)
}

# Simulate paths
mc_paths <- matrix(NA_real_, nrow = n_sim, ncol = n_total_m + 1L)
mc_paths[, 1L] <- PV

for (m in seq_len(n_accum_m))
  mc_paths[, m + 1L] <- mc_paths[, m] * (1 + ret_mc[, m]) + CF_annual / 12

for (m in seq(n_accum_m + 1L, n_total_m))
  mc_paths[, m + 1L] <- pmax(0, mc_paths[, m] * (1 + ret_mc[, m]) - cash_out_nominal / 12)

wealth_at_retire <- mc_paths[, n_accum_m + 1L]
p_fire           <- mean(wealth_at_retire >= fire_target_4)

ruin_by_retire <- apply(mc_paths[, (n_accum_m + 1L):(n_total_m + 1L)], 1,
                        function(x) any(x <= 0))
p_ruin <- mean(ruin_by_retire)

# Quantile bands across full horizon
mc_quantiles <- apply(mc_paths, 2, quantile, probs = c(0.05, 0.50, 0.95), na.rm = TRUE)
mc_band <- data.frame(
  Year  = (0:n_total_m) / 12,
  P5    = mc_quantiles[1, ],
  P50   = mc_quantiles[2, ],
  P95   = mc_quantiles[3, ]
)

# ── Plot 4: MC distribution at retirement ─────────────────────────────────────

retire_df <- data.frame(wealth = wealth_at_retire / 1e3)

p_mc_dist <- ggplot(retire_df, aes(x = wealth)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60,
                 fill = "#5D7FA8", alpha = 0.7, color = "white") +
  geom_vline(xintercept = fire_target_4 / 1e3, color = "#E08080",
             linewidth = 1, linetype = "dashed") +
  annotate("text", x = fire_target_4 / 1e3 * 1.02,
           y = max(density(wealth_at_retire / 1e3)$y) * 0.8,
           label = sprintf("FIRE @ 4%%\nEUR %sk", round(fire_target_4 / 1e3)),
           hjust = 0, size = 3.2, color = "#E08080") +
  scale_x_continuous(labels = function(x) paste0("€", x, "k")) +
  labs(
    title    = sprintf("MC: distribuzione del patrimonio a %d anni (pensionamento)", t_accum),
    subtitle = sprintf("n_sim = %d  |  P(FIRE @ 4%%) = %.1f%%  |  P(rovina in pensione) = %.1f%%  |  Metodo: %s",
                       n_sim, p_fire * 100, p_ruin * 100,
                       if (use_bootstrap) "bootstrap storico" else "normale parametrica"),
    x = "Patrimonio (EUR k)",
    y = "Densità"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 9))

# ── Plot 5a: survival curve ───────────────────────────────────────────────────

retire_phase <- mc_paths[, (n_accum_m + 1L):(n_total_m + 1L)]
survival_pct <- colMeans(retire_phase > 0) * 100
surv_df      <- data.frame(
  Year     = seq(0, t_retire, length.out = ncol(retire_phase)),
  Survival = survival_pct
)

p_survival <- ggplot(surv_df, aes(x = Year, y = Survival)) +
  geom_line(color = "#5D7FA8", linewidth = 1.2) +
  geom_hline(yintercept = 90, linetype = "dashed", color = "#E08080", linewidth = 0.7) +
  annotate("text", x = 1, y = 91.5, label = "Soglia 90%",
           color = "#E08080", size = 3.2) +
  scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
  labs(
    title    = "Curva di sopravvivenza del portafoglio in pensione",
    subtitle = sprintf("Probabilità che il patrimonio > 0 per anno di pensione  |  Prelievo: EUR %s/anno",
                       fmt_eur(cash_out_nominal)),
    x = "Anno di pensione",
    y = "P(patrimonio > 0)"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 9))

# ── Plot 5b: full P5/P50/P95 band chart (both phases) ─────────────────────────

p_mc_band <- ggplot(mc_band, aes(x = Year)) +
  geom_ribbon(aes(ymin = P5 / 1e3, ymax = P95 / 1e3), fill = "#5D7FA8", alpha = 0.2) +
  geom_line(aes(y = P50 / 1e3), color = "#5D7FA8", linewidth = 1) +
  geom_vline(xintercept = t_accum, linetype = "dashed", color = "grey40", linewidth = 0.7) +
  geom_hline(yintercept = fire_target_4 / 1e3, linetype = "dotted",
             color = "#E08080", linewidth = 0.8) +
  annotate("text", x = t_accum + 0.5,
           y = max(mc_band$P95, na.rm = TRUE) * 0.95 / 1e3,
           label = "Pensionamento", hjust = 0, size = 3.5, color = "grey40") +
  annotate("text", x = 1, y = fire_target_4 / 1e3 * 1.03,
           label = sprintf("FIRE target: EUR %sk", round(fire_target_4 / 1e3)),
           hjust = 0, size = 3.2, color = "#E08080") +
  scale_y_continuous(labels = function(x) paste0("€", x, "k")) +
  labs(
    title    = "MC: banda P5/P50/P95 — accumulo + decumulo",
    subtitle = sprintf("n_sim = %d  |  P(FIRE @ 4%%) = %.1f%%  |  P(rovina) = %.1f%%  |  Metodo: %s",
                       n_sim, p_fire * 100, p_ruin * 100,
                       if (use_bootstrap) "bootstrap storico" else "normale parametrica"),
    x = "Anno", y = "Patrimonio (EUR k)"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title    = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 9))

# ── Summary table (page 1) ────────────────────────────────────────────────────

tbl_data <- data.frame(
  Metrica   = c(
    "Reddito annuo target",
    "Patrimonio attuale",
    "Risparmio annuo",
    "Orizzonte accumulo",
    "Orizzonte pensione",
    "Inflazione assunta",
    "",
    "FIRE # (SWR 3.5%)",
    "FIRE # (SWR 4.0%)",
    "",
    "r reale richiesto (SWR 3.5%)",
    "r reale richiesto (SWR 4.0%)",
    "r nominale richiesto (SWR 3.5%)",
    "r nominale richiesto (SWR 4.0%)",
    "",
    "Shortfall attuale (@ SWR 4%)",
    "Anni a r=0%",
    sprintf("Anni a r=%.0f%% reale (appross.)", r_real_base * 100),
    "",
    "P(FIRE @ 4%) — MC",
    "P(rovina in pensione) — MC"
  ),
  Valore    = c(
    paste0("EUR ", fmt_eur(cash_out_nominal)),
    paste0("EUR ", fmt_eur(PV)),
    paste0("EUR ", fmt_eur(CF_annual)),
    paste0(t_accum, " anni"),
    paste0(t_retire, " anni"),
    paste0(inflation * 100, "%"),
    "",
    paste0("EUR ", fmt_eur(fire_numbers[1])),
    paste0("EUR ", fmt_eur(fire_numbers[2])),
    "",
    paste0(round(req_real[1] * 100, 2), "%"),
    paste0(round(req_real[2] * 100, 2), "%"),
    paste0(round(req_nom[1]  * 100, 2), "%"),
    paste0(round(req_nom[2]  * 100, 2), "%"),
    "",
    paste0("EUR ", fmt_eur(shortfall)),
    paste0(round(yrs_zero, 1), " anni"),
    paste0(round(yrs_base, 1), " anni"),
    "",
    paste0(round(p_fire * 100, 1), "%"),
    paste0(round(p_ruin * 100, 1), "%")
  ),
  stringsAsFactors = FALSE
)

p_table <- tableGrob(
  tbl_data,
  rows  = NULL,
  theme = ttheme_minimal(
    core    = list(fg_params = list(hjust = 0, x = 0.05, fontsize = 10)),
    colhead = list(fg_params = list(hjust = 0, x = 0.05, fontsize = 11, fontface = "bold"))
  )
)

title_grob <- grid::textGrob(
  label = "FIRE Planning Summary",
  gp    = grid::gpar(fontsize = 16, fontface = "bold")
)
sub_grob <- grid::textGrob(
  label = sprintf("Analisi al %s | Metodo MC: %s",
                  format(Sys.Date(), "%d/%m/%Y"),
                  if (use_bootstrap) "bootstrap storico da data_returns.parquet"
                  else "normale parametrica (fallback)"),
  gp = grid::gpar(fontsize = 10, col = "grey40")
)

page1 <- arrangeGrob(title_grob, sub_grob, p_table,
                      ncol = 1, heights = c(0.08, 0.05, 0.87))

# ── Save PDF ──────────────────────────────────────────────────────────────────

if (!dir.exists("output")) dir.create("output")
pdf("output/FIRE_analysis.pdf", width = 11, height = 8.5)
  grid.draw(page1)
  print(p_heatmap)
  print(p_det)
  print(p_mc_dist)
  print(arrangeGrob(p_survival, p_mc_band, ncol = 2))
dev.off()

message("Saved: output/FIRE_analysis.pdf (5 pages)")
