# =============================================================================
# R/fun_optimization.R
# Optimization functions: goal-based (FIRE), Markowitz, Black-Litterman.
# =============================================================================


# -----------------------------------------------------------------------------
# 1. GOAL-BASED: required return calculation
# -----------------------------------------------------------------------------

#' Compute the annual return needed to reach a financial goal
#'
#' Solves the future-value-of-annuity equation for r:
#'   FV = PV * (1 + r_m)^n + CF_m * ((1 + r_m)^n - 1) / r_m
#' where n = 12 * t months, r_m is the monthly rate, CF_m is monthly contribution.
#'
#' @param PV  Current portfolio value
#' @param FV  Target terminal value
#' @param t   Horizon in years
#' @param CF  Monthly contribution (assumed end-of-month)
#' @param search_range Vector of (min, max) annual rate to bracket the root
#' @return Annual rate of return required (numeric, e.g. 0.07 = 7%)
calculate_required_return <- function(PV, FV, t, CF, search_range = c(-0.05, 0.30)) {
  n <- 12 * t

  # Function returning 0 when r_m solves the equation
  fv_equation <- function(r_annual) {
    r_m <- (1 + r_annual)^(1/12) - 1
    if (abs(r_m) < 1e-10) {
      # Degenerate case: zero rate
      return(PV + CF * n - FV)
    }
    PV * (1 + r_m)^n + CF * ((1 + r_m)^n - 1) / r_m - FV
  }

  # Try to bracket a root within the search range
  f_lo <- fv_equation(search_range[1])
  f_hi <- fv_equation(search_range[2])

  if (sign(f_lo) == sign(f_hi)) {
    if (f_hi < 0) {
      # Even at max rate the goal is not reachable
      return(NA_real_)
    }
    if (f_lo > 0) {
      # Even at min (or negative) rate the goal is over-reached
      return(search_range[1])
    }
  }

  uniroot(fv_equation, search_range, tol = 1e-8)$root
}


#' Project terminal value, contributions and gains given a return assumption
#'
#' @param PV  Current value
#' @param r   Annual return (e.g. 0.07)
#' @param t   Horizon in years
#' @param CF  Monthly contribution
#' @return data.frame with terminal value, contributions, gains
project_goal <- function(PV, r, t, CF) {
  r_m <- (1 + r)^(1/12) - 1
  n   <- 12 * t

  if (abs(r_m) < 1e-10) {
    terminal_value <- PV + CF * n
  } else {
    terminal_value <- PV * (1 + r_m)^n + CF * ((1 + r_m)^n - 1) / r_m
  }

  total_contributions <- PV + CF * n
  total_gains         <- terminal_value - total_contributions

  data.frame(
    PV                  = PV,
    Monthly_CF          = CF,
    Horizon_years       = t,
    Annual_return       = r,
    Total_contributions = total_contributions,
    Terminal_value      = terminal_value,
    Total_gains         = total_gains
  )
}


# -----------------------------------------------------------------------------
# 2. MARKOWITZ OPTIMIZATION
# -----------------------------------------------------------------------------

#' Global minimum variance portfolio (no return constraint)
#'
#' @param Sigma     Covariance matrix (n x n)
#' @param long_only If TRUE, enforce w_i >= 0
#' @return Vector of optimal weights (sums to 1)
markowitz_min_variance <- function(Sigma, long_only = TRUE) {
  n <- nrow(Sigma)

  Dmat <- 2 * Sigma
  dvec <- rep(0, n)

  if (long_only) {
    # Constraints: sum(w) = 1 (eq), w_i >= 0 (ineq)
    Amat <- cbind(rep(1, n), diag(n))
    bvec <- c(1, rep(0, n))
    meq  <- 1
  } else {
    Amat <- matrix(rep(1, n), ncol = 1)
    bvec <- c(1)
    meq  <- 1
  }

  sol <- quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq = meq)
  sol$solution
}


#' Markowitz min-variance for a target return
#'
#' Solves: min w'Sigma w  s.t.  w'mu = target_return, sum(w) = 1, w_i >= 0
#'
#' @param mu            Vector of expected returns (annualized or monthly,
#'                      consistent with target_return)
#' @param Sigma         Covariance matrix (same frequency as mu)
#' @param target_return Required return (same scale as mu)
#' @param long_only     If TRUE, enforce w_i >= 0
#' @return Vector of optimal weights, or NULL if no feasible solution
markowitz_target_return <- function(mu, Sigma, target_return, long_only = TRUE) {
  n <- length(mu)

  Dmat <- 2 * Sigma
  dvec <- rep(0, n)

  # Equality constraints: sum(w) = 1, w'mu = target_return
  if (long_only) {
    Amat <- cbind(rep(1, n), mu, diag(n))
    bvec <- c(1, target_return, rep(0, n))
    meq  <- 2
  } else {
    Amat <- cbind(rep(1, n), mu)
    bvec <- c(1, target_return)
    meq  <- 2
  }

  result <- tryCatch(
    quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq = meq),
    error = function(e) NULL
  )

  if (is.null(result)) NULL else result$solution
}


#' Markowitz max Sharpe portfolio
#'
#' Iterates over the efficient frontier and selects the point with max Sharpe.
#' Closed-form unconstrained solution exists but breaks under long-only.
#'
#' @param mu          Vector of expected returns (annualized)
#' @param Sigma       Covariance matrix (annualized)
#' @param risk_free   Annual risk-free rate
#' @param n_points    Number of frontier points to evaluate
#' @param long_only   If TRUE, enforce w_i >= 0
#' @return List with weights, expected_return, volatility, sharpe
markowitz_max_sharpe <- function(mu, Sigma, risk_free = 0.03,
                                 n_points = 200, long_only = TRUE) {
  frontier <- efficient_frontier(mu, Sigma, n_points = n_points,
                                 long_only = long_only)
  if (is.null(frontier) || nrow(frontier) == 0) return(NULL)

  frontier$Sharpe <- (frontier$Return - risk_free) / frontier$Risk
  best_idx <- which.max(frontier$Sharpe)
  if (length(best_idx) == 0L) return(NULL)
  best <- frontier[best_idx, ]

  asset_names <- names(mu)
  weight_cols <- setdiff(names(best), c("Return", "Risk", "Sharpe"))
  weights     <- as.numeric(best[, weight_cols])
  if (length(weights) != length(asset_names)) return(NULL)
  names(weights) <- asset_names

  list(
    weights         = weights,
    expected_return = best$Return,
    volatility      = best$Risk,
    sharpe          = best$Sharpe
  )
}


#' Compute the efficient frontier
#'
#' @param mu        Vector of expected returns
#' @param Sigma     Covariance matrix
#' @param n_points  Number of frontier points
#' @param long_only If TRUE, enforce w_i >= 0
#' @return data.frame with Return, Risk, and one column per asset weight
efficient_frontier <- function(mu, Sigma, n_points = 100, long_only = TRUE) {
  asset_names <- names(mu)

  # Range of target returns: from min-variance to max single-asset return
  mvp_w   <- markowitz_min_variance(Sigma, long_only = long_only)
  r_min   <- as.numeric(mvp_w %*% mu)
  r_max   <- if (long_only) max(mu) else max(mu) * 1.2

  if (r_max <= r_min) return(NULL)
  targets <- seq(r_min, r_max, length.out = n_points)

  rows <- lapply(targets, function(r) {
    w <- markowitz_target_return(mu, Sigma, r, long_only = long_only)
    if (is.null(w)) return(NULL)
    list(
      Return  = sum(w * mu),
      Risk    = sqrt(as.numeric(t(w) %*% Sigma %*% w)),
      Weights = w
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(NULL)

  weights_mat <- do.call(rbind, lapply(rows, `[[`, "Weights"))
  colnames(weights_mat) <- asset_names

  data.frame(
    Return = sapply(rows, `[[`, "Return"),
    Risk   = sapply(rows, `[[`, "Risk"),
    weights_mat
  )
}


# -----------------------------------------------------------------------------
# 3. GOAL-BASED OPTIMIZATION (combines required return + Markowitz)
# -----------------------------------------------------------------------------

#' Optimize weights to meet a financial goal with minimum risk
#'
#' Workflow:
#'  1. Compute required annual return from PV, FV, t, CF
#'  2. Check feasibility against the efficient frontier
#'  3. Solve min-variance for the required target
#'
#' @param ptf       Portfolio object (output of build_portfolio)
#' @param goal      List with PV, FV, t, CF
#' @param risk_free Annual risk-free rate
#' @return List with required_return, feasibility, weights, projection
optimize_for_goal <- function(ptf, goal, risk_free = 0.03) {
  # Annualized expected returns and covariance from monthly history
  mu_monthly <- as.numeric(ptf$avg_returns)
  names(mu_monthly) <- ptf$assets
  mu          <- (1 + mu_monthly)^12 - 1
  Sigma       <- ptf$var_cov * 12

  # 1) Required return
  r_req <- calculate_required_return(
    PV = goal$PV, FV = goal$FV, t = goal$t, CF = goal$CF
  )

  # 2) Feasibility check: build the frontier and compare
  frontier <- efficient_frontier(mu, Sigma, n_points = 200, long_only = TRUE)
  if (is.null(frontier)) {
    return(list(status = "ERROR: cannot compute frontier"))
  }
  r_min_frontier <- min(frontier$Return)
  r_max_frontier <- max(frontier$Return)

  feasibility <- dplyr::case_when(
    is.na(r_req)                 ~ "UNREACHABLE",
    r_req > r_max_frontier       ~ "UNREACHABLE",
    r_req < r_min_frontier       ~ "TRIVIAL",
    TRUE                          ~ "FEASIBLE"
  )

  # 3) Solve for the appropriate point on the frontier
  if (feasibility == "UNREACHABLE") {
    weights <- NULL
    realized_return <- NA_real_
    realized_vol    <- NA_real_
  } else if (feasibility == "TRIVIAL") {
    # Use global min-variance: required return is below frontier minimum
    weights <- markowitz_min_variance(Sigma, long_only = TRUE)
    names(weights)  <- ptf$assets
    realized_return <- as.numeric(weights %*% mu)
    realized_vol    <- sqrt(as.numeric(t(weights) %*% Sigma %*% weights))
  } else {
    weights <- markowitz_target_return(mu, Sigma, r_req, long_only = TRUE)
    names(weights)  <- ptf$assets
    realized_return <- as.numeric(weights %*% mu)
    realized_vol    <- sqrt(as.numeric(t(weights) %*% Sigma %*% weights))
  }

  # 4) Projection
  projection <- if (!is.na(r_req) && feasibility != "UNREACHABLE") {
    project_goal(goal$PV, realized_return, goal$t, goal$CF)
  } else NULL

  list(
    status            = feasibility,
    required_return   = r_req,
    realized_return   = realized_return,
    realized_vol      = realized_vol,
    sharpe            = (realized_return - risk_free) / realized_vol,
    weights           = weights,
    projection        = projection,
    frontier          = frontier,
    frontier_range    = c(r_min_frontier, r_max_frontier)
  )
}


# -----------------------------------------------------------------------------
# 3b. SAVE FUNCTIONS FOR GOAL AND MACRO OUTPUTS
# -----------------------------------------------------------------------------

#' Save goal-based optimization results to parquet
#'
#' @param ptf_name    Portfolio name
#' @param goal_opt    Output of optimize_for_goal()
#' @param output_path Path to output folder
#' @return Vector of saved file paths
save_goal_output <- function(ptf_name, goal_opt, output_path) {
  if (is.null(goal_opt) || !is.list(goal_opt) || goal_opt$status == "ERROR: cannot compute frontier") {
    return(character(0))
  }

  paths <- c(
    weights = paste0(output_path, ptf_name, "_goal_weights.parquet"),
    summary = paste0(output_path, ptf_name, "_goal_summary.parquet")
  )

  weights_df <- if (!is.null(goal_opt$weights)) {
    data.frame(Asset = names(goal_opt$weights), Weight = as.numeric(goal_opt$weights))
  } else {
    data.frame(Asset = character(0), Weight = numeric(0))
  }

  summary_df <- data.frame(
    Status          = goal_opt$status,
    Required_Return = goal_opt$required_return,
    Realized_Return = goal_opt$realized_return,
    Volatility      = goal_opt$realized_vol,
    Sharpe          = goal_opt$sharpe
  )

  arrow::write_parquet(weights_df, paths["weights"])
  arrow::write_parquet(summary_df, paths["summary"])
  unname(paths)
}


#' Save macro-tilted (Black-Litterman) optimization results to parquet
#'
#' @param ptf_name    Portfolio name
#' @param macro_opt   Output of optimize_macro()
#' @param output_path Path to output folder
#' @return Vector of saved file paths
save_macro_output <- function(ptf_name, macro_opt, output_path) {
  paths <- c(
    weights     = paste0(output_path, ptf_name, "_macro_weights.parquet"),
    summary     = paste0(output_path, ptf_name, "_macro_summary.parquet"),
    attribution = paste0(output_path, ptf_name, "_macro_attribution.parquet"),
    views_txt   = paste0(output_path, ptf_name, "_macro_views.txt"),
    indicators  = paste0(output_path, ptf_name, "_macro_indicators.parquet")
  )

  weights_df <- data.frame(
    Asset  = names(macro_opt$weights),
    Weight = as.numeric(macro_opt$weights)
  )

  summary_df <- data.frame(
    Expected_Return = macro_opt$expected_return,
    Volatility      = macro_opt$volatility,
    Sharpe          = macro_opt$sharpe,
    N_active_views  = length(macro_opt$views$narrative),
    Max_Weight      = macro_opt$max_weight_used %||% NA_real_,
    Max_Abs_Tilt    = macro_opt$max_abs_tilt    %||% NA_real_
  )

  # Attribution: shows each weight as a move from current -> strategic anchor ->
  # macro-tilted, so the report can explain WHY each weight moved (C6).
  assets <- names(macro_opt$weights)
  pick   <- function(v) if (is.null(v)) rep(NA_real_, length(assets)) else as.numeric(v[assets])
  attribution_df <- data.frame(
    Asset        = assets,
    Class        = if (!is.null(macro_opt$classes)) unname(macro_opt$classes[assets]) else NA_character_,
    Current      = pick(macro_opt$current_weights),
    Equilibrium  = pick(macro_opt$weights_equilibrium),
    Macro_Tilted = pick(macro_opt$weights),
    stringsAsFactors = FALSE
  )
  attribution_df$Delta_Tilt <- attribution_df$Macro_Tilted - attribution_df$Equilibrium

  arrow::write_parquet(weights_df,     paths["weights"])
  arrow::write_parquet(summary_df,     paths["summary"])
  arrow::write_parquet(attribution_df, paths["attribution"])
  narrative_lines <- as.character(unlist(macro_opt$views$narrative))
  if (length(narrative_lines) == 0) narrative_lines <- "(no active macro views)"
  writeLines(narrative_lines, paths["views_txt"])

  # Save indicator snapshot for offline report rendering
  ind <- macro_opt$indicators
  sv  <- function(x) if (is.null(x) || is.na(x$latest)) NA_real_ else as.numeric(x$latest)
  sd  <- function(x) if (is.null(x) || all(is.na(x$date))) NA_character_ else as.character(x$date)
  cpi_date <- if (!is.null(ind$cpi_yoy$series) && nrow(ind$cpi_yoy$series) > 0)
    as.character(tail(ind$cpi_yoy$series$date, 1)) else NA_character_

  indicators_df <- data.frame(
    Indicator = c("Yield_Curve_10Y2Y", "CPI_YoY", "Fed_Funds",
                  "HY_Spread", "Breakeven_5Y", "VIX"),
    Label     = c("Curva Tassi (10Y-2Y)", "CPI (YoY)", "Fed Funds Rate",
                  "HY Spread (OAS)", "Breakeven 5Y", "VIX"),
    Value     = c(sv(ind$yield_curve_10y2y), compute_yoy(ind$cpi_yoy$series),
                  sv(ind$fed_funds), sv(ind$hy_spread),
                  sv(ind$breakeven_5y), sv(ind$vix)),
    Date      = c(sd(ind$yield_curve_10y2y), cpi_date, sd(ind$fed_funds),
                  sd(ind$hy_spread), sd(ind$breakeven_5y), sd(ind$vix)),
    Unit      = c("%", "%", "%", "%", "%", "index"),
    stringsAsFactors = FALSE
  )
  arrow::write_parquet(indicators_df, paths["indicators"])

  unname(paths)
}


# -----------------------------------------------------------------------------
# 4. BLACK-LITTERMAN
# -----------------------------------------------------------------------------

# Default asset-class regex map — fallback when config has no `asset_classes`.
# Kept in sync with config.yaml `asset_classes`. Order matters: first match wins.
DEFAULT_ASSET_CLASSES <- list(
  equity    = paste0("MSCI World|World Momentum|World Quality|World Low Volatility|",
                     "World Value|Europe RAFI|Health Care|Consumer Staples|Small Cap|",
                     "Emerging|EM IMI|Europe 600|STOXX|World Small|China|All Country"),
  bond      = "Gov bonds|Corp.*bonds|High-Yield|Inflation-Linked|TIPS|linker",
  cash      = "Overnight|Short Treasury|Short.*Bond|money market",
  gold      = "GOLD|Gold|ETC GOLD",
  commodity = "Commodit|Oil|Energy|Metals|Agricol"
)

#' Classify asset display names into broad classes (single source of truth)
#'
#' Shared by the macro view engine, the strategic prior, the 60/40 backtest
#' benchmark and the report colours, so all four agree on what is equity / bond /
#' cash / gold / commodity. First matching pattern wins; unmatched -> "other".
#'
#' @param asset_names Character vector of human-readable asset labels
#' @param class_map   Ordered named list class -> regex (e.g. cfg$asset_classes).
#'                    If NULL, falls back to DEFAULT_ASSET_CLASSES.
#' @return Named character vector (names = asset_names) of class labels
classify_assets <- function(asset_names, class_map = NULL) {
  if (is.null(class_map) || length(class_map) == 0) class_map <- DEFAULT_ASSET_CLASSES
  vapply(asset_names, function(name) {
    for (cls in names(class_map)) {
      if (grepl(class_map[[cls]], name, ignore.case = TRUE)) return(cls)
    }
    "other"
  }, character(1L), USE.NAMES = TRUE)
}

#' Build a strategic-asset-allocation market-cap proxy from class shares
#'
#' Each class share is split equally across the assets of that class, then the
#' shares are renormalised over the classes actually present in the portfolio.
#' This is the BL equilibrium anchor — a broad, stable target, NOT the current
#' portfolio weights (which would bias the result toward the status quo).
#'
#' @param classes      Named vector asset -> class (from classify_assets)
#' @param prior_shares Named list/vector class -> target share (cfg$macro_prior)
#' @return Named vector of weights summing to 1, in names(classes) order
strategic_prior <- function(classes, prior_shares) {
  asset_names <- names(classes)
  n           <- length(asset_names)
  equal       <- setNames(rep(1 / n, n), asset_names)
  if (is.null(prior_shares) || length(prior_shares) == 0) return(equal)

  shares  <- unlist(prior_shares)
  present <- intersect(unique(classes), names(shares))
  present <- present[shares[present] > 0]
  if (length(present) == 0) return(equal)   # no class matched -> equal weight

  tot <- sum(shares[present])
  w   <- setNames(rep(0, n), asset_names)
  for (cls in present) {
    members <- asset_names[classes == cls]
    if (length(members) > 0) w[members] <- (shares[[cls]] / tot) / length(members)
  }
  if (sum(w) <= 0) return(equal)            # assets only in zero-share classes
  w / sum(w)
}

#' Shrink a covariance matrix toward a constant-correlation target
#'
#' Ledoit-Wolf-style regularisation with a fixed intensity: keeps the sample
#' variances, pulls the pairwise correlations toward their average. Stabilises a
#' near-singular covariance before it is inverted in the BL formula, without
#' adding a package dependency. Deterministic (reproducible).
#'
#' @param S     Sample covariance matrix
#' @param delta Shrinkage intensity in [0, 1] (0 = no shrinkage)
#' @return Shrunk covariance matrix with the same dimnames as S
shrink_cov <- function(S, delta = 0.15) {
  if (is.null(delta) || delta <= 0 || ncol(S) < 2) return(S)
  d      <- sqrt(diag(S))
  if (any(d <= 0)) return(S)
  R      <- S / outer(d, d)                 # sample correlation
  r_bar  <- mean(R[upper.tri(R)])           # average pairwise correlation
  F_corr <- matrix(r_bar, nrow(S), ncol(S))
  diag(F_corr) <- 1
  Sigma_s <- (1 - delta) * S + delta * (F_corr * outer(d, d))
  dimnames(Sigma_s) <- dimnames(S)
  Sigma_s
}

#' Max-Sharpe portfolio subject to per-asset bounds (long-only)
#'
#' Approximates the bounded efficient frontier by solving the bounded QP for a
#' grid of target returns and picking the highest-Sharpe feasible point. Used to
#' contain the macro tilt (concentration cap + deviation-from-prior budget) and
#' shared by the universe optimisers to avoid duplicating the frontier loop.
#'
#' @param mu        Expected returns (annualized), named
#' @param Sigma     Covariance (annualized), same order as mu
#' @param risk_free Annual risk-free rate
#' @param lb,ub     Per-asset lower/upper weight bounds (same order as mu)
#' @param n_points  Number of frontier points
#' @return List(weights, expected_return, volatility, sharpe) or NULL
bounded_max_sharpe <- function(mu, Sigma, risk_free, lb, ub, n_points = 100) {
  n    <- length(mu)
  Dmat <- 2 * Sigma
  dvec <- rep(0, n)

  w_mv <- tryCatch(
    quadprog::solve.QP(Dmat, dvec, cbind(rep(1, n), diag(n), -diag(n)),
                       c(1, lb, -ub), meq = 1)$solution,
    error = function(e) NULL
  )
  r_min <- if (!is.null(w_mv)) sum(w_mv * mu) else min(mu)
  r_max <- max(mu)
  if (!(r_max > r_min)) r_max <- r_min + 1e-4
  targets <- seq(r_min, r_max, length.out = n_points)

  rows <- lapply(targets, function(r) {
    Amat <- cbind(rep(1, n), mu, diag(n), -diag(n))
    bvec <- c(1, r, lb, -ub)
    sol  <- tryCatch(quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq = 2),
                     error = function(e) NULL)
    if (is.null(sol)) return(NULL)
    w <- pmax(sol$solution, 0)
    s <- sum(w); if (s <= 0) return(NULL)
    w <- w / s
    list(w = w, ret = sum(w * mu),
         vol = sqrt(as.numeric(t(w) %*% Sigma %*% w)))
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(NULL)

  sharpes <- sapply(rows, function(x) (x$ret - risk_free) / x$vol)
  best    <- rows[[which.max(sharpes)]]
  w       <- best$w; names(w) <- names(mu)
  list(weights = w, expected_return = best$ret, volatility = best$vol,
       sharpe = (best$ret - risk_free) / best$vol)
}

#' Per-asset bounds that keep the optimiser close to the strategic prior
#'
#' lb = max(0, prior - tilt_budget); ub = max(prior, min(prior + tilt_budget,
#' max_weight)). The ub floor at `prior` guarantees the prior is always feasible
#' (so the QP never becomes infeasible even when a concentrated prior exceeds the
#' cap, e.g. a 2-asset portfolio), while still capping the achievable tilt.
#'
#' @param prior       Named strategic-prior weights (sum to 1)
#' @param max_weight  Hard per-asset cap
#' @param tilt_budget Max absolute deviation from the prior
#' @return list(lb, ub) named vectors
prior_tilt_bounds <- function(prior, max_weight, tilt_budget) {
  lb <- pmax(0, prior - tilt_budget)
  ub <- pmax(prior, pmin(prior + tilt_budget, max_weight))
  list(lb = lb, ub = ub)
}

#' Compute equilibrium expected returns from market caps (reverse CAPM)
#'
#' @param Sigma       Annualized covariance matrix
#' @param market_caps Vector of market cap weights (sums to 1)
#' @param lambda      Market risk aversion (typical 2.5-3.5)
#' @return Vector of equilibrium expected returns (Pi)
equilibrium_returns <- function(Sigma, market_caps, lambda = 3.0) {
  as.numeric(lambda * Sigma %*% market_caps)
}


#' Black-Litterman posterior expected returns
#'
#' @param Sigma       Annualized covariance
#' @param market_caps Market cap weights (vector summing to 1)
#' @param P           View matrix (k x n) — each row is a view
#' @param Q           View expected returns (vector of length k)
#' @param confidence  Vector [0,1] for each view (1 = full confidence in view)
#' @param tau         Scaling for prior uncertainty (default 0.025)
#' @param lambda      Market risk aversion
#' @return List with prior, posterior, and Sigma_posterior
black_litterman <- function(Sigma, market_caps, P, Q, confidence,
                            tau = 0.025, lambda = 3.0) {

  asset_names <- colnames(Sigma)
  n           <- length(asset_names)

  Pi <- equilibrium_returns(Sigma, market_caps, lambda)
  names(Pi) <- asset_names

  # Confidence -> Omega: higher confidence = lower omega entry
  # Omega diag: tau * P Sigma P' element-wise * (1 - c) / c
  # Clamp confidence away from 0 and 1: c = 1 gives omega = 0 (singular Omega),
  # c = 0 gives omega = Inf; both break solve(Omega).
  confidence <- pmin(pmax(confidence, 1e-6), 1 - 1e-6)
  base    <- diag(P %*% (tau * Sigma) %*% t(P))
  omega_d <- base * (1 - confidence) / confidence
  Omega   <- diag(omega_d, nrow = length(omega_d))

  # Posterior mean (BL formula)
  tauSigma_inv <- solve(tau * Sigma)
  Omega_inv    <- solve(Omega)

  M_inv  <- tauSigma_inv + t(P) %*% Omega_inv %*% P
  M      <- solve(M_inv)
  mu_bl  <- as.numeric(M %*% (tauSigma_inv %*% Pi + t(P) %*% Omega_inv %*% Q))
  names(mu_bl) <- asset_names

  Sigma_post <- Sigma + M
  colnames(Sigma_post) <- rownames(Sigma_post) <- asset_names

  list(prior = Pi, posterior = mu_bl, Sigma_posterior = Sigma_post)
}


# -----------------------------------------------------------------------------
# 5. MACRO DATA AND RULE-BASED VIEWS (FRED via quantmod)
# -----------------------------------------------------------------------------

#' Fetch macro indicators from FRED via JSON API
#'
#' Uses the FRED REST API directly (readLines + jsonlite) to avoid package
#' network-library conflicts. Returns the most recent value of each indicator.
#' If a series is unavailable the corresponding element is NA.
#'
#' @param api_key  FRED API key (register free at fred.stlouisfed.org)
#' @return Named list of recent macro readings; $series is a data.frame(date, value)
fetch_macro_indicators <- function(api_key = "") {

  # Load .env so FRED_API_KEY is available in {targets} worker processes
  # (which do not source library.R and therefore don't inherit the key)
  local({
    env_file <- file.path(getwd(), ".env")
    if (file.exists(env_file)) {
      lines <- readLines(env_file, warn = FALSE)
      for (line in lines) {
        line <- trimws(line)
        if (nchar(line) == 0 || startsWith(line, "#")) next
        kv <- strsplit(line, "=", fixed = TRUE)[[1]]
        if (length(kv) >= 2)
          do.call(Sys.setenv, setNames(list(paste(kv[-1], collapse = "=")), kv[1]))
      }
    }
  })
  if (nchar(trimws(api_key)) == 0) api_key <- Sys.getenv("FRED_API_KEY")

  safe_fetch <- function(series_id, limit = 500) {
    # sort_order=desc + limit gives the most recent N obs; we reverse to ascending
    url <- paste0(
      "https://api.stlouisfed.org/fred/series/observations",
      "?series_id=", series_id,
      "&api_key=",   api_key,
      "&file_type=json",
      "&sort_order=desc",
      "&limit=",     limit
    )
    tryCatch({
      txt  <- paste(readLines(url, warn = FALSE), collapse = "")
      obs  <- jsonlite::fromJSON(txt)$observations[, c("date", "value")]
      obs$date  <- as.Date(obs$date)
      obs$value <- suppressWarnings(as.numeric(obs$value))
      obs       <- obs[!is.na(obs$value), ]
      obs       <- obs[order(obs$date), ]   # restore ascending order
      list(latest = tail(obs$value, 1), date = tail(obs$date, 1), series = obs)
    }, error = function(e) {
      warning(sprintf("FRED fetch failed for %s: %s", series_id, e$message))
      list(latest = NA_real_, date = NA, series = NULL)
    })
  }

  list(
    yield_curve_10y2y = safe_fetch("T10Y2Y",        limit = 60),   # 10Y-2Y spread (%)
    cpi_yoy           = safe_fetch("CPIAUCSL",       limit = 60),   # CPI level (YoY computed below)
    fed_funds         = safe_fetch("DFF",            limit = 30),   # Effective Fed Funds (%)
    hy_spread         = safe_fetch("BAMLH0A0HYM2",   limit = 30),   # HY OAS (%)
    breakeven_5y      = safe_fetch("T5YIE",          limit = 30),   # 5Y breakeven inflation (%)
    vix               = safe_fetch("VIXCLS",         limit = 30)    # VIX
  )
}


#' Compute year-over-year change for a level series (e.g. CPI)
#'
#' @param series data.frame(date, value) as returned by fetch_macro_indicators
#' @return YoY % change at most recent observation, or NA
compute_yoy <- function(series) {
  if (is.null(series) || nrow(series) < 13) return(NA_real_)
  if (is.data.frame(series)) {
    n <- nrow(series)
    100 * (series$value[n] / series$value[n - 12] - 1)
  } else {
    # xts fallback for legacy callers
    100 * (as.numeric(tail(series, 1)) / as.numeric(series[nrow(series) - 12]) - 1)
  }
}


#' Generate Black-Litterman views from macro indicators using rule-based logic
#'
#' Produces a P matrix, Q vector, and confidence vector based on two views:
#'   1. Equity vs Bonds (driven by yield curve)
#'   2. Gold absolute (driven by inflation)
#'
#' The former Cash absolute view (Q = Fed Funds Rate) was removed: an absolute
#' return view on a near-zero-volatility asset mechanically dominated the
#' downstream max-Sharpe step. Cash is now governed by the strategic prior + bounds.
#'
#' Asset class mapping: each view aggregates over all matching assets in the
#' universe with equal weights inside each side of the view.
#'
#' @param indicators  Output of fetch_macro_indicators()
#' @param asset_names Vector of portfolio asset names (must match config)
#' @param rules       List of thresholds and Q values from config
#' @param classes     Optional named vector asset -> class (from classify_assets).
#'                    If NULL, computed from DEFAULT_ASSET_CLASSES.
#' @return List with P (matrix), Q (vector), confidence (vector), narrative
generate_macro_views <- function(indicators, asset_names, rules, classes = NULL) {

  # Single source of truth for class assignment (shared with prior / backtest / report)
  if (is.null(classes)) classes <- classify_assets(asset_names)

  build_p_row <- function(long_class, short_class = NULL) {
    p <- rep(0, length(asset_names))
    long_idx <- which(classes == long_class)
    if (length(long_idx) > 0) p[long_idx] <-  1 / length(long_idx)
    if (!is.null(short_class)) {
      short_idx <- which(classes == short_class)
      if (length(short_idx) > 0) p[short_idx] <- -1 / length(short_idx)
    }
    p
  }

  views     <- list()
  narrative <- list()

  # --- View 1: Equity vs Bonds (yield curve) ----------------------------------
  yc <- indicators$yield_curve_10y2y$latest
  if (!is.na(yc) && any(classes == "equity") && any(classes == "bond")) {
    if (yc < rules$yield_curve$inversion_strong) {
      Q_val <- -rules$yield_curve$Q_recession
      conf  <-  rules$yield_curve$conf_recession
      regime <- "Strong inversion (recession signal)"
    } else if (yc < rules$yield_curve$flat) {
      Q_val <-  0
      conf  <-  0      # No view
      regime <- "Mild inversion / flat (neutral)"
    } else if (yc > rules$yield_curve$steep) {
      Q_val <-  rules$yield_curve$Q_expansion
      conf  <-  rules$yield_curve$conf_expansion
      regime <- "Steep curve (expansion signal)"
    } else {
      Q_val <- 0
      conf  <- 0
      regime <- "Normal curve (neutral)"
    }

    if (conf > 0) {
      views$equity_vs_bonds <- list(
        P = build_p_row("equity", "bond"),
        Q = Q_val,
        confidence = conf
      )
      narrative$equity_vs_bonds <- sprintf(
        "Yield curve 10Y-2Y = %.2f → %s. Equity - Bonds expected at %.1f%% (conf %.0f%%)",
        yc, regime, Q_val * 100, conf * 100
      )
    }
  }

  # --- View 2: Gold absolute (inflation) --------------------------------------
  cpi_series <- indicators$cpi_yoy$series
  cpi_yoy    <- compute_yoy(cpi_series)
  if (!is.na(cpi_yoy) && any(classes == "gold")) {
    if (cpi_yoy > rules$inflation$high) {
      Q_val <- rules$inflation$Q_high
      conf  <- rules$inflation$conf_high
      regime <- "High inflation"
    } else if (cpi_yoy > rules$inflation$moderate) {
      Q_val <- rules$inflation$Q_moderate
      conf  <- rules$inflation$conf_moderate
      regime <- "Moderate inflation"
    } else {
      Q_val <- 0
      conf  <- 0
      regime <- "Low inflation (neutral)"
    }

    if (conf > 0) {
      views$gold_absolute <- list(
        P = build_p_row("gold"),
        Q = Q_val,
        confidence = conf
      )
      narrative$gold_absolute <- sprintf(
        "CPI YoY = %.2f%% → %s. Gold expected at %.1f%% (conf %.0f%%)",
        cpi_yoy, regime, Q_val * 100, conf * 100
      )
    }
  }

  # View 3 (Cash absolute, Q = Fed Funds Rate) was intentionally removed — see the
  # function docstring and config.yaml. Fed Funds remains available in `indicators`
  # for reporting context but no longer drives an absolute BL view.

  if (length(views) == 0) {
    return(list(P = NULL, Q = NULL, confidence = NULL,
                narrative = list()))
  }

  P          <- do.call(rbind, lapply(views, `[[`, "P"))
  Q          <- sapply(views, `[[`, "Q")
  confidence <- sapply(views, `[[`, "confidence")
  rownames(P) <- names(views)
  colnames(P) <- asset_names

  list(P = P, Q = Q, confidence = confidence, narrative = narrative)
}


# -----------------------------------------------------------------------------
# 6. MACRO-TILTED OPTIMIZATION (Black-Litterman + Markowitz)
# -----------------------------------------------------------------------------

#' Optimize weights using Black-Litterman with rule-based macro views
#'
#' Pipeline: strategic-prior equilibrium returns -> BL posterior conditioned on
#' the active macro views -> bounded max-Sharpe (concentration cap + deviation-
#' from-prior budget). Also returns the equilibrium-only (no-views) bounded
#' weights so the macro tilt can be reported as a move FROM the strategic anchor.
#'
#' @param ptf         Portfolio object (build_portfolio output)
#' @param cfg         Config list
#' @param market_caps Strategic-prior weights (named, sum to 1). If NULL, built
#'                    from cfg$macro_prior via strategic_prior() — NOT the current
#'                    portfolio weights (which would bias toward the status quo).
#' @param indicators  Output of fetch_macro_indicators(). If NULL, fetched here
#'                    (the {targets} pipeline passes a shared, dated snapshot).
#' @return List with tilted weights, equilibrium weights, prior, returns, views
optimize_macro <- function(ptf, cfg, market_caps = NULL, indicators = NULL) {

  asset_names <- ptf$assets
  rf          <- cfg$global$risk_free
  classes     <- classify_assets(asset_names, cfg$asset_classes)

  # Annualized, shrinkage-regularised covariance (stable to invert in BL)
  Sigma <- shrink_cov(ptf$var_cov, cfg$macro_rules$cov_shrinkage %||% 0) * 12
  dimnames(Sigma) <- list(asset_names, asset_names)

  # Strategic-asset-allocation prior (the equilibrium anchor)
  if (is.null(market_caps)) {
    market_caps <- strategic_prior(classes, cfg$macro_prior)
  }
  market_caps <- market_caps / sum(market_caps)

  # 1) Macro indicators (fetched here only if not supplied by the pipeline)
  if (is.null(indicators)) {
    message("Fetching macro indicators from FRED...")
    indicators <- fetch_macro_indicators({
      k <- cfg$fred_api_key %||% ""
      if (nchar(trimws(k)) == 0) Sys.getenv("FRED_API_KEY") else k
    })
  }

  # 2) Generate views from rules (shared class map)
  views <- generate_macro_views(indicators, asset_names, cfg$macro_rules, classes)

  # 3) Black-Litterman posterior (fall back to equilibrium-only on no-views or
  #    a singular-covariance solve failure)
  equilibrium_only <- function() {
    mu <- equilibrium_returns(Sigma, market_caps, lambda = cfg$macro_rules$lambda)
    names(mu) <- asset_names
    list(prior = mu, posterior = mu, Sigma_posterior = Sigma)
  }
  if (is.null(views$P)) {
    message("No active macro views — using equilibrium (strategic-prior) returns only.")
    bl <- equilibrium_only()
  } else {
    bl <- tryCatch(
      black_litterman(
        Sigma = Sigma, market_caps = market_caps,
        P = views$P, Q = views$Q, confidence = views$confidence,
        tau = cfg$macro_rules$tau, lambda = cfg$macro_rules$lambda
      ),
      error = function(e) {
        warning(sprintf("Black-Litterman failed (%s) — falling back to equilibrium.",
                        conditionMessage(e)))
        equilibrium_only()
      }
    )
  }

  # 4) Bounded max-Sharpe on posterior; same bounds on the prior for attribution.
  #    Bounds keep weights within tilt_budget of the strategic prior and under
  #    max_weight, so the result can never become an extreme corner solution.
  b  <- prior_tilt_bounds(market_caps,
                          cfg$macro_rules$max_weight  %||% 1,
                          cfg$macro_rules$tilt_budget %||% 1)
  fallback_metrics <- function(w) list(
    weights = w, expected_return = sum(w * bl$posterior),
    volatility = sqrt(as.numeric(t(w) %*% Sigma %*% w)),
    sharpe = (sum(w * bl$posterior) - rf) /
             sqrt(as.numeric(t(w) %*% Sigma %*% w))
  )
  msr        <- bounded_max_sharpe(bl$posterior, Sigma, rf, b$lb, b$ub) %||%
                fallback_metrics(market_caps)
  msr_equil  <- bounded_max_sharpe(bl$prior,     Sigma, rf, b$lb, b$ub) %||%
                list(weights = market_caps)
  w_tilt     <- msr$weights[asset_names]
  w_equil    <- msr_equil$weights[asset_names]

  list(
    weights            = w_tilt,
    weights_equilibrium = w_equil,
    strategic_prior    = market_caps,
    current_weights    = ptf$quotes,
    classes            = classes,
    expected_return    = msr$expected_return,
    volatility         = msr$volatility,
    sharpe             = msr$sharpe,
    max_weight_used    = max(w_tilt),
    max_abs_tilt       = max(abs(w_tilt - market_caps)),
    mu_prior           = bl$prior,
    mu_posterior       = bl$posterior,
    views              = views,
    indicators         = indicators
  )
}


# =============================================================================
# 7. UNIVERSE EXPANSION OPTIMIZATION
# =============================================================================

#' Constrained Markowitz optimization over the full asset universe
#'
#' Extends the portfolio's current allocation by considering ALL assets available
#' in the universe returns matrix. Per-asset weight bounds enforce:
#'   - Existing assets: original weight ± current_slack (anchored around current allocation)
#'   - New assets: 0 to max_new_weight
#'
#' The optimizer solves global min-variance subject to these bounds, then also
#' finds the max-Sharpe portfolio on the expanded universe.
#'
#' @param ptf                Portfolio object (build_portfolio output)
#' @param returns_universe   data.frame with all available assets + Dates (NAs allowed)
#' @param cfg                Config list
#' @return List with min-variance and max-Sharpe solutions on the full universe
# Trim a weight vector to at most max_assets non-trivial positions by re-solving
# the min-variance QP with the lowest-weight assets forced to zero.
trim_to_max_assets <- function(w, lb, ub, Sigma, max_assets, min_report) {
  if (is.null(max_assets) || sum(w > min_report) <= max_assets) return(w)
  keep  <- names(sort(w, decreasing = TRUE)[seq_len(max_assets)])
  ub2   <- ub; ub2[setdiff(names(ub), keep)] <- 0
  n     <- length(w)
  Amat2 <- cbind(rep(1, n), diag(n), -diag(n))
  bvec2 <- c(1, lb, -ub2)
  sol   <- tryCatch(
    quadprog::solve.QP(2 * Sigma, rep(0, n), Amat2, bvec2, meq = 1),
    error = function(e) NULL
  )
  if (is.null(sol)) return(w)
  w2 <- sol$solution; w2[w2 < 1e-6] <- 0; w2 / sum(w2)
}


optimize_universe <- function(ptf, returns_universe, cfg,
                              exclude_assets = character(0)) {

  univ_cfg    <- cfg$universe_optimization
  max_new     <- univ_cfg$max_new_weight
  slack       <- univ_cfg$current_slack
  min_report  <- univ_cfg$min_weight_report
  max_assets  <- univ_cfg$max_assets
  min_months  <- 12 * (cfg$sourcing$min_history_years %||% 5)

  # Identify available asset columns (exclude Dates)
  all_assets     <- setdiff(names(returns_universe), "Dates")
  current_assets <- ptf$assets
  new_assets     <- setdiff(all_assets, c(current_assets, exclude_assets))
  universe       <- c(current_assets, new_assets)

  # Filter to assets with enough non-NA monthly observations
  ret_univ_raw <- returns_universe %>%
    select(Dates, all_of(universe)) %>%
    arrange(Dates)

  enough_data <- sapply(universe, function(a) {
    sum(!is.na(ret_univ_raw[[a]])) >= min_months
  })
  universe <- universe[enough_data]
  if (length(setdiff(new_assets, universe)) > 0) {
    dropped <- setdiff(new_assets, universe)
    message(sprintf("Dropped from universe (insufficient history): %s",
                    paste(dropped, collapse = ", ")))
  }
  new_assets <- intersect(new_assets, universe)

  # Build returns matrix using only rows where ALL universe assets have data
  # (use pairwise approach: take the longest overlap for which all series exist)
  ret_mat_full <- ret_univ_raw %>%
    select(Dates, all_of(universe)) %>%
    na.omit() %>%
    select(-Dates) %>%
    as.matrix()

  if (nrow(ret_mat_full) < 24) {
    warning("Universe optimization: fewer than 24 months of common history — skipping.")
    return(NULL)
  }

  n      <- length(universe)
  mu     <- (1 + colMeans(ret_mat_full))^12 - 1
  Sigma  <- cov(ret_mat_full) * 12
  names(mu) <- universe

  # Per-asset lower and upper bounds
  orig_w <- ptf$quotes
  names(orig_w) <- current_assets

  lb <- rep(0,       n); names(lb) <- universe
  ub <- rep(max_new, n); names(ub) <- universe

  for (a in intersect(current_assets, universe)) {
    lb[a] <- max(0,   orig_w[a] - slack)
    ub[a] <- min(1,   orig_w[a] + slack)
  }

  # Constrained QP:
  # min  w'Sigma w
  # s.t. sum(w) = 1   [equality]
  #      w >= lb       [lower bounds]
  #      w <= ub       [upper bounds → -w >= -ub]
  Dmat <- 2 * Sigma
  dvec <- rep(0, n)
  Amat <- cbind(rep(1, n), diag(n), -diag(n))
  bvec <- c(1, lb, -ub)
  meq  <- 1

  sol_minvar <- tryCatch(
    quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq = meq),
    error = function(e) {
      warning(sprintf("Universe min-variance QP failed: %s", e$message))
      NULL
    }
  )

  w_minvar <- if (!is.null(sol_minvar)) {
    w <- sol_minvar$solution
    w[w < 1e-6] <- 0
    w <- w / sum(w)
    trim_to_max_assets(w, lb, ub, Sigma, max_assets, min_report)
  } else NULL
  names(w_minvar) <- universe

  # Max-Sharpe on expanded universe (unconstrained except long-only, reuses bounds via frontier)
  msr <- tryCatch(
    markowitz_max_sharpe(mu, Sigma, risk_free = cfg$global$risk_free, long_only = TRUE),
    error = function(e) NULL
  )

  # Also solve max-Sharpe with same bounds (by running over the bounded frontier)
  bounded_frontier <- tryCatch({
    # Use the bounded QP for several target returns to approximate bounded frontier
    r_min <- if (!is.null(w_minvar)) sum(w_minvar * mu) else min(mu)
    r_max <- max(mu[current_assets])   # cap at max of existing assets
    targets <- seq(r_min, r_max, length.out = 100)
    rows <- lapply(targets, function(r) {
      Amat2 <- cbind(rep(1, n), mu, diag(n), -diag(n))
      bvec2 <- c(1, r, lb, -ub)
      sol <- tryCatch(quadprog::solve.QP(Dmat, dvec, Amat2, bvec2, meq = 2),
                      error = function(e) NULL)
      if (is.null(sol)) return(NULL)
      w2 <- pmax(sol$solution, 0); w2 <- w2 / sum(w2)
      list(Return = sum(w2 * mu),
           Risk   = sqrt(as.numeric(t(w2) %*% Sigma %*% w2)),
           w      = w2)
    })
    Filter(Negate(is.null), rows)
  }, error = function(e) list())

  w_maxsharpe_bounded <- if (length(bounded_frontier) > 0) {
    sharpes <- sapply(bounded_frontier, function(r) (r$Return - cfg$global$risk_free) / r$Risk)
    best    <- bounded_frontier[[which.max(sharpes)]]
    w2 <- best$w; w2[w2 < 1e-6] <- 0; w2 <- w2 / sum(w2)
    w2 <- trim_to_max_assets(w2, lb, ub, Sigma, max_assets, min_report)
    names(w2) <- universe
    w2
  } else w_minvar

  # Summarise: which new assets are suggested?
  summarise_suggestion <- function(w, label) {
    if (is.null(w)) return(NULL)
    w_sig <- w[w >= min_report]
    added <- intersect(new_assets, names(w_sig))
    rebalanced <- sapply(intersect(current_assets, names(w_sig)), function(a) {
      delta <- w_sig[a] - (if (a %in% names(orig_w)) orig_w[a] else 0)
      delta
    })
    list(
      label            = label,
      weights          = w,
      new_assets_added = added,
      new_weights      = w[added],
      delta_current    = rebalanced,
      expected_return  = sum(w * mu),
      volatility       = sqrt(as.numeric(t(w) %*% Sigma %*% w)),
      sharpe           = (sum(w * mu) - cfg$global$risk_free) /
                          sqrt(as.numeric(t(w) %*% Sigma %*% w))
    )
  }

  list(
    min_variance = summarise_suggestion(w_minvar,            "Min-Variance (bounded)"),
    max_sharpe   = summarise_suggestion(w_maxsharpe_bounded, "Max-Sharpe (bounded)"),
    universe     = universe,
    new_assets   = new_assets,
    mu           = mu,
    Sigma        = Sigma,
    n_months     = nrow(ret_mat_full)
  )
}


#' Macro-aware universe optimization (Black-Litterman on full universe + bounded Markowitz)
#'
#' Extends optimize_macro() to the full asset universe: BL posterior is computed
#' for ALL available assets (not just portfolio assets), so macro-favored assets
#' outside the current portfolio can be selected by the optimizer.
#'
#' @param ptf              Portfolio object (build_portfolio output)
#' @param returns_universe data.frame with all assets + Dates
#' @param cfg              Config list
#' @param exclude_assets   Asset names to keep out of the universe
#' @param indicators       Output of fetch_macro_indicators(); fetched if NULL
#' @return List with BL-tilted bounded max-Sharpe suggestion over full universe
optimize_macro_universe <- function(ptf, returns_universe, cfg,
                                    exclude_assets = character(0),
                                    indicators = NULL) {

  univ_cfg   <- cfg$universe_optimization
  max_new    <- univ_cfg$max_new_weight
  slack      <- univ_cfg$current_slack
  min_report <- univ_cfg$min_weight_report
  max_assets <- univ_cfg$max_assets
  min_months <- 12 * (cfg$sourcing$min_history_years %||% 5)

  all_assets     <- setdiff(names(returns_universe), "Dates")
  current_assets <- ptf$assets
  new_assets     <- setdiff(all_assets, c(current_assets, exclude_assets))
  universe       <- c(current_assets, new_assets)

  # Filter to assets with sufficient history
  ret_raw <- returns_universe %>% select(Dates, all_of(universe)) %>% arrange(Dates)
  enough  <- sapply(universe, function(a) sum(!is.na(ret_raw[[a]])) >= min_months)
  universe   <- universe[enough]
  new_assets <- intersect(new_assets, universe)

  ret_mat <- ret_raw %>%
    select(Dates, all_of(universe)) %>%
    na.omit() %>%
    select(-Dates) %>%
    as.matrix()

  if (nrow(ret_mat) < 24) return(NULL)

  n       <- length(universe)
  classes <- classify_assets(universe, cfg$asset_classes)
  mu_raw  <- (1 + colMeans(ret_mat))^12 - 1
  Sigma   <- shrink_cov(cov(ret_mat), cfg$macro_rules$cov_shrinkage %||% 0) * 12
  dimnames(Sigma) <- list(universe, universe)
  names(mu_raw)   <- universe

  # Strategic-prior market-cap proxy: held assets follow the SAA (cfg$macro_prior)
  # by class; brand-new assets share a small pool (new_asset_mktcap_scale) so the
  # anchor is the broad strategic allocation, not the current portfolio.
  orig_w     <- setNames(ptf$quotes, current_assets)
  held       <- intersect(current_assets, universe)
  new_in     <- intersect(new_assets, universe)
  new_scale  <- if (length(new_in) > 0) (univ_cfg$new_asset_mktcap_scale %||% 0.1) else 0
  saa_held   <- strategic_prior(classes[held], cfg$macro_prior)
  market_caps <- setNames(rep(0, n), universe)
  market_caps[held] <- saa_held[held] * (1 - new_scale)
  if (length(new_in) > 0) market_caps[new_in] <- new_scale / length(new_in)
  market_caps <- market_caps / sum(market_caps)

  # Macro indicators (fetched here only if not supplied by the pipeline)
  if (is.null(indicators)) {
    message("Fetching macro indicators from FRED for universe optimization...")
    indicators <- fetch_macro_indicators({
      k <- cfg$fred_api_key %||% ""
      if (nchar(trimws(k)) == 0) Sys.getenv("FRED_API_KEY") else k
    })
  }

  # Generate views on the FULL universe (shared class map)
  views <- generate_macro_views(indicators, universe, cfg$macro_rules, classes)

  # Black-Litterman posterior on full universe (fall back to equilibrium-only on
  # no-views or a singular-covariance solve failure)
  equilibrium_only <- function() {
    mu <- equilibrium_returns(Sigma, market_caps, lambda = cfg$macro_rules$lambda)
    names(mu) <- universe
    list(prior = mu, posterior = mu, Sigma_posterior = Sigma)
  }
  if (is.null(views$P)) {
    bl <- equilibrium_only()
  } else {
    bl <- tryCatch(
      black_litterman(
        Sigma = Sigma, market_caps = market_caps,
        P = views$P, Q = views$Q, confidence = views$confidence,
        tau = cfg$macro_rules$tau, lambda = cfg$macro_rules$lambda
      ),
      error = function(e) {
        warning(sprintf("Universe Black-Litterman failed (%s) — equilibrium fallback.",
                        conditionMessage(e)))
        equilibrium_only()
      }
    )
  }
  mu_post <- bl$posterior

  # Per-asset bounds (same as optimize_universe)
  lb <- rep(0,       n); names(lb) <- universe
  ub <- rep(max_new, n); names(ub) <- universe
  for (a in intersect(current_assets, universe)) {
    lb[a] <- max(0, orig_w[a] - slack)
    ub[a] <- min(1, orig_w[a] + slack)
  }

  # Bounded max-Sharpe on BL posterior returns
  Dmat <- 2 * Sigma
  dvec <- rep(0, n)
  r_min <- sum(markowitz_min_variance(Sigma) * mu_post)
  r_max <- max(mu_post[current_assets])
  targets <- seq(r_min, r_max, length.out = 100)

  rows <- lapply(targets, function(r) {
    Amat2 <- cbind(rep(1, n), mu_post, diag(n), -diag(n))
    bvec2 <- c(1, r, lb, -ub)
    sol <- tryCatch(quadprog::solve.QP(Dmat, dvec, Amat2, bvec2, meq = 2),
                    error = function(e) NULL)
    if (is.null(sol)) return(NULL)
    w2 <- pmax(sol$solution, 0); w2 <- w2 / sum(w2)
    list(Return = sum(w2 * mu_post),
         Risk   = sqrt(as.numeric(t(w2) %*% Sigma %*% w2)),
         w      = w2)
  })
  rows <- Filter(Negate(is.null), rows)

  if (length(rows) == 0) return(NULL)

  sharpes <- sapply(rows, function(r) (r$Return - cfg$global$risk_free) / r$Risk)
  best    <- rows[[which.max(sharpes)]]
  w_opt   <- best$w; w_opt[w_opt < 1e-6] <- 0; w_opt <- w_opt / sum(w_opt)
  w_opt   <- trim_to_max_assets(w_opt, lb, ub, Sigma, max_assets, min_report)
  names(w_opt) <- universe

  # Identify suggested new assets
  added <- intersect(new_assets, names(w_opt[w_opt >= min_report]))

  list(
    weights          = w_opt,
    new_assets_added = added,
    new_weights      = w_opt[added],
    expected_return  = sum(w_opt * mu_post),
    volatility       = sqrt(as.numeric(t(w_opt) %*% Sigma %*% w_opt)),
    sharpe           = (sum(w_opt * mu_post) - cfg$global$risk_free) /
                       sqrt(as.numeric(t(w_opt) %*% Sigma %*% w_opt)),
    mu_prior         = bl$prior,
    mu_posterior     = bl$posterior,
    views            = views,
    indicators       = indicators,
    universe         = universe,
    new_assets       = new_assets,
    n_months         = nrow(ret_mat)
  )
}


#' Save universe optimization results to parquet
#'
#' @param ptf_name    Portfolio name
#' @param univ_opt    Output of optimize_universe() or optimize_macro_universe()
#' @param suffix      File name suffix ("universe" or "macro_universe")
#' @param output_path Path to output folder
#' @return Vector of saved file paths
save_universe_opt_output <- function(ptf_name, univ_opt, suffix = "universe", output_path) {
  if (is.null(univ_opt)) return(character(0))

  # For optimize_universe() output (has min_variance / max_sharpe sub-lists)
  # For optimize_macro_universe() output (flat structure)
  extract_weights <- function(x) {
    if (!is.null(x$weights)) {
      data.frame(Asset = names(x$weights), Weight = as.numeric(x$weights))
    } else if (!is.null(x$min_variance$weights)) {
      mv <- x$min_variance; ms <- x$max_sharpe
      rbind(
        data.frame(Asset = names(mv$weights), Weight = as.numeric(mv$weights), Method = "Min_Variance"),
        data.frame(Asset = names(ms$weights), Weight = as.numeric(ms$weights), Method = "Max_Sharpe")
      )
    } else data.frame(Asset = character(0), Weight = numeric(0))
  }

  extract_summary <- function(x) {
    get_row <- function(obj, method = "Main") {
      if (is.null(obj)) return(NULL)
      data.frame(
        Method          = method,
        Expected_Return = obj$expected_return,
        Volatility      = obj$volatility,
        Sharpe          = obj$sharpe,
        New_Assets      = paste(obj$new_assets_added, collapse = "|")
      )
    }
    if (!is.null(x$weights)) {
      get_row(x, "Main")
    } else {
      do.call(rbind, list(get_row(x$min_variance, "Min_Variance"),
                          get_row(x$max_sharpe,   "Max_Sharpe")))
    }
  }

  paths <- c(
    weights = paste0(output_path, ptf_name, "_", suffix, "_weights.parquet"),
    summary = paste0(output_path, ptf_name, "_", suffix, "_summary.parquet")
  )

  arrow::write_parquet(extract_weights(univ_opt), paths["weights"])
  arrow::write_parquet(extract_summary(univ_opt), paths["summary"])
  unname(paths)
}
