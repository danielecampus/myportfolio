get_prices <- function(ticker){
  prices <- getSymbols(ticker
                       , src = 'yahoo'
                       , auto.assign = FALSE)
  prices <- na.omit(prices)
  adj_prices <- Ad(prices)
  return(adj_prices)
}

organize_data <- function(tsPrices){

  min_length <- min(map_int(tsPrices, nrow))
  uniformed_ts <- map(tsPrices, ~tail(.x, min_length))
  
  price_matrix <- do.call(cbind, uniformed_ts) %>% na.omit()
  colnames(price_matrix) <- names(tsPrices)
  
  return(price_matrix)
}

final_prices <- function(price_matrix){
  sheet_prices <- price_matrix %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column(var = "Dates_chr") %>% 
    mutate(Dates = as.Date(Dates_chr),
           year_group = year(Dates),
           month_group = month(Dates)) %>% 
    group_by(year_group, month_group) %>% 
    slice_tail() %>% 
    ungroup() %>% 
    select(-year_group, -month_group, -Dates_chr) %>% 
    relocate(Dates, .before = everything()) %>% 
    arrange(Dates) 
  
  return(sheet_prices)
}

get_returns <- function(sheet_prices, tsPrices){
  
  tsReturns1 <- sheet_prices[, -1]/lag(sheet_prices[, -1])-1
  colnames(tsReturns1) <- names(tsPrices)
  
  sheet_returns <- tsReturns1 %>% 
    mutate(Dates = sheet_prices$Dates) %>% 
    relocate(Dates, .before = everything()) %>% 
    arrange(desc(Dates)) %>% 
    na.omit()
  
  return(sheet_returns)
}


risk_portfolio <- function(quotes, var_cov, avg_returns, ticker_df, returns){
  
  var_ptf <- t(quotes) %*% var_cov %*% quotes %>% as.numeric()
  sd_ptf <- sqrt(var_ptf) %>% as.numeric()
  
  ptf_tbl <- tibble(Quotes = quotes) %>% 
    mutate(
      VarCov_weighted = var_cov %*% quotes %>% as.vector(),
      Marginal_RC = VarCov_weighted / as.numeric(sd_ptf), 
      Total_RC = quotes * Marginal_RC, # actual risk contribution
      Expected_RC = sd_ptf / length(quotes), # expected risk contribution
      Squared_Errors = (Total_RC-Expected_RC)^2, # Squared error
      Ret_Avg = t(avg_returns) %>% as.vector(),
      Ret_Weighted = quotes * Ret_Avg,
      Assets = ticker_df
    ) %>% 
    relocate(Assets, .before = everything())
  
  VaR <- VaR(returns, p = 0.95,  method = "historical", portfolio_method = "component", weights = quotes)
  ES <-  ES(returns, p = 0.95, method = "historical", portfolio_method = "component", weights = quotes)
  
  ptf_tot <- tibble(
    Tot_Quotes = sum(quotes),
    Var_ptf = var_ptf,
    Std_Dev = sd_ptf,
    SSE = sum(ptf_tbl$Squared_Errors), # Sum of Squared Errors
    Monthly_Ret = sum(ptf_tbl$Ret_Weighted),
    Annual_Ret = (Monthly_Ret+1)^12 - 1,
    VaR = VaR$hVaR,
    ES = ES$`-r_exceed/c_exceed`
  )
  
  ptf_output <- list(Ptf_Analysis = ptf_tbl, Ptf_Summary = ptf_tot)
  
  return(ptf_output)
}

plot_quotes <- function(assets, quotes){
  df <- data.frame(assets, quotes)
  df$assets = factor(df$assets, levels = df$assets)
  
  plot_q <- ggplot(df, aes(x = "", y = quotes, fill = assets)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    geom_text(aes(label = scales::percent(quotes, accuracy = 0.1)),
              position = position_stack(vjust = 0.5), 
              size = 4) +
    labs(title = "Portfolio Asset Allocation") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5))
  return(plot_q)
}

plot_returns <- function(assets, ret_avg, ret_weighted){
  df <- data.frame(assets, ret_avg, ret_weighted) %>% 
    pivot_longer(cols = c(ret_avg,ret_weighted),
                 names_to = "Type",
                 values_to = "Value")
  
  plot_r <- ggplot(df, aes(x = reorder(assets, -Value), y = Value, fill = Type)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = scales::percent(Value, accuracy = 0.01)),
              position = position_dodge(width = 0.9),
              hjust = -0.1,
              size = 3.5) +
    labs(title = "Avg returns vs Weighted returns",
         x = "Assets",
         y = "Return",
         fill = "Type") +
    coord_flip() +
    theme_minimal()
  
  return(plot_r)
}


portfolio_optimization <- function(var_cov, avg_returns, target_return, upper_bounds){
  n_assets <- as.numeric(length(avg_returns))  # Numero di asset
# Funzione obiettivo: Minimizzare l'SSE tra le TRC
  objective_function <- function(w) {
    var_ptf <- t(w) %*% var_cov %*% w
    sd_ptf <- sqrt(var_ptf)
    TRC <- as.vector(w * (var_cov %*% w)) / sd_ptf
    TRC_mean <- mean(TRC)
    TRC_sum <- sum(TRC)
    SSE <- sum((TRC - TRC_mean)^2)
    return(SSE)
  }
  
  # Vincoli: Rendimento minimo richiesto
  eval_g_ineq <- function(w) {
    ret <- sum(w * avg_returns)
    return(target_return - ret)  # target_return <= rendimento del portafoglio
  }
  
  # Vincolo di uguaglianza: Somma delle quote = 1
  eval_g_eq <- function(w) {
    return(sum(w) - 1)
  }
  
  # Parametri per ottimizzazione
  lower_bounds <- rep(0.01, n_assets)  # Limite inferiore per ogni asset
  
  # Pesi iniziali uniformi normalizzati
  w0 <- rep(1 / n_assets, n_assets)
  w0 <- pmin(pmax(w0, lower_bounds), upper_bounds)  # Rispetto limiti
  w0 <- w0 / sum(w0)  # Normalizzazione
  
  # Ottimizzazione con nloptr
  result <- nloptr(
    x0 = w0,
    eval_f = objective_function,
    lb = lower_bounds,
    ub = upper_bounds,  # Limiti definiti
    eval_g_ineq = eval_g_ineq,  # Vincoli di rendimento
    eval_g_eq = eval_g_eq,  # Somma quote = 1
    opts = list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel" = 1e-6)
  )
  return(result)
}

named_list <- function(nome_lista, t, assets, quotes, input_path) {
  
  # Creazione della lista con i dati forniti
  nuova_lista <- list(
      horizon = t,
      time_decay = t/35, # horizon / ideal horizon (30 or 35 years)
      assets = assets,
      quotes = quotes
  )
  
  
  nuova_lista$tickers <- read_parquet(paste0(input_path, "data_ticker_df.parquet")) %>% 
    filter(Index %in% nuova_lista$assets)
  nuova_lista$returns <- read_parquet(paste0(input_path, "data_returns.parquet")) %>% 
    select(Dates, all_of(nuova_lista$assets))
  nuova_lista$prices <- read_parquet(paste0(input_path, "data_prices.parquet")) %>% 
    select(Dates, all_of(nuova_lista$assets))
  
  nuova_lista$ret_pure <- nuova_lista$returns %>% select(-Dates) %>% as.data.frame()
  nuova_lista$var_cov <- cov(nuova_lista$ret_pure)
  nuova_lista$corr_matrix <- cor(nuova_lista$ret_pure)
  nuova_lista$avg_returns <- nuova_lista$ret_pure %>% summarise(across(everything(), mean)) 
  
  nuova_lista$VaR <- VaR(nuova_lista$returns, p = 0.95,  method = "historical", portfolio_method = "component", weights = nuova_lista$quotes) 
  nuova_lista$ES <- ES(nuova_lista$returns, p = 0.95, method = "historical", portfolio_method = "component", weights = nuova_lista$quotes)
  nuova_lista$ptf_output <- risk_portfolio(nuova_lista$quotes, nuova_lista$var_cov, nuova_lista$avg_returns, nuova_lista$assets, nuova_lista$returns)
  # Assegna la lista a una variabile con il nome fornito
  assign(nome_lista, nuova_lista, envir = .GlobalEnv)
  
  return(nuova_lista)
}


############################### 
# montecarlo simulation

montecarlo_simulation <- function(assets, quotes, n_sim, n_months, initial_value){
  
  ticker_df <- read_parquet(paste0(input_path, "data_ticker_df.parquet")) %>% 
    filter(Index %in% assets)
  sheet_prices <- read_parquet(paste0(input_path, "data_prices.parquet")) %>% 
    select(Dates, all_of(assets))
  sheet_returns <- read_parquet(paste0(input_path, "data_returns.parquet")) %>% 
    select(Dates, all_of(assets))
  
  # analysis
  ret_pure <- sheet_returns %>% select(-Dates) %>% as.data.frame()
  var_cov <- cov(ret_pure)
  corr_matrix <- cor(ret_pure)
  avg_returns <- ret_pure %>% summarise(across(everything(), mean)) 
  
  avg_returns_vector <- avg_returns %>% as.numeric() %>% as.vector()
  
  # simulation of returns
  set.seed(123)
  simulated_returns <- MASS::mvrnorm(
    n = n_sim * n_period,
    mu = avg_returns_vector,
    Sigma = var_cov
  )
  
  simulated_returns <- array(simulated_returns, dim =  c(n_sim, n_months, length(avg_returns_vector)))
  portfolio_returns <-apply(simulated_returns, c(1,2), function(x) sum(x*quotes))
  
  portfolio_values <- matrix(NA, nrow = n_sim, ncol = n_period)
  portfolio_values[, 1] <- initial_value * (1 + portfolio_returns[, 1])
  
  for (t in 2:n_period) {
    portfolio_values[, t] <- portfolio_values[, t - 1] * (1 + portfolio_returns[, t])
  }
  
  final_values <- portfolio_values[, n_period]
  mean_value <- mean(final_values)
  expected_return <- mean_value / initial_value - 1
  sd_value <- sd(final_values)
  quantiles <- quantile(final_values, probs = c(0.05, 0.95))
  
  final_returns <- portfolio_returns[, n_period]
  VaR_percent <- quantile(final_returns, probs = 0.05)
  ES_percent <- mean(final_returns[final_returns <= VaR_percent])
  
  simulated_results <- data.frame(
    Horizon_Months = n_months,
    Expected_Return = expected_return,
    Std_Dev = sd_value/mean_value,
    VaR =  VaR_percent,
    ES = ES_percent,
    Initial_Value = initial_value,
    Expected_Value = mean_value,
    Q_5 = quantiles[1] %>% as.numeric(),
    Q_95 = quantiles[2] %>% as.numeric()
  ) 
  rownames(simulated_results) <- 'Forecast'
  
  # FORECAST OF RETURNS TIMESERIES
  ret_pure <- sheet_returns %>% arrange(Dates) %>% select(-Dates) %>% as.matrix()
  historical_returns <- ret_pure %*% quotes
  
  historical_data <- data.frame(Dates = sheet_returns$Dates) %>% 
    arrange(Dates) %>% 
    mutate(
      Returns = historical_returns %>% as.vector(),
      Cumulative_Value = 100 * cumprod(1 + historical_returns)
    )
  
  forecast_returns <- portfolio_returns[, 1:n_period]
  forecast_cumulative <- matrix(NA, nrow = n_sim, ncol = n_period)
  forecast_cumulative[, 1] <- tail(historical_data$Cumulative_Value, 1) * (1 + portfolio_returns[, 1])
  
  for (t in 2:n_period) {
    forecast_cumulative[, t] <- forecast_cumulative[, t - 1] * (1 + portfolio_returns[, t])
  }
  
  forecast_summary <- data.frame(
    Dates = seq.Date(from = max(historical_data$Dates) + 1, by = "month", length.out = n_period),
    Mean = apply(forecast_cumulative, 2, mean),
    P5 = apply(forecast_cumulative, 2, quantile, probs = 0.05),
    P95 = apply(forecast_cumulative, 2, quantile, probs = 0.95)
  )
  
  combined_data <- rbind(
    data.frame(Dates = historical_data$Dates, 
               Mean = historical_data$Cumulative_Value, 
               P5 = NA, 
               P95 = NA),
    forecast_summary
  ) %>% 
    arrange(desc(Dates))
  
  simulated_data <- list(
    forecast_summary = simulated_results,
    forecast_ts = combined_data
  )
  
  return(simulated_data)
}

plot_simulation <- function(forecast_ts, forecast_summary, name){
  plot_sim <- ggplot(forecast_ts, aes(x = forecast_ts$Dates)) +
    geom_line(aes(y = Mean), color = "blue", size = 1) +
    geom_ribbon(data = forecast_ts, aes(ymin = forecast_ts$P5, ymax = forecast_ts$P95), fill = "blue", alpha = 0.2) +
    labs(
      title = paste0("Forecast of the multiasset portfolio returns", " (", name,")"),
      subtitle = paste("t:", n_months, 
                       "months | E[R] =", round(forecast_summary$Expected_Return*100, 2), 
                       "% | VaR:", round(forecast_summary$VaR*100, 2), 
                       "% | ES:", round(forecast_summary$ES*100, 2), "%"),
      x = "Dates",
      y = "Portfolio Returns %") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 12)
    )
  return(plot_sim)
}
