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


risk_portfolio <- function(quotes, var_cov, avg_returns, ticker_df){
  
  var_ptf <- t(quotes) %*% var_cov %*% quotes %>% as.numeric()
  sd_ptf <- sqrt(var_ptf) %>% as.numeric()
  
  ptf_tbl <- tibble(Quotes = quotes) %>% 
    mutate(
      VarCov_weighted = var_cov %*% quotes %>% as.vector(),
      MRC = VarCov_weighted / as.numeric(sd_ptf),
      TRC = quotes * MRC,
      Ret_Avg = t(avg_returns) %>% as.vector(),
      Ret_Weighted = quotes * Ret_Avg,
      'sd/n' = sd_ptf / length(quotes),
      Assets = ticker_df$Index
    ) %>% 
    relocate(Assets, .before = everything())
  
  ptf_tot <- tibble(
    Tot_Quotes = sum(quotes),
    Var_ptf = var_ptf,
    Std_Dev = sd_ptf,
    Monthly_Ret = sum(ptf_tbl$Ret_Weighted),
    Annual_Ret = (Monthly_Ret+1)^12 - 1
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
