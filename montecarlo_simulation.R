# Parametri
n_sim <- 10000
n_period <- 24
initial_value <- 50000
avg_returns_vector <- avg_returns %>% as.numeric() %>% as.vector()

# Simulazione dei rendimenti
set.seed(123)
simulated_returns <- MASS::mvrnorm(
  n = n_sim * n_period,
  mu = avg_returns_vector,
  Sigma = var_cov
)

# Trasformare i rendimenti simulati in una matrice
simulated_returns <- array(simulated_returns, dim =  c(n_sim, n_period, length(avg_returns_vector)))

# Calcolo dei rendimenti del portafoglio
portfolio_returns <-apply(simulated_returns, c(1,2), function(x) sum(x*multiasset_quotes))

# Calcolo del valore del portafoglio
portfolio_values <- matrix(NA, nrow = n_sim, ncol = n_period)
portfolio_values[, 1] <- initial_value * (1 + portfolio_returns[, 1])

for (t in 2:n_period) {
  portfolio_values[, t] <- portfolio_values[, t - 1] * (1 + portfolio_returns[, t])
}

# Analisi finale
final_values <- portfolio_values[, n_period]
mean_value <- mean(final_values)
expected_return <- mean_value / initial_value - 1
sd_value <- sd(final_values)
quantiles <- quantile(final_values, probs = c(0.05, 0.95))

# Output
cat("Valore medio simulato:", round(mean_value, 2), "\n")
cat("Simulated expected return after", n_period, "months:", round(expected_return*100, 2), "%", "\n")

cat("Deviazione standard del valore finale:", round(sd_value, 2), "\n")
cat("VaR 95% (percentile 5%):", round(quantiles[1], 2), "\n")
cat("Intervallo di confidenza 95%:", round(quantiles, 2), "\n")

# Plot
hist(final_values, breaks = 50, main = "Distribuzione dei Valori Finali Simulati",
     xlab = "Valore Finale del Portafoglio", col = "lightblue", border = "white")
abline(v = quantiles[1], col = "red", lwd = 2, lty = 2)
abline(v = mean_value, col = "blue", lwd = 2)

# forecast
final_returns <- portfolio_returns[, n_period]
VaR_percent <- quantile(final_returns, probs = 0.05)
ES_percent <- mean(final_returns[final_returns <= VaR_percent])

# Conversione in percentuali
VaR_percent <- VaR_percent * 100
ES_percent <- ES_percent * 100

cat("VaR (percentuale):", round(VaR_percent, 2), "%\n")
cat("ES (percentuale):", round(ES_percent, 2), "%\n")

# Aggiunta delle date ai periodi

# Calcolo dei rendimenti storici ponderati del portafoglio
ret_pure <- multiasset_sheet_returns %>% arrange(Dates) %>% select(-Dates) %>% as.matrix()
historical_returns <- ret_pure %*% multiasset_quotes

# Creazione di un dataframe con dati storici
historical_data <- data.frame(Dates = multiasset_sheet_returns$Dates) %>% 
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

# Creazione di un dataframe con i dati delle previsioni
forecast_summary <- data.frame(
  Dates = seq.Date(from = max(historical_data$Dates) + 1, by = "month", length.out = n_period),
  Mean = apply(forecast_cumulative, 2, mean),
  P5 = apply(forecast_cumulative, 2, quantile, probs = 0.05),
  P95 = apply(forecast_cumulative, 2, quantile, probs = 0.95)
)

# Combiniamo i dati storici e le previsioni
combined_data <- rbind(
  data.frame(Dates = historical_data$Dates, 
             Mean = historical_data$Cumulative_Value, 
             P5 = NA, 
             P95 = NA),
  forecast_summary
  ) %>% 
  arrange(desc(Dates))

# Grafico del trend storico e previsioni future
ggplot(combined_data, aes(x = Dates)) +
  geom_line(aes(y = Mean), color = "blue", size = 1) +
  geom_ribbon(data = forecast_summary, aes(ymin = P5, ymax = P95), fill = "blue", alpha = 0.2) +
  labs(
    title = "Forecast of Portfolio Returns",
    subtitle = paste("VaR:", round(VaR_percent, 2), "% | ES:", round(ES_percent, 2), "%"),
    x = "Dates",
    y = "Portfolio Returns %"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12)
  )
