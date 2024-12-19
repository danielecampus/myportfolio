input_path <- "C:/Users/danie/OneDrive/GitHub/myportfolio/input/"
output_path <- "C:/Users/danie/OneDrive/GitHub/myportfolio/output/"

# open inputs
equity_ticker_df <- read_parquet(paste0(input_path, "equity_ticker_df.parquet"))
equity_sheet_prices <- read_parquet(paste0(input_path, "equity_sheet_prices.parquet"))
equity_sheet_returns <- read_parquet(paste0(input_path, "equity_sheet_returns.parquet"))

# analysis
ret_pure <- equity_sheet_returns %>% select(-Dates) %>% as.data.frame()
var_cov <- cov(ret_pure)
corr_matrix <- cor(ret_pure)
avg_returns <- ret_pure %>% summarise(across(everything(), mean)) 

# define quotes of each asset
equity_quotes <- c(0.35,0.4,0.25)
sum(equity_quotes) # = 1

# monte carlo inputs
n_sim <- 10000
n_period <- 24
initial_value <- 50000
avg_returns_vector <- avg_returns %>% as.numeric() %>% as.vector()

# simulation of returns
set.seed(123)
simulated_returns <- MASS::mvrnorm(
  n = n_sim * n_period,
  mu = avg_returns_vector,
  Sigma = var_cov
)

simulated_returns <- array(simulated_returns, dim =  c(n_sim, n_period, length(avg_returns_vector)))

portfolio_returns <-apply(simulated_returns, c(1,2), function(x) sum(x*equity_quotes))

# portfolio value
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

# Output
cat("Valore medio simulato:", round(mean_value, 2), "\n")
cat("Simulated expected return after", n_period, "months:", round(expected_return*100, 2), "%", "\n")

cat("Deviazione standard del valore finale:", round(sd_value, 2), "\n")
cat("VaR 95% (percentile 5%):", round(quantiles[1], 2), "\n")
cat("Intervallo di confidenza 95%:", round(quantiles, 2), "\n")

# # Plot
# hist(final_values, breaks = 50, main = "Distribuzione dei Valori Finali Simulati",
#      xlab = "Valore Finale del Portafoglio", col = "lightblue", border = "white")
# abline(v = quantiles[1], col = "red", lwd = 2, lty = 2)
# abline(v = mean_value, col = "blue", lwd = 2)

# forecast
final_returns <- portfolio_returns[, n_period]
VaR_percent <- quantile(final_returns, probs = 0.05)
ES_percent <- mean(final_returns[final_returns <= VaR_percent])

VaR_percent <- VaR_percent * 100
ES_percent <- ES_percent * 100

cat("VaR:", round(VaR_percent, 2), "%\n")
cat("ES:", round(ES_percent, 2), "%\n")


#### FORECAST OF RETURNS ####

# weighted returns
ret_pure <- equity_sheet_returns %>% arrange(Dates) %>% select(-Dates) %>% as.matrix()
historical_returns <- ret_pure %*% equity_quotes

historical_data <- data.frame(Dates = equity_sheet_returns$Dates) %>% 
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

equity_forecast_summary <- data.frame(
  Dates = seq.Date(from = max(historical_data$Dates) + 1, by = "month", length.out = n_period),
  Mean = apply(forecast_cumulative, 2, mean),
  P5 = apply(forecast_cumulative, 2, quantile, probs = 0.05),
  P95 = apply(forecast_cumulative, 2, quantile, probs = 0.95)
)

equity_combined_data <- rbind(
  data.frame(Dates = historical_data$Dates, 
             Mean = historical_data$Cumulative_Value, 
             P5 = NA, 
             P95 = NA),
  equity_forecast_summary
) %>% 
  arrange(desc(Dates))

write_parquet(equity_combined_data, paste0(output_path, "equity_mc_forecast.parquet"))

exp_ret <- max(equity_forecast_summary$Mean)/last(historical_data$Cumulative_Value)-1

# plot
png(paste0(output_path, "gfx/equity_forecast.png"))
ggplot(equity_combined_data, aes(x = Dates)) +
  geom_line(aes(y = Mean), color = "blue", size = 1) +
  geom_ribbon(data = forecast_summary, aes(ymin = P5, ymax = P95), fill = "blue", alpha = 0.2) +
  labs(
    title = "Forecast of the equity portfolio returns",
    subtitle = paste("t:", n_period, "months | E[R] =", round(exp_ret*100, 2), "% | VaR:", round(VaR_percent, 2), "% | ES:", round(ES_percent, 2), "%"),
    x = "Dates",
    y = "Portfolio Returns %"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12)
  )
dev.off()
