setwd("C:/Users/danie/OneDrive/GitHub/myportfolio")

source("library.R")
source("functions.R")

input_path <- "C:/Users/danie/OneDrive/GitHub/myportfolio/input/"
output_path <- "C:/Users/danie/OneDrive/GitHub/myportfolio/output/"

# monte carlo inputs
n_sim <- 10000
n_period <- 12
initial_value <- 10000

###############################
# CHIARA
chiara_input <- readRDS(paste0(input_path, "chiara_input.rds"))

chiara_simulation <- montecarlo_simulation(chiara_input$assets, chiara_input$quotes, n_sim, n_months, initial_value)

# Output
cat("Average simulated final value after", n_period, "months:", round(chiara_simulation$forecast_summary$Expected_Value, 2), "\n")
cat("Simulated expected return after", n_period, "months:", round(chiara_simulation$forecast_summary$Expected_Return*100, 2), "%", "\n")

cat("Standard deviation of the final value:", round(chiara_simulation$forecast_summary$Std_Dev, 2), "\n")
cat("VaR 95% (percentile 5%):", round(chiara_simulation$forecast_summary$Q_5, 2), "\n")
cat("Intervallo di confidenza 95%:", round(chiara_simulation$forecast_summary$Q_95, 2), "\n")

cat("VaR:", round(chiara_simulation$forecast_summary$VaR * 100, 2), "%\n")
cat("ES:", round(chiara_simulation$forecast_summary$ES * 100, 2), "%\n")


write_parquet(chiara_simulation$forecast_ts, paste0(output_path, "chiara_mc_forecast.parquet"))

# plot
png(paste0(output_path, "gfx/chiara_12m_forecast.png"))
ggplot(chiara_simulation$forecast_ts, aes(x = chiara_simulation$forecast_ts$Dates)) +
  geom_line(aes(y = Mean), color = "blue", size = 1) +
  geom_ribbon(data = chiara_simulation$forecast_ts, aes(ymin = chiara_simulation$forecast_ts$P5, ymax = chiara_simulation$forecast_ts$P95), fill = "blue", alpha = 0.2) +
  labs(
    title = "Forecast of the multiasset portfolio returns",
    subtitle = paste("t:", n_months, 
                     "months | E[R] =", round(chiara_simulation$forecast_summary$Expected_Return*100, 2), 
                     "% | VaR:", round(chiara_simulation$forecast_summary$VaR*100, 2), 
                     "% | ES:", round(chiara_simulation$forecast_summary$ES*100, 2), "%"),
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

#######################################
