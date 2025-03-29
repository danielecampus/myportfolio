setwd("C:/Users/danie/OneDrive/GitHub/myportfolio")

source("library.R")
source("functions.R")

input_path <- "C:/Users/danie/OneDrive/GitHub/myportfolio/input/"
output_path <- "C:/Users/danie/OneDrive/GitHub/myportfolio/output/"

# monte carlo inputs
n_sim <- 10000
n_months <- 12
initial_value <- 10000

###############################
# CHIARA
chiara_input <- readRDS(paste0(input_path, "chiara_input.rds"))

chiara_simulation <- montecarlo_simulation(chiara_input$assets, chiara_input$quotes, n_sim, n_months, initial_value)

# Output
cat("Average simulated final value after", n_months, "months:", round(chiara_simulation$forecast_summary$Expected_Value, 2), "\n")
cat("Simulated expected return after", n_months, "months:", round(chiara_simulation$forecast_summary$Expected_Return*100, 2), "%", "\n")

cat("Standard deviation of the final value:", round(chiara_simulation$forecast_summary$Std_Dev, 2), "\n")
cat("VaR 95% (percentile 5%):", round(chiara_simulation$forecast_summary$Q_5, 2), "\n")
cat("Intervallo di confidenza 95%:", round(chiara_simulation$forecast_summary$Q_95, 2), "\n")

cat("VaR:", round(chiara_simulation$forecast_summary$VaR * 100, 2), "%\n")
cat("ES:", round(chiara_simulation$forecast_summary$ES * 100, 2), "%\n")

write.csv(chiara_simulation$forecast_summary, paste0(output_path, "chiara_mc_summary.csv"))
write_parquet(chiara_simulation$forecast_ts, paste0(output_path, "chiara_mc_forecast.parquet"))

# plot
png(paste0(output_path, "gfx/chiara_12m_forecast.png"))
plot_simulation(chiara_simulation$forecast_ts, chiara_simulation$forecast_summary, "Chiara", n_months)
dev.off()

#######################################

# ANNA
anna_input <- readRDS(paste0(input_path, "anna_input.rds"))

anna_simulation <- montecarlo_simulation(anna_input$assets, anna_input$quotes, n_sim, n_months, initial_value)

# Output
cat("Average simulated final value after", n_months, "months:", round(anna_simulation$forecast_summary$Expected_Value, 2), "\n")
cat("Simulated expected return after", n_months, "months:", round(anna_simulation$forecast_summary$Expected_Return*100, 2), "%", "\n")

cat("Standard deviation of the final value:", round(anna_simulation$forecast_summary$Std_Dev, 2), "\n")
cat("VaR 95% (percentile 5%):", round(anna_simulation$forecast_summary$Q_5, 2), "\n")
cat("Intervallo di confidenza 95%:", round(anna_simulation$forecast_summary$Q_95, 2), "\n")

cat("VaR:", round(anna_simulation$forecast_summary$VaR * 100, 2), "%\n")
cat("ES:", round(anna_simulation$forecast_summary$ES * 100, 2), "%\n")

write.csv(anna_simulation$forecast_summary, paste0(output_path, "anna_mc_summary.csv"))
write_parquet(anna_simulation$forecast_ts, paste0(output_path, "anna_mc_forecast.parquet"))

# plot
png(paste0(output_path, "gfx/anna_12m_forecast.png"))
plot_simulation(anna_simulation$forecast_ts, anna_simulation$forecast_summary, "Anna", n_months)

dev.off()

#######################################

# FAM
fam_input <- readRDS(paste0(input_path, "fam_input.rds"))

fam_simulation <- montecarlo_simulation(fam_input$assets, fam_input$quotes, n_sim, n_months, fam_input$initial_value)

# Output
cat("Average simulated final value after", n_months, "months:", round(fam_simulation$forecast_summary$Expected_Value, 2), "\n")
cat("Simulated expected return after", n_months, "months:", round(fam_simulation$forecast_summary$Expected_Return*100, 2), "%", "\n")

cat("Standard deviation of the final value:", round(fam_simulation$forecast_summary$Std_Dev, 2), "\n")
cat("VaR 95% (percentile 5%):", round(fam_simulation$forecast_summary$Q_5, 2), "\n")
cat("Intervallo di confidenza 95%:", round(fam_simulation$forecast_summary$Q_95, 2), "\n")

cat("VaR:", round(fam_simulation$forecast_summary$VaR * 100, 2), "%\n")
cat("ES:", round(fam_simulation$forecast_summary$ES * 100, 2), "%\n")

write.csv(fam_simulation$forecast_summary, paste0(output_path, "fam_mc_summary.csv"))
write_parquet(fam_simulation$forecast_ts, paste0(output_path, "fam_mc_forecast.parquet"))

# plot
png(paste0(output_path, "gfx/fam_12m_forecast.png"))
plot_simulation(fam_simulation$forecast_ts, fam_simulation$forecast_summary, "Sofi", n_months)
dev.off()

#######################################
