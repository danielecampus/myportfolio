setwd("C:/Users/danie/OneDrive/GitHub/myportfolio")

source("library.R")
source("functions.R")

input_path <- "C:/Users/danie/OneDrive/GitHub/myportfolio/input/"
output_path <- "C:/Users/danie/OneDrive/GitHub/myportfolio/output/"

# open inputs

dani_data <- list(
  horizon = 30,
  time_decay = 30/35, # horizon / ideal horizon (30 or 35 years)
  assets = c(
  "World Momentum",
  "US Quality",
  "World Health Care",
  "EU Gov bonds 7-10y",
  "EU Inflation-Linked",
  "US Treasury",
  "EU Overnight"),
  quotes = c(0.3,0.25,0.15, 0.1,0.05,0.05, 0.1)
)
sum(dani_data$quotes)
length(dani_data$assets)==length(dani_data$quotes)

###########################
# CHIARA
chiara_input <- list(
  name = "chiara_data",
  t = 30,
  assets = c(
  "World Momentum", # useful with time decay > 0.8
  "US Quality",
  "World Low Volatility",
  "EU Gov bonds 7-10y",
  "EU Inflation-Linked",
  "EU Overnight",
  "US Short Treasury",
  "ETC GOLD"),
  quotes = c(0.25,0.2,0.15, 0.1,0.05,0.1, 0.05, 0.1)
)
saveRDS(chiara_input, paste0(input_path, "chiara_input.rds"))

named_list(chiara_input$name, chiara_input$t, chiara_input$assets, chiara_input$quotes, input_path)

cat("Expected Return Chiara portfolio:", round(chiara_data$ptf_output$Ptf_Summary$Annual_Ret*100, 1), "%", "\n")
cat("VaR Chiara portfolio:", round(chiara_data$VaR$hVaR*100, 1), "%", "\n")
cat("ES Chiara portfolio:", round(chiara_data$ES$`-r_exceed/c_exceed`*100, 1), "%", "\n")

# ptf analysis
chiara_data$ptf_output <- risk_portfolio(chiara_data$quotes, chiara_data$var_cov, chiara_data$avg_returns, chiara_data$assets, chiara_data$returns)
chiara_data$ptf_output$Ptf_Summary$Annual_Ret

write_parquet(chiara_data$ptf_output$Ptf_Analysis, paste0(output_path, "chiara_analysis.parquet"))
write_parquet(chiara_data$ptf_output$Ptf_Summary, paste0(output_path, "chiara_summary.parquet"))

# plot
png(paste0(output_path, "gfx/chiara_plot_quotes.png"))
plot_quotes(chiara_data$ptf_output$Ptf_Analysis$Assets, chiara_data$ptf_output$Ptf_Analysis$Quotes)
dev.off()

png(paste0(output_path, "gfx/chiara_plot_returns.png"))
plot_returns(chiara_data$ptf_output$Ptf_Analysis$Assets, (chiara_data$ptf_output$Ptf_Analysis$Ret_Avg + 1)^12 - 1, (chiara_data$ptf_output$Ptf_Analysis$Ret_Weighted + 1)^12 - 1)
dev.off()

###############################
# ANNA

anna_input <- list(
  name = "anna_data",
  t = 30,
  assets = c(
    "MSCI World",
    "US Quality",
    "World Low Volatility",
    "EU Gov bonds 7-10y",
    "EU Inflation-Linked",
    "EU Overnight",
    "US Short Treasury",
    "ETC GOLD"
  ),
  quotes = c(0.30,0.15,0.10, 0.15, 0.05,0.1, 0.05, 0.1)
)
saveRDS(anna_input, paste0(input_path, "anna_input.rds"))

named_list(anna_input$name, anna_input$t, anna_input$assets, anna_input$quotes, input_path)

cat("Expected Return Anna portfolio:", round(anna_data$ptf_output$Ptf_Summary$Annual_Ret*100, 1), "%", "\n")
cat("VaR Anna:", round(anna_data$VaR$hVaR*100, 1), "%", "\n")
cat("ES Anna:", round(anna_data$ES$`-r_exceed/c_exceed`*100, 1), "%", "\n")

write_parquet(anna_data$ptf_output$Ptf_Analysis, paste0(output_path, "anna_analysis.parquet"))
write_parquet(anna_data$ptf_output$Ptf_Summary, paste0(output_path, "anna_summary.parquet"))

# plot
png(paste0(output_path, "gfx/anna_plot_quotes.png"))
plot_quotes(anna_data$ptf_output$Ptf_Analysis$Assets, anna_data$ptf_output$Ptf_Analysis$Quotes)
dev.off()

png(paste0(output_path, "gfx/anna_plot_returns.png"))
plot_returns(anna_data$ptf_output$Ptf_Analysis$Assets, (anna_data$ptf_output$Ptf_Analysis$Ret_Avg + 1)^12 - 1, (anna_data$ptf_output$Ptf_Analysis$Ret_Weighted + 1)^12 - 1)
dev.off()

#############################

#  sofi e fam

fam_input <- list(
  name = "fam_data",
  t = 10,
  assets = c(
    "MSCI World",
    "World Low Volatility",
    "EU Gov bonds 7-10y",
    "EU Inflation-Linked",
    "EU Overnight",
    "US Short Treasury",
    "ETC GOLD"
  ),
  quotes = c(0.25, 0.15, 0.22, 0.08, 0.12, 0.05, 0.13),
  initial_value = 11000
)
saveRDS(fam_input, paste0(input_path, "fam_input.rds"))

named_list(fam_input$name, fam_input$t, fam_input$assets, fam_input$quotes, input_path)

cat("Expected Return portfolio:", round(fam_data$ptf_output$Ptf_Summary$Annual_Ret*100, 1), "%", "\n")
cat("VaR:", round(fam_data$VaR$hVaR*100, 1), "%", "\n")
cat("ES:", round(fam_data$ES$`-r_exceed/c_exceed`*100, 1), "%", "\n")

write_parquet(fam_data$ptf_output$Ptf_Analysis, paste0(output_path, "fam_analysis.parquet"))
write_parquet(fam_data$ptf_output$Ptf_Summary, paste0(output_path, "fam_summary.parquet"))

# plot
png(paste0(output_path, "gfx/fam_plot_quotes.png"))
plot_quotes(fam_data$ptf_output$Ptf_Analysis$Assets, fam_data$ptf_output$Ptf_Analysis$Quotes)
dev.off()

png(paste0(output_path, "gfx/anna_plot_returns.png"))
plot_returns(fam_data$ptf_output$Ptf_Analysis$Assets, (fam_data$ptf_output$Ptf_Analysis$Ret_Avg + 1)^12 - 1, (fam_data$ptf_output$Ptf_Analysis$Ret_Weighted + 1)^12 - 1)
dev.off()

