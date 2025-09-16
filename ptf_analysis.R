setwd("C:/Users/danie/OneDrive/GitHub/myportfolio")

source("library.R")
source("functions.R")

input_path <- "C:/Users/danie/OneDrive/GitHub/myportfolio/input/"
output_path <- "C:/Users/danie/OneDrive/GitHub/myportfolio/output/"

# open inputs
# 
# dc_input <- list(
#   name = "dc_data",
#   t = 30,
#   assets = c(
#   "World Momentum",
#   "World Quality",
#   "World Low Volatility",
#   # "EU Corp Large Cap bonds",
#   "EU Gov bonds 7-10y",
#   # "EU Overnight",
#   "US Short Treasury",
#   "ETC GOLD"
#   ),
#   quotes = c(0.4,0.2,0.1, 0.15,0.05, 0.1)
# )
# sum(dc_input$quotes)
# 
# saveRDS(dc_input, paste0(input_path, "dc_input.rds"))
# 
# named_list(dc_input$name, dc_input$t, dc_input$assets, dc_input$quotes, input_path)
# 
# cat("Expected Return dc portfolio:", round(dc_data$ptf_output$Ptf_Summary$Annual_Ret*100, 1), "%", "\n")
# cat("hVaR dc portfolio:", round(dc_data$VaR$hVaR*100, 1), "%", "\n")
# cat("ES dc portfolio:", round(dc_data$ES$`-r_exceed/c_exceed`*100, 1), "%", "\n")
# 
# write_parquet(dc_data$ptf_output$Ptf_Analysis, paste0(output_path, "dc_analysis.parquet"))
# write_parquet(dc_data$ptf_output$Ptf_Summary, paste0(output_path, "dc_summary.parquet"))
# 
# # plot
# png(paste0(output_path, "gfx/dc_plot_quotes.png"))
# plot_quotes(dc_data$ptf_output$Ptf_Analysis$Assets, dc_data$ptf_output$Ptf_Analysis$Quotes)
# dev.off()
# 
# png(paste0(output_path, "gfx/dc_plot_returns.png"))
# plot_returns(dc_data$ptf_output$Ptf_Analysis$Assets, (dc_data$ptf_output$Ptf_Analysis$Ret_Avg + 1)^12 - 1, (dc_data$ptf_output$Ptf_Analysis$Ret_Weighted + 1)^12 - 1)
# dev.off()


###########################

# CHIARA
#' IL not strictly necessary if the focus is on equities. 
#' In this case we must choose the key protective assets only, 
#' excluding the ones with higher expected return since it is already given by the equities.

chiara_input <- list(
  name = "chiara_data",
  t = 30,
  assets = c(
  "World Momentum", 
  "World Low Volatility",
  "EU Gov bonds 7-10y",
  "EU Overnight",
  "US Short Treasury",
  "ETC GOLD"),
  quotes = c(0.4,0.2, 0.15,0.08, 0.05, 0.12)
)
sum(chiara_input$quotes)
saveRDS(chiara_input, paste0(input_path, "chiara_input.rds"))

named_list(chiara_input$name, chiara_input$t, chiara_input$assets, chiara_input$quotes, input_path)

cat("Expected Return Chiara portfolio:", round(chiara_data$ptf_output$Ptf_Summary$Annual_Ret*100, 1), "%", "\n")
cat("Sharpe Ratio:", round(chiara_data$ptf_output$Ptf_Summary$Sharpe_ratio, 2),"\n")
cat("VaR Chiara portfolio:", round(chiara_data$VaR$hVaR*100, 1), "%", "\n")
cat("ES Chiara portfolio:", round(chiara_data$ES$`-r_exceed/c_exceed`*100, 1), "%", "\n")

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
    "World Quality",
    # "World Low Volatility",
    "EU Gov bonds 7-10y",
    # "EU Inflation-Linked",
    "EU Overnight",
    "US Short Treasury",
    "ETC GOLD"
  ),
  quotes = c(0.4,0.15, 0.18,0.1,0.05, 0.12)
)
sum(anna_input$quotes)
saveRDS(anna_input, paste0(input_path, "anna_input.rds"))

named_list(anna_input$name, anna_input$t, anna_input$assets, anna_input$quotes, input_path)

cat("Expected Return Anna portfolio:", round(anna_data$ptf_output$Ptf_Summary$Annual_Ret*100, 1), "%", "\n")
cat("Sharpe Ratio:", round(anna_data$ptf_output$Ptf_Summary$Sharpe_ratio, 2),"\n")
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

#  sofi e gt

gts_input <- list(
  name = "gts_data",
  t = 10,
  assets = c(
    "MSCI World",
    "World Quality",
    "EU Gov bonds 7-10y",
    "EU Inflation-Linked",
    "EU Overnight",
    "US Short Treasury",
    "ETC GOLD"
  ),
  quotes = c(0.35, 0.1, 0.15, 0.1, 0.1, 0.05, 0.15),
  initial_value = 25000
)
sum(gts_input$quotes)

saveRDS(gts_input, paste0(input_path, "gts_input.rds"))

named_list(gts_input$name, gts_input$t, gts_input$assets, gts_input$quotes, input_path)

cat("Expected Return portfolio:", round(gts_data$ptf_output$Ptf_Summary$Annual_Ret*100, 1), "%", "\n")
cat("Sharpe Ratio:", round(gts_data$ptf_output$Ptf_Summary$Sharpe_ratio, 2),"\n")
cat("VaR:", round(gts_data$VaR$hVaR*100, 1), "%", "\n")
cat("ES:", round(gts_data$ES$`-r_exceed/c_exceed`*100, 1), "%", "\n")

write_parquet(gts_data$ptf_output$Ptf_Analysis, paste0(output_path, "gts_analysis.parquet"))
write_parquet(gts_data$ptf_output$Ptf_Summary, paste0(output_path, "gts_summary.parquet"))

# plot
png(paste0(output_path, "gfx/gts_plot_quotes.png"))
plot_quotes(gts_data$ptf_output$Ptf_Analysis$Assets, gts_data$ptf_output$Ptf_Analysis$Quotes)
dev.off()

png(paste0(output_path, "gfx/gts_plot_returns.png"))
plot_returns(gts_data$ptf_output$Ptf_Analysis$Assets, (gts_data$ptf_output$Ptf_Analysis$Ret_Avg + 1)^12 - 1, (gts_data$ptf_output$Ptf_Analysis$Ret_Weighted + 1)^12 - 1)
dev.off()

