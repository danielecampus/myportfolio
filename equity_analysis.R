input_path <- "C:/Users/danie/OneDrive/GitHub/myportfolio/input/"
ouput_path <- "C:/Users/danie/OneDrive/GitHub/myportfolio/output/"

# open inputs
equity_ticker_df <- read_parquet(paste0(input_path, "equity_ticker_df.parquet"))
equity_sheet_prices <- read_parquet(paste0(input_path, "equity_sheet_prices.parquet"))
equity_sheet_returns <- read_parquet(paste0(input_path, "equity_sheet_returns.parquet"))

# analysis
ret_pure <- equity_sheet_returns %>% select(-Dates)
var_cov <- cov(ret_pure)
corr_matrix <- cor(ret_pure)
avg_returns <- ret_pure %>% summarise(across(everything(), mean))

# define quotes of each asset
equity_quotes <- c(0.35,0.4,0.25)
sum(equity_quotes) # = 1

# VaR - ES
equity_VaR <- VaR(equity_sheet_returns, p = 0.95, method = "historical", portfolio_method = "component", weights = equity_quotes)
equity_ES <- ES(equity_sheet_returns, p = 0.95, method = "historical", portfolio_method = "component", weights = equity_quotes)
cat("VaR equity portfolio:", round(equity_VaR$hVaR*100, 1), "%", "\n")
cat("ES equity portfolio:", round(equity_ES$`-r_exceed/c_exceed`*100, 1), "%", "\n")

#analysis
equity_ptf_output <- risk_portfolio(equity_quotes, var_cov, avg_returns, ticker_df)
equity_ptf_output$Ptf_Summary$Annual_Ret

write_parquet(equity_ptf_output$Ptf_Analysis, paste0(output_path, "equity_ptf_analysis.parquet"))
write_parquet(equity_ptf_output$Ptf_Summary, paste0(output_path, "equity_ptf_summary.parquet"))

# plot
png(paste0(output_path, "gfx/equity_plot_quotes.png"))
plot_quotes(equity_ptf_output$Ptf_Analysis$Assets, equity_ptf_output$Ptf_Analysis$Quotes)
dev.off()

png(paste0(output_path, "gfx/equity_plot_returns.png"))
plot_returns(equity_ptf_output$Ptf_Analysis$Assets, (equity_ptf_output$Ptf_Analysis$Ret_Avg + 1)^12 - 1, (equity_ptf_output$Ptf_Analysis$Ret_Weighted + 1)^12 - 1)
dev.off()

