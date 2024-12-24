setwd("C:/Users/danie/OneDrive/GitHub/myportfolio")

source("library.R")
source("functions.R")

input_path <- "C:/Users/danie/OneDrive/GitHub/myportfolio/input/"
output_path <- "C:/Users/danie/OneDrive/GitHub/myportfolio/output/"

# open inputs
multiasset_ticker_df <- read_parquet(paste0(input_path, "multiasset_ticker_df.parquet"))
multiasset_sheet_prices <- read_parquet(paste0(input_path, "multiasset_sheet_prices.parquet"))
multiasset_sheet_returns <- read_parquet(paste0(input_path, "multiasset_sheet_returns.parquet"))

# analysis
ret_pure <- multiasset_sheet_returns %>% select(-Dates) %>% as.data.frame()
var_cov <- cov(ret_pure)
corr_matrix <- cor(ret_pure)
avg_returns <- ret_pure %>% summarise(across(everything(), mean)) 

# define quotes of each asset
multiasset_quotes <- c(0.15,0.15,0.10, 0.22,0.06,0.15, 0.05, 0.12)
sum(multiasset_quotes) # = 1

# Var - ES
multiasset_VaR <- VaR(multiasset_sheet_returns, p = 0.95,  method = "historical", portfolio_method = "component", weights = multiasset_quotes) 
multiasset_ES <- ES(multiasset_sheet_returns, p = 0.95, method = "historical", portfolio_method = "component", weights = multiasset_quotes)
cat("VaR multiasset portfolio:", round(multiasset_VaR$hVaR*100, 1), "%", "\n")
cat("ES multiasset portfolio:", round(multiasset_ES$`-r_exceed/c_exceed`*100, 1), "%", "\n")

# ptf analysis
multiasset_ptf_output <- risk_portfolio(multiasset_quotes, var_cov, avg_returns, multiasset_ticker_df, multiasset_sheet_returns)
multiasset_ptf_output$Ptf_Summary$Annual_Ret

write_parquet(multiasset_ptf_output$Ptf_Analysis, paste0(output_path, "multiasset_analysis.parquet"))
write_parquet(multiasset_ptf_output$Ptf_Summary, paste0(output_path, "multiasset_summary.parquet"))

# plot
png(paste0(output_path, "gfx/multiasset_plot_quotes.png"))
plot_quotes(multiasset_ptf_output$Ptf_Analysis$Assets, multiasset_ptf_output$Ptf_Analysis$Quotes)
dev.off()

png(paste0(output_path, "gfx/multiasset_plot_returns.png"))
plot_returns(multiasset_ptf_output$Ptf_Analysis$Assets, (multiasset_ptf_output$Ptf_Analysis$Ret_Avg + 1)^12 - 1, (multiasset_ptf_output$Ptf_Analysis$Ret_Weighted + 1)^12 - 1)
dev.off()
