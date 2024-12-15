#' data preparation
setwd("C:/Users/danie/OneDrive/GitHub/myportfolio")

source("library.R")
source("functions.R")

output_path <- "C:/Users/danie/OneDrive/GitHub/myportfolio/output/"

# open file with assets
ticker_df <- read_xlsx("C:/Users/danie/OneDrive/GitHub/myportfolio/input/assets.xlsx"
                          , sheet = "ticker"
                          , col_names = T
                          , col_types = "text")
ticker_assets <- ticker_df$Ticker
index_assets <- ticker_df$Index

# download prices from Yahoo Finance
tsPrices <- map(ticker_assets, get_prices)
names(tsPrices) <- index_assets
price_matrix <- organize_data(tsPrices)
sheet_prices <- final_prices(price_matrix)

# compute returns
sheet_returns <- get_returns(sheet_prices, tsPrices)
write_parquet(sheet_returns, paste0(output_path, "sheet_returns.parquet"))


# arrange prices otherwise I couldn't compute returns
sheet_prices <- sheet_prices %>% arrange(desc(Dates))
write_parquet(sheet_prices, paste0(output_path, "sheet_prices.parquet"))

# save in output path
wb <- createWorkbook()

addWorksheet(wb, "Prices")
writeData(wb, "Prices", sheet_prices)

addWorksheet(wb, "Returns")
writeData(wb, "Returns", sheet_returns)

saveWorkbook(wb, paste0(output_path, "data_assets.xlsx"), overwrite = T)

# analysis
ret_pure <- sheet_returns %>% select(-Dates)
var_cov <- cov(ret_pure)
corr_matrix <- cor(ret_pure)
avg_returns <- ret_pure %>% summarise(across(everything(), mean))

# define quotes of each asset
quotes <- c(0.22,0.16,0.17,0.1, 0.15,0.08,0.05,0.07)
sum(quotes) # = 1

ptf_output <- risk_portfolio(quotes, var_cov, avg_returns, ticker_df)

write_parquet(ptf_output$Ptf_Analysis, paste0(output_path, "ptf_analysis.parquet"))
write_parquet(ptf_output$Ptf_Summary, paste0(output_path, "ptf_summary.parquet"))
