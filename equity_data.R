#' data preparation
setwd("C:/Users/danie/OneDrive/GitHub/myportfolio")

source("library.R")
source("functions.R")

input_path <- "C:/Users/danie/OneDrive/GitHub/myportfolio/input/"

# open file with assets
ticker_df <- read_xlsx(paste0(input_path, "equity.xlsx")
                       , sheet = "ticker"
                       , col_names = T
                       , col_types = "text")
ticker_assets <- ticker_df$Ticker
index_assets <- ticker_df$Index
write_parquet(ticker_df, paste0(input_path, "equity_ticker_df.parquet"))

# download prices from Yahoo Finance
tsPrices <- map(ticker_assets, get_prices)
names(tsPrices) <- index_assets
price_matrix <- organize_data(tsPrices)
sheet_prices <- final_prices(price_matrix)

# compute returns
sheet_returns <- get_returns(sheet_prices, tsPrices)
write_parquet(sheet_returns, paste0(input_path, "equity_sheet_returns.parquet"))


# arrange prices otherwise I couldn't compute returns
sheet_prices <- sheet_prices %>% arrange(desc(Dates))
write_parquet(sheet_prices, paste0(input_path, "equity_sheet_prices.parquet"))

# save in output path
wb <- createWorkbook()

addWorksheet(wb, "Prices")
writeData(wb, "Prices", sheet_prices)

addWorksheet(wb, "Returns")
writeData(wb, "Returns", sheet_returns)

saveWorkbook(wb, paste0(input_path, "equity_ret.xlsx"), overwrite = T)
