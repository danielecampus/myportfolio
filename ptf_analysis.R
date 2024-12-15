#' data preparation
setwd("C:/Users/danie/OneDrive/GitHub/myportfolio")
source("library.R")

output_path <- "C:/Users/danie/OneDrive/GitHub/myportfolio/output/"

walk("functions.R", source)

ticker_df <- read_xlsx("C:/Users/danie/OneDrive/GitHub/myportfolio/input/assets.xlsx"
                          , sheet = "ticker"
                          , col_names = T
                          , col_types = "text")
ticker_assets <- ticker_df$Ticker
index_assets <- ticker_df$Index

tsPrices <- map(ticker_assets, get_prices)
names(tsPrices) <- index_assets

price_matrix <- organize_data(tsPrices)

sheet_prices <- final_prices(price_matrix)

sheet_returns <- get_returns(sheet_prices, tsPrices)

sheet_prices <- sheet_prices %>% arrange(desc(Dates))

save_sheets(sheet_prices, sheet_returns, output_path)

# analysis
ret_pure <- sheet_returns %>% select(-Dates)
var_cov <- cov(ret_pure)
corr_matrix <- cor(ret_pure)
avg_returns <- ret_pure %>% summarise(across(everything(), mean))

quotes <- c(0.22,0.16,0.17,0.1, 0.15,0.08,0.05,0.07)
sum(quotes)

ptf_output <- risk_portfolio(quotes, var_cov, avg_returns, ticker_df)
