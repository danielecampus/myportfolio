library(arrow)
library(dplyr)

prices <- read_parquet("input/data_prices.parquet")
cat("Date range:", as.character(min(prices$Dates)), 
    "to", as.character(max(prices$Dates)), "\n")
cat("Total months:", nrow(prices), "\n\n")

# Scarica le serie individuali e vedi chi inizia tardi
ticker_df <- read_parquet("input/data_ticker_df.parquet")
ts_prices  <- read_parquet("input/data_prices_universe.parquet")
cat("Months available per asset:\n")
print(sort(colSums(!is.na(ts_prices %>% select(-Dates)))))
