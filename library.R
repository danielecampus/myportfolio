
#install.packages(c(
#  "tidyverse", "purrr", "readr", "readxl", "openxlsx", "writexl",
#  "yfR", "rvest", "tidyquant", "quantmod",
#  "nloptr", "quadprog", "arrow",
#  "PerformanceAnalytics", "yaml",
#  "targets", "tarchetypes" "gridextra"
#))
# Load secrets from .env if present (gitignored)
local({
  env_file <- file.path(getwd(), ".env")
  if (file.exists(env_file)) {
    lines <- readLines(env_file, warn = FALSE)
    for (line in lines) {
      line <- trimws(line)
      if (nchar(line) == 0 || startsWith(line, "#")) next
      kv <- strsplit(line, "=", fixed = TRUE)[[1]]
      if (length(kv) >= 2) Sys.setenv(setNames(paste(kv[-1], collapse = "="), kv[1]))
    }
  }
})

library(tidyverse)
library(purrr)
library(readr)
library(readxl)
library(openxlsx)
library(writexl)
library(arrow)
library(yaml)

# Market data
library(jsonlite)
library(yfR)
library(rvest)
library(tidyquant)
library(quantmod)

# Optimization and risk
library(nloptr)
library(quadprog)
library(PerformanceAnalytics)

# Report layout
# install.packages("gridExtra")
library(gridExtra)
library(grid)
library(lubridate)

# Pipeline (loaded by _targets.R itself, but available for interactive use)
# library(targets)
# library(tarchetypes)