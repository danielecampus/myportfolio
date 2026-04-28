
#install.packages(c(
#  "tidyverse", "purrr", "readr", "readxl", "openxlsx", "writexl",
#  "yfR", "rvest", "tidyquant", "quantmod",
#  "nloptr", "quadprog", "arrow",
#  "PerformanceAnalytics", "yaml",
#  "targets", "tarchetypes"
#))
library(tidyverse)
library(purrr)
library(readr)
library(readxl)
library(openxlsx)
library(writexl)
library(arrow)
library(yaml)

# Market data
library(yfR)
library(rvest)
library(tidyquant)
library(quantmod)

# Optimization and risk
library(nloptr)
library(quadprog)
library(PerformanceAnalytics)

# Pipeline (loaded by _targets.R itself, but available for interactive use)
# library(targets)
# library(tarchetypes)