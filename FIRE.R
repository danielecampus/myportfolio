# FIRE inputs

# I want to retire by 60 yo and take out 50k per year as integrated pension
cash_out <- 50000
# consider Safe Withdrawal Rate (SWR) of 3.5% and 4%
FV_3.5 <- cash_out/0.035
FV_4 <- cash_out/0.04

# below the assumptions
PV <- 5000 # current wealth
CF <- 10000 # annual savings
t <- 30 # years

# FV function
future_value <- function(r, FV_target){
  ( PV * (1 + r)^t + CF * ((1 + r)^t - 1) / r ) - FV_target
}
calculate_rmn <- function(FV_target){
  r_function <- function(r){
    future_value(r, FV_target)
  }
  # solve for r_min
  r_min <- uniroot(r_function, c(0.05, 0.2))$root
}

r_min_3.5 <- calculate_rmn(FV_3.5)
r_min_4 <- calculate_rmn(FV_4)

cat(round(r_min_3.5*100,2), "%", "\n")
cat(round(r_min_4*100,2), "%", "\n")
