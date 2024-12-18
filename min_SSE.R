
target_return <- 0.085  # Rendimento minimo desiderato
upper_bounds <- c(0.4, 0.4, 0.4, 0.4, 0.2, 0.3, 0.3, 0.3)  # Limite superiore

# Risultati
opt_quotes <- portfolio_optimization(var_cov, avg_returns, target_return, upper_bounds)
optimized_weights <- opt_quotes$solution
final_return <- sum(optimized_weights * avg_returns)
final_risk <- sqrt(t(optimized_weights) %*% var_cov %*% optimized_weights)

# Stampa risultati
cat("Optimal returns:\n", paste0(round(optimized_weights*100, 1), "%"), "\n")
cat("Expected return:", round(final_return*100,1), "%", "\n")
cat("Standard deviation:", round(final_risk*100,1),"%", "\n")
cat("Min SSE:", result$objective, "\n")
