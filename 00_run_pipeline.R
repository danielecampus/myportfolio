# Note: changes to config.yaml are NOT auto-detected by targets because the
# config drives the pipeline structure, not just data. After editing config,
# re-run tar_make() ---   targets will rebuild what depends on the changed values.

library(targets)

# Clear previous run if you want a clean rebuild
# tar_destroy(ask = FALSE)

tar_make()

# After the run, inspect a few results
cat("\n== Pipeline outputs ==\n")
cat("Targets built:\n")
print(tar_progress())

cat("\nSummary (chiara):\n")
print(tar_read(ptf_chiara)$ptf_output$Ptf_Summary)

cat("\nMC summary (chiara):\n")
print(tar_read(mc_chiara)$forecast_summary)
