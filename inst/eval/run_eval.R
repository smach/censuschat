# Run censuschat evaluation with vitals
#
# Prerequisites:
#   - Census API key: census_api_key("your-key")
#   - Anthropic API key: Sys.setenv(ANTHROPIC_API_KEY = "your-key")
#   - vitals package: pak::pak("tidyverse/vitals")
#
# Usage:
#   devtools::load_all()
#   source("inst/eval/run_eval.R")

library(censuschat)
library(vitals)

# Run evaluation (logs saved to censuschat_eval_logs/ by default)
cat("Running censuschat evaluation...\n\n")
task <- census_eval(view = FALSE)

# Print results
cat("\n=== Results ===\n")
print(task$metrics)
cat("\nCost:\n")
print(task$get_cost())
cat("\nView details: vitals_view()\n")
