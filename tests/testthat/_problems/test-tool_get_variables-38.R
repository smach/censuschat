# Extracted from test-tool_get_variables.R:38

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "censuschat", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
tool <- tool_get_variables()
result <- tool(category = "income", dataset = "acs")
parsed <- jsonlite::fromJSON(result)
expect_true(is.list(parsed))
codes <- sapply(parsed, function(x) x$code)
