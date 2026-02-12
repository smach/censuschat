# Extracted from test-tool_get_variables.R:64

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "censuschat", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
tool <- tool_get_variables()
result <- tool(category = "income", dataset = "acs")
parsed <- jsonlite::fromJSON(result)
for (var in parsed) {
    expect_true("code" %in% names(var))
    expect_true("name" %in% names(var))
    expect_type(var$code, "character")
    expect_type(var$name, "character")
  }
