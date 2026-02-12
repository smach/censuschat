# Extracted from test-tools.R:15

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "censuschat", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
tools <- census_tools()
for (i in seq_along(tools)) {
    expect_s3_class(tools[[i]], "ToolDef")
  }
