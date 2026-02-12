# Extracted from test-tools.R:62

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "censuschat", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
tool <- tool_get_variables()
expect_s3_class(tool, "ToolDef")
