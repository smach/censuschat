# Extracted from test-tools.R:51

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "censuschat", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
tool <- tool_resolve_fips()
expect_s3_class(tool, "ToolDef")
