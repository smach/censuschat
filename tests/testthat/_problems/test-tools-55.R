# Extracted from test-tools.R:55

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "censuschat", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
tool <- tool_resolve_fips()
expect_s3_class(tool, "ToolDef")
expect_match(tool@description, "FIPS", ignore.case = TRUE)
arg_names <- names(tool@arguments)
expect_true("name" %in% arg_names)
