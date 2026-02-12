# Extracted from test-tools.R:37

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "censuschat", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
tool <- tool_fetch_data()
expect_s3_class(tool, "ToolDef")
arg_names <- names(tool@arguments)
expect_true("geography" %in% arg_names)
expect_true("variables" %in% arg_names)
