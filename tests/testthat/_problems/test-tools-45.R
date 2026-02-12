# Extracted from test-tools.R:45

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
expect_true("year" %in% arg_names)
expect_true("state" %in% arg_names)
expect_true("county" %in% arg_names)
expect_true("survey" %in% arg_names)
expect_true("decennial_sumfile" %in% arg_names)
expect_true("code_only" %in% arg_names)
