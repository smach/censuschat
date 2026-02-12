# Extracted from test-tools.R:66

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "censuschat", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
tool <- tool_get_variables()
expect_s3_class(tool, "ToolDef")
expect_match(tool@description, "variable", ignore.case = TRUE)
arg_names <- names(tool@arguments)
expect_true("category" %in% arg_names)
