# Extracted from test-tools.R:27

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "censuschat", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
tool <- tool_list_datasets()
expect_s3_class(tool, "ToolDef")
expect_match(tool@description, "Census", ignore.case = TRUE)
expect_match(tool@description, "dataset", ignore.case = TRUE)
expect_true("search_term" %in% names(tool@arguments))
