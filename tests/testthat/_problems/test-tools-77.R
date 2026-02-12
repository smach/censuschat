# Extracted from test-tools.R:77

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "censuschat", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
tool <- tool_get_place_tracts()
expect_s3_class(tool, "ToolDef")
expect_match(tool@description, "tract", ignore.case = TRUE)
arg_names <- names(tool@arguments)
expect_true("place_name" %in% arg_names)
