# Extracted from test-tool_fetch_data.R:164

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "censuschat", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
skip_if_not_installed("withr")
mock_data <- sample_acs_data(n = 10)
local_mocked_bindings(
    get_acs = function(...) mock_data,
    .package = "tidycensus"
  )
tool <- tool_fetch_data()
result <- tool(
    geography = "state",
    variables = c("B19013_001"),
    code_only = FALSE
  )
parsed <- jsonlite::fromJSON(result)
expect_equal(parsed$rows_returned, 10)
expect_equal(length(parsed$data), 10)
