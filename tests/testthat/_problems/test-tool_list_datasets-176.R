# Extracted from test-tool_list_datasets.R:176

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "censuschat", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
mock_response <- function(req) {
    httr2::response_json(
      status_code = 200L,
      body = list(
        dataset = list(
          list(
            title = "Test Dataset",
            description = "Test description",
            c_dataset = c("test", "data"),
            c_vintage = 2023
          )
        )
      )
    )
  }
httr2::local_mocked_responses(mock_response)
tool <- tool_list_datasets()
result <- tool()
parsed <- jsonlite::fromJSON(result)
ds <- parsed$datasets[[1]]
expect_true("title" %in% names(ds))
expect_true("description" %in% names(ds))
