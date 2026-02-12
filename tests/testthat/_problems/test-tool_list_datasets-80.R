# Extracted from test-tool_list_datasets.R:80

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
            title = "American Community Survey 5-Year Data",
            description = "ACS 5-year estimates",
            c_dataset = c("acs", "acs5"),
            c_vintage = 2022
          ),
          list(
            title = "Decennial Census DHC",
            description = "Population counts",
            c_dataset = c("dec", "dhc"),
            c_vintage = 2020
          ),
          list(
            title = "American Community Survey 1-Year Data",
            description = "ACS 1-year estimates",
            c_dataset = c("acs", "acs1"),
            c_vintage = 2022
          )
        )
      )
    )
  }
httr2::local_mocked_responses(mock_response)
tool <- tool_list_datasets()
result <- tool(search_term = "ACS")
parsed <- jsonlite::fromJSON(result)
expect_equal(parsed$count, 2)
for (ds in parsed$datasets) {
    title_match <- grepl("ACS", ds$title, ignore.case = TRUE)
    desc_match <- grepl("ACS", ds$description, ignore.case = TRUE)
    expect_true(title_match || desc_match)
  }
