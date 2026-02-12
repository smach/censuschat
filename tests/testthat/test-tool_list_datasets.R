# Tests for tool_list_datasets()
# This tool fetches available Census datasets from the Census API

test_that("tool_list_datasets fetches and returns datasets", {
  # Mock the Census API response
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
            description = "Demographic and Housing Characteristics",
            c_dataset = c("dec", "dhc"),
            c_vintage = 2020
          )
        )
      )
    )
  }

  httr2::local_mocked_responses(mock_response)

  tool <- tool_list_datasets()
  result <- tool(search_term = NULL)

  parsed <- jsonlite::fromJSON(result)

  expect_equal(parsed$count, 2)
  expect_true("datasets" %in% names(parsed))
})

test_that("tool_list_datasets filters by search term", {
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

  # Should only return datasets matching "ACS"
  expect_equal(parsed$count, 2)

  # jsonlite simplifies datasets array to a data frame
  # All returned datasets should contain "ACS" in title or description
  for (i in seq_len(nrow(parsed$datasets))) {
    title_match <- grepl("ACS", parsed$datasets$title[i], ignore.case = TRUE)
    desc_match <- grepl("ACS", parsed$datasets$description[i], ignore.case = TRUE)
    expect_true(title_match || desc_match)
  }
})

test_that("tool_list_datasets is case-insensitive for search", {
  mock_response <- function(req) {
    httr2::response_json(
      status_code = 200L,
      body = list(
        dataset = list(
          list(
            title = "American Community Survey",
            description = "Survey data",
            c_dataset = c("acs", "acs5"),
            c_vintage = 2022
          )
        )
      )
    )
  }

  httr2::local_mocked_responses(mock_response)

  tool <- tool_list_datasets()

  # Search with lowercase
  result_lower <- tool(search_term = "community")
  parsed_lower <- jsonlite::fromJSON(result_lower)

  # Search with uppercase
  httr2::local_mocked_responses(mock_response)
  result_upper <- tool(search_term = "COMMUNITY")
  parsed_upper <- jsonlite::fromJSON(result_upper)

  expect_equal(parsed_lower$count, parsed_upper$count)
})

test_that("tool_list_datasets limits results to 30", {
  # Create more than 30 datasets
  many_datasets <- lapply(1:50, function(i) {
    list(
      title = paste("Dataset", i),
      description = paste("Description", i),
      c_dataset = paste0("ds", i),
      c_vintage = 2020
    )
  })

  mock_response <- function(req) {
    httr2::response_json(
      status_code = 200L,
      body = list(dataset = many_datasets)
    )
  }

  httr2::local_mocked_responses(mock_response)

  tool <- tool_list_datasets()
  result <- tool(search_term = NULL)

  parsed <- jsonlite::fromJSON(result)

  # Should be limited to 30
  expect_lte(parsed$count, 30)
  expect_lte(nrow(parsed$datasets), 30)  # Use nrow() since jsonlite returns data frame
})

test_that("tool_list_datasets returns dataset metadata", {
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

  # Check that expected fields are present (jsonlite returns data frame)
  expect_true("title" %in% names(parsed$datasets))
  expect_true("description" %in% names(parsed$datasets))
  expect_true("identifier" %in% names(parsed$datasets))
  expect_true("vintage" %in% names(parsed$datasets))
})

test_that("tool_list_datasets handles empty search results", {
  mock_response <- function(req) {
    httr2::response_json(
      status_code = 200L,
      body = list(
        dataset = list(
          list(
            title = "American Community Survey",
            description = "ACS data",
            c_dataset = "acs",
            c_vintage = 2022
          )
        )
      )
    )
  }

  httr2::local_mocked_responses(mock_response)

  tool <- tool_list_datasets()
  result <- tool(search_term = "NONEXISTENT_TERM_12345")

  parsed <- jsonlite::fromJSON(result)

  expect_equal(parsed$count, 0)
})

test_that("tool_list_datasets returns valid JSON", {
  mock_response <- function(req) {
    httr2::response_json(
      status_code = 200L,
      body = list(
        dataset = list(
          list(
            title = "Test",
            description = "Test",
            c_dataset = "test",
            c_vintage = 2022
          )
        )
      )
    )
  }

  httr2::local_mocked_responses(mock_response)

  tool <- tool_list_datasets()
  result <- tool()

  # Should not error when parsing
  expect_no_error(jsonlite::fromJSON(result))
})
