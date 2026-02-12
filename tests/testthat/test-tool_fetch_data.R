# Tests for tool_fetch_data()
# This tool fetches Census data using tidycensus

# =============================================================================
# Code-only mode tests (no API calls needed)
# =============================================================================

test_that("tool_fetch_data returns code when code_only=TRUE", {
  tool <- tool_fetch_data()

  result <- tool(
    geography = "state",
    variables = c("B19013_001"),
    state = "CA",
    code_only = TRUE
  )

  parsed <- jsonlite::fromJSON(result)

  expect_equal(parsed$mode, "code_only")
  expect_true("tidycensus_code" %in% names(parsed))
  expect_match(parsed$tidycensus_code, "get_acs")
  expect_match(parsed$tidycensus_code, "B19013_001")
  expect_match(parsed$tidycensus_code, "CA")
})

test_that("tool_fetch_data generates ACS code correctly", {
  tool <- tool_fetch_data()

  result <- tool(
    geography = "county",
    variables = c("B25077_001"),
    state = "MA",
    survey = "acs5",
    code_only = TRUE
  )

  parsed <- jsonlite::fromJSON(result)

  expect_match(parsed$tidycensus_code, "get_acs")
  expect_match(parsed$tidycensus_code, 'geography = "county"')
  expect_match(parsed$tidycensus_code, "B25077_001")
  expect_match(parsed$tidycensus_code, 'survey = "acs5"')
})

test_that("tool_fetch_data generates decennial code correctly", {
  tool <- tool_fetch_data()

  result <- tool(
    geography = "state",
    variables = c("P1_001N"),
    year = 2020,
    decennial_sumfile = "dhc",
    code_only = TRUE
  )

  parsed <- jsonlite::fromJSON(result)

  expect_match(parsed$tidycensus_code, "get_decennial")
  expect_match(parsed$tidycensus_code, "P1_001N")
  expect_match(parsed$tidycensus_code, 'sumfile = "dhc"')
  expect_match(parsed$tidycensus_code, "year = 2020")
})

test_that("tool_fetch_data includes year only when specified for ACS", {
  tool <- tool_fetch_data()

  # Without year
  result_no_year <- tool(
    geography = "state",
    variables = c("B19013_001"),
    code_only = TRUE
  )
  parsed_no_year <- jsonlite::fromJSON(result_no_year)

  # Should not contain year parameter
  expect_false(grepl("year\\s*=", parsed_no_year$tidycensus_code))

  # With year
  result_with_year <- tool(
    geography = "state",
    variables = c("B19013_001"),
    year = 2021,
    code_only = TRUE
  )
  parsed_with_year <- jsonlite::fromJSON(result_with_year)

  # Should contain year parameter
  expect_match(parsed_with_year$tidycensus_code, "year = 2021")
})

test_that("tool_fetch_data handles multiple variables", {
  tool <- tool_fetch_data()

  result <- tool(
    geography = "state",
    variables = c("B19013_001", "B25077_001", "B01003_001"),
    code_only = TRUE
  )

  parsed <- jsonlite::fromJSON(result)

  expect_match(parsed$tidycensus_code, "B19013_001")
  expect_match(parsed$tidycensus_code, "B25077_001")
  expect_match(parsed$tidycensus_code, "B01003_001")
})

# =============================================================================
# Data fetching mode tests (requires mocking tidycensus)
# =============================================================================

test_that("tool_fetch_data fetches ACS data and returns results", {
  skip_if_not_installed("withr")

  # Create mock ACS data (small dataset)
  mock_acs_data <- data.frame(
    GEOID = c("06", "25"),
    NAME = c("California", "Massachusetts"),
    variable = c("B19013_001", "B19013_001"),
    estimate = c(80000, 85000),
    moe = c(500, 600),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    get_acs = function(...) mock_acs_data,
    .package = "tidycensus"
  )

  tool <- tool_fetch_data()
  result <- tool(
    geography = "state",
    variables = c("B19013_001"),
    code_only = FALSE
  )

  parsed <- jsonlite::fromJSON(result)

  expect_equal(parsed$rows_returned, 2)
  expect_true("data" %in% names(parsed))
  expect_true("tidycensus_code" %in% names(parsed))
})

test_that("tool_fetch_data returns full data for small results (<=15 rows)", {
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
  expect_equal(nrow(parsed$data), 10)  # Use nrow() since jsonlite returns data frame
  expect_false("preview" %in% names(parsed))
})

test_that("tool_fetch_data returns full data with note for medium results (15-60 rows)", {
  skip_if_not_installed("withr")

  mock_data <- sample_acs_data(n = 30)

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

  expect_equal(parsed$rows_returned, 30)
  expect_equal(nrow(parsed$data), 30)  # Use nrow() since jsonlite returns data frame
  expect_true("note" %in% names(parsed))
})

test_that("tool_fetch_data returns preview for large results (>60 rows)", {
  skip_if_not_installed("withr")

  mock_data <- sample_acs_data(n = 100)

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

  expect_equal(parsed$rows_returned, 100)
  expect_true("preview" %in% names(parsed))
  expect_equal(nrow(parsed$preview), 10)  # Use nrow() since jsonlite returns data frame
  expect_true("recommendation" %in% names(parsed))
})

test_that("tool_fetch_data fetches decennial data", {
  skip_if_not_installed("withr")

  mock_decennial_data <- data.frame(
    GEOID = c("06", "25"),
    NAME = c("California", "Massachusetts"),
    variable = c("P1_001N", "P1_001N"),
    value = c(39500000, 7000000),
    stringsAsFactors = FALSE
  )

  local_mocked_bindings(
    get_decennial = function(...) mock_decennial_data,
    .package = "tidycensus"
  )

  tool <- tool_fetch_data()
  result <- tool(
    geography = "state",
    variables = c("P1_001N"),
    year = 2020,
    decennial_sumfile = "dhc",
    code_only = FALSE
  )

  parsed <- jsonlite::fromJSON(result)

  expect_equal(parsed$rows_returned, 2)
  expect_true("data" %in% names(parsed))
})

test_that("tool_fetch_data handles API errors gracefully", {
  skip_if_not_installed("withr")

  local_mocked_bindings(
    get_acs = function(...) stop("Census API error: Invalid API key"),
    .package = "tidycensus"
  )

  tool <- tool_fetch_data()
  result <- tool(
    geography = "state",
    variables = c("B19013_001"),
    code_only = FALSE
  )

  parsed <- jsonlite::fromJSON(result)

  expect_true("error" %in% names(parsed))
  expect_match(parsed$error, "error", ignore.case = TRUE)
  expect_true("tidycensus_code" %in% names(parsed))  # Still includes code
  expect_true("suggestion" %in% names(parsed))
})

test_that("tool_fetch_data passes state and county parameters", {
  skip_if_not_installed("withr")

  captured_args <- NULL

  local_mocked_bindings(
    get_acs = function(...) {
      captured_args <<- list(...)
      sample_acs_data(n = 5)
    },
    .package = "tidycensus"
  )

  tool <- tool_fetch_data()
  tool(
    geography = "county",
    variables = c("B19013_001"),
    state = "MA",
    county = "Middlesex",
    code_only = FALSE
  )

  expect_equal(captured_args$state, "MA")
  expect_equal(captured_args$county, "Middlesex")
})

test_that("tool_fetch_data always includes tidycensus code in response", {
  skip_if_not_installed("withr")

  mock_data <- sample_acs_data(n = 5)

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

  expect_true("tidycensus_code" %in% names(parsed))
  expect_match(parsed$tidycensus_code, "get_acs")
})
