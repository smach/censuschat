# Tests for tool_get_place_tracts()
# This tool generates code for getting tract data within a place (city/town)

test_that("tool_get_place_tracts generates valid R code", {
  tool <- tool_get_place_tracts()

  result <- tool(
    place_name = "Boston",
    state = "MA",
    variables = c("B19013_001")
  )

  parsed <- jsonlite::fromJSON(result)

  # Should include tidycensus code
  expect_true("tidycensus_code" %in% names(parsed))
  expect_type(parsed$tidycensus_code, "character")

  # Code should include required libraries
  expect_match(parsed$tidycensus_code, "library\\(tidycensus\\)")
  expect_match(parsed$tidycensus_code, "library\\(tigris\\)")
  expect_match(parsed$tidycensus_code, "library\\(sf\\)")
})

test_that("tool_get_place_tracts includes place name in code", {
  tool <- tool_get_place_tracts()

  result <- tool(
    place_name = "Framingham",
    state = "MA",
    variables = c("B19013_001")
  )

  parsed <- jsonlite::fromJSON(result)

  # Place name should appear in the filtering code

  expect_match(parsed$tidycensus_code, "Framingham")
})

test_that("tool_get_place_tracts includes state in code", {
  tool <- tool_get_place_tracts()

  result <- tool(
    place_name = "Boston",
    state = "Massachusetts",
    variables = c("B25077_001")
  )

  parsed <- jsonlite::fromJSON(result)

  # State should appear in the code
  expect_match(parsed$tidycensus_code, "Massachusetts")
})

test_that("tool_get_place_tracts includes variables in code", {
  tool <- tool_get_place_tracts()

  result <- tool(
    place_name = "Cambridge",
    state = "MA",
    variables = c("B19013_001", "B25077_001")
  )

  parsed <- jsonlite::fromJSON(result)

  # Both variables should appear
  expect_match(parsed$tidycensus_code, "B19013_001")
  expect_match(parsed$tidycensus_code, "B25077_001")
})

test_that("tool_get_place_tracts includes year when specified", {
  tool <- tool_get_place_tracts()

  result <- tool(
    place_name = "Boston",
    state = "MA",
    variables = c("B19013_001"),
    year = 2021
  )

  parsed <- jsonlite::fromJSON(result)

  # Year should appear in the code
  expect_match(parsed$tidycensus_code, "2021")
})

test_that("tool_get_place_tracts omits year when not specified", {
  tool <- tool_get_place_tracts()

  result <- tool(
    place_name = "Boston",
    state = "MA",
    variables = c("B19013_001")
  )

  parsed <- jsonlite::fromJSON(result)

  # Should not hardcode a year - let tidycensus use default
  # The code should NOT contain "year =" when year is not specified
  expect_false(grepl("year\\s*=\\s*\\d{4}", parsed$tidycensus_code))
})

test_that("tool_get_place_tracts includes survey parameter", {
  tool <- tool_get_place_tracts()

  result <- tool(
    place_name = "Boston",
    state = "MA",
    variables = c("B19013_001"),
    survey = "acs1"
  )

  parsed <- jsonlite::fromJSON(result)

  expect_match(parsed$tidycensus_code, "acs1")
})

test_that("tool_get_place_tracts lists required packages", {
  tool <- tool_get_place_tracts()

  result <- tool(
    place_name = "Boston",
    state = "MA",
    variables = c("B19013_001")
  )

  parsed <- jsonlite::fromJSON(result)

  expect_true("packages_needed" %in% names(parsed))
  expect_true("tidycensus" %in% parsed$packages_needed)
  expect_true("tigris" %in% parsed$packages_needed)
  expect_true("sf" %in% parsed$packages_needed)
})

test_that("tool_get_place_tracts includes install command", {
  tool <- tool_get_place_tracts()

  result <- tool(
    place_name = "Boston",
    state = "MA",
    variables = c("B19013_001")
  )

  parsed <- jsonlite::fromJSON(result)

  expect_true("install_command" %in% names(parsed))
  expect_match(parsed$install_command, "install.packages")
})

test_that("tool_get_place_tracts includes explanation", {
  tool <- tool_get_place_tracts()

  result <- tool(
    place_name = "Boston",
    state = "MA",
    variables = c("B19013_001")
  )

  parsed <- jsonlite::fromJSON(result)

  expect_true("explanation" %in% names(parsed))
  expect_type(parsed$explanation, "character")
})

# Snapshot test for consistent code generation
test_that("tool_get_place_tracts code generation snapshot", {
  tool <- tool_get_place_tracts()

  result <- tool(
    place_name = "Framingham",
    state = "MA",
    variables = c("B19013_001", "B25077_001"),
    year = 2022
  )

  expect_snapshot(cat(result))
})
