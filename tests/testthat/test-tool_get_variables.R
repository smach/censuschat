# Tests for tool_get_variables()
# This tool returns static Census variable code references

test_that("tool_get_variables returns all ACS categories when no filter", {
  tool <- tool_get_variables()
  result <- tool(dataset = "acs")

  parsed <- jsonlite::fromJSON(result)

  expect_true("population" %in% names(parsed))
  expect_true("income" %in% names(parsed))
  expect_true("housing" %in% names(parsed))
  expect_true("education" %in% names(parsed))
  expect_true("race" %in% names(parsed))
})

test_that("tool_get_variables returns all decennial categories when no filter", {
  tool <- tool_get_variables()
  result <- tool(dataset = "decennial")

  parsed <- jsonlite::fromJSON(result)

  expect_true("population" %in% names(parsed))
  expect_true("housing" %in% names(parsed))
  expect_true("race" %in% names(parsed))
})

test_that("tool_get_variables filters by category for ACS", {
  tool <- tool_get_variables()
  result <- tool(category = "income", dataset = "acs")

  parsed <- jsonlite::fromJSON(result)

  # jsonlite simplifies the JSON array of objects to a data frame
  expect_true(is.data.frame(parsed))

  # Check for expected income variables
  expect_true("B19013_001" %in% parsed$code)  # Median Household Income
  expect_true("B19301_001" %in% parsed$code)  # Per Capita Income
})

test_that("tool_get_variables filters by category for decennial", {
  tool <- tool_get_variables()
  result <- tool(category = "population", dataset = "decennial")

  parsed <- jsonlite::fromJSON(result)

  # jsonlite simplifies the JSON array of objects to a data frame
  expect_true(is.data.frame(parsed))

  # Check for expected decennial population variables
  expect_true("P1_001N" %in% parsed$code)  # Total Population (2020 DHC)
})

test_that("tool_get_variables includes variable descriptions", {
  tool <- tool_get_variables()
  result <- tool(category = "income", dataset = "acs")

  parsed <- jsonlite::fromJSON(result)

  # jsonlite returns a data frame with code and name columns
  expect_true("code" %in% names(parsed))
  expect_true("name" %in% names(parsed))
  expect_type(parsed$code, "character")
  expect_type(parsed$name, "character")
  expect_gt(nrow(parsed), 0)  # Should have at least one variable
})

test_that("tool_get_variables defaults to ACS dataset", {
  tool <- tool_get_variables()

  # Default dataset should be "acs"
  result_default <- tool()
  result_acs <- tool(dataset = "acs")

  # Both should return the same structure
  parsed_default <- jsonlite::fromJSON(result_default)
  parsed_acs <- jsonlite::fromJSON(result_acs)

  expect_equal(names(parsed_default), names(parsed_acs))
})

# Snapshot tests for consistent output
test_that("tool_get_variables ACS income snapshot", {
  tool <- tool_get_variables()
  result <- tool(category = "income", dataset = "acs")

  expect_snapshot(cat(result))
})

test_that("tool_get_variables decennial population snapshot", {
  tool <- tool_get_variables()
  result <- tool(category = "population", dataset = "decennial")

  expect_snapshot(cat(result))
})
